#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

#include "gc.h"

#define LOW_TAGS							\
  X(FIXNUM, 0)                                                                 \
  X(FLONUM1, 1)                                                                    \
  X(PTR, 2)                                                                 \
  X(CONS, 3)                                                                   \
  X(FLONUM2, 4)                                                                \
  X(FLONUM3, 5)                                                                \
  X(LITERAL, 6)                                                                 \
  X(VECTOR, 7)

#define PTR_TAGS                                                               \
  X(STRING, 0x2)                                                              \
  X(RECORD, 0xa)                                                               \
  X(CLOSURE, 0x12)							\
  X(SYMBOL, 0x1a)							\
  X(CONT, 0x22)

#define IMMEDIATE_TAGS                                                         \
  X(BOOL, 0x6)                                                                 \
  X(CHAR, 0x0e)                                                                \
  X(NIL, 0x16)                                                                 \
  X(EOF, 0x1e)                                                                 \
  X(UNDEFINED, 0x26)

extern char *low_tag_names[];
extern char *ptr_tag_names[];
extern char *immediate_tag_names[];

enum : uint8_t {
#define X(name, num) name##_TAG = (num),
  LOW_TAGS IMMEDIATE_TAGS PTR_TAGS
#undef X
      TAG_MASK = 0x7,
  IMMEDIATE_MASK = 0xff,
};

#define NIL                                                                    \
  (gc_obj) { .value = NIL_TAG }
#define UNDEFINED                                                              \
  (gc_obj) { .value = UNDEFINED_TAG }
#define TRUE_REP (gc_obj){.value = 0x0106}
#define FALSE_REP (gc_obj){.value = 0x0006}
#define EOF_OBJ                                                                \
  (gc_obj) { .value = EOF_TAG }

typedef struct {
  int64_t value;
} gc_obj;

typedef struct flonum_s {
  uint64_t type;
  double x;
} flonum_s;

typedef struct string_s {
  uint64_t type;
  gc_obj len;
  char str[];
} string_s;

typedef struct symbol {
  uint64_t type;
  gc_obj name; // string_s PTR_TAG'd value
  gc_obj val;
} symbol;

typedef struct vector_s {
  gc_obj len;
  gc_obj v[];
} vector_s;

typedef struct cons_s {
  gc_obj a;
  gc_obj b;
} cons_s;

typedef struct closure_s {
  uint64_t type;
  // v[0] is always the closure ptr.
  gc_obj v[];
} closure_s;

// This one is not PTR, but anything!
void *to_raw_ptr(gc_obj obj) { return (void *)(obj.value & ~TAG_MASK); }
string_s *to_string(gc_obj obj) { return (string_s *)(obj.value - PTR_TAG); }
symbol *to_symbol(gc_obj obj) { return (symbol *)(obj.value - PTR_TAG); }
int64_t to_fixnum(gc_obj obj) { return obj.value >> 3; }
cons_s *to_cons(gc_obj obj) { return (cons_s *)(obj.value - CONS_TAG); }
vector_s *to_vector(gc_obj obj) { return (vector_s *)(obj.value - VECTOR_TAG); }
closure_s *to_closure(gc_obj obj) { return (closure_s *)(obj.value - PTR_TAG); }
char to_char(gc_obj obj) { return (char)(obj.value >> 8); }

uint8_t get_tag(gc_obj obj) { return obj.value & TAG_MASK; }
uint8_t get_imm_tag(gc_obj obj) { return obj.value & IMMEDIATE_MASK; }
uint32_t get_ptr_tag(gc_obj obj) {
  return ((uint32_t *)(obj.value - PTR_TAG))[0];
}
bool is_char(gc_obj obj) { return get_imm_tag(obj) == CHAR_TAG; }
bool is_closure(gc_obj obj) { return get_tag(obj) == CLOSURE_TAG; }
bool is_cons(gc_obj obj) { return get_tag(obj) == CONS_TAG; }
bool is_ptr(gc_obj obj) { return get_tag(obj) == PTR_TAG; }
bool is_literal(gc_obj obj) { return get_tag(obj) == LITERAL_TAG; }
bool is_string(gc_obj obj) {
  return is_ptr(obj) && get_ptr_tag(obj) == STRING_TAG;
}
bool is_record(gc_obj obj) {
  return is_ptr(obj) && get_ptr_tag(obj) == RECORD_TAG;
}
bool is_undefined(gc_obj obj) { return get_imm_tag(obj) == UNDEFINED_TAG; }
bool is_vector(gc_obj obj) { return get_tag(obj) == VECTOR_TAG; }
bool is_symbol(gc_obj obj) { return get_tag(obj) == SYMBOL_TAG; }
bool is_fixnum(gc_obj obj) { return get_tag(obj) == FIXNUM_TAG; }
bool is_heap_object(gc_obj obj) { return !is_fixnum(obj) && !is_literal(obj); }
gc_obj tag_fixnum(int64_t num) {
  assert(((num << 3) >> 3) == num);
  return (gc_obj){.value = num << 3};
}
gc_obj tag_string(string_s *s) {
  return (gc_obj){.value = ((int64_t)s + PTR_TAG)};
}
gc_obj tag_cons(cons_s *s) {
  return (gc_obj){.value = ((int64_t)s + CONS_TAG)};
}
gc_obj tag_vector(vector_s *s) {
  return (gc_obj){.value = ((int64_t)s + VECTOR_TAG)};
}
gc_obj tag_cont(closure_s *s) {
  return (gc_obj){.value = ((int64_t)s + PTR_TAG)};
}
gc_obj tag_closure(closure_s *s) {
  return (gc_obj){.value = ((int64_t)s + PTR_TAG)};
}
gc_obj tag_char(char ch) {
  return (gc_obj){.value = (((int64_t)ch << 8) + CHAR_TAG)};
}

#define TAG_SET2 ((1 <<5)|(1 <<4)|(1 <<1))
static bool has_tag_5_or_4_or_1 ( int64_t n ) {
  // Note that unlike the paper, we need to explictly ensure n is
  // masked to 5 bits: shifting by more than 32 bits here is undefined
  // behavior, and clang will happily optimize everything out.
  return ((( uint32_t )1 << (n&0x1f) ) & (~( uint32_t )0/0xff * TAG_SET2 )) != 0;
}
bool is_flonum_fast(gc_obj obj) {
  return has_tag_5_or_4_or_1(obj.value);
}

bool double_to_gc(double d, gc_obj* res) {
  uint64_t di;
  memcpy(&di, &d, sizeof(d));
  di = __builtin_rotateleft64(di, 4);
  di++; // Offset by one, so that we keep fixnum as 0 tag.
  if (has_tag_5_or_4_or_1(di)) {
    *res = (gc_obj){.value = di};
    return true;
  }
  return false;
}

double to_double(gc_obj obj) {
  if (is_ptr(obj)) {
    abort();
  }
  assert(has_tag_5_or_4_or_1(obj.value));
  uint64_t r = obj.value - 1;
  r = __builtin_rotateright64(r, 4);
  double res;
  memcpy(&res, &r, sizeof(res));
  return res;
}

double to_double_fast(gc_obj obj) {
  uint64_t r = obj.value - 1;
  r = __builtin_rotateright64(r, 4);
  double res;
  memcpy(&res, &r, sizeof(res));
  return res;
}

gc_obj display(gc_obj obj);
void print_obj(gc_obj obj, FILE*) {
  display (obj);
}

gc_obj display(gc_obj obj) {
  auto tag = get_tag(obj);
  switch(tag) {
  case FIXNUM_TAG:
    printf("%li", to_fixnum(obj));
    break;
  case FLONUM1_TAG:
  case FLONUM2_TAG:
  case FLONUM3_TAG:
    char buffer[40];
    double d = to_double(obj);
    snprintf(buffer, 40 - 3, "%g", d);
    if (strpbrk(buffer, ".eE") == nullptr) {
      size_t len = strlen(buffer);
      buffer[len] = '.';
      buffer[len + 1] = '0';
      buffer[len + 2] = '\0';
    }
    printf("%s", buffer);
    break;
  case PTR_TAG: {
    auto ptr_tag = get_ptr_tag(obj);
    switch(ptr_tag) {
    case STRING_TAG: {
      auto str = to_string(obj);
      printf("%.*s", (int)to_fixnum(str->len), str->str);
      break;
    }
    case SYMBOL_TAG: {
      auto sym = to_symbol(obj);
      auto str = to_string(sym->name);
      printf("%.*s", (int)to_fixnum(str->len), str->str);
      break;
    }
    case RECORD_TAG: {
      printf("#<record>");
      break;
    }
    case CLOSURE_TAG: {
      printf("#<closure>");
      break;
    }
    case CONT_TAG: {
      printf("#<cont>");
      break;
    }
    default:
      printf("Unknown ptr tag: %i\n", ptr_tag);
      abort();
    }
    break;
  }
  case CONS_TAG: {
    auto file = stdout;
    auto c = to_cons(obj);
    fputc('(', file);
    while (is_cons(c->b)) {
      print_obj(c->a, file);
      c = to_cons(c->b);
      fputc(' ', file);
    }
    print_obj(c->a, file);
    if (c->b.value != NIL_TAG) {
      fputs(" . ", file);
      print_obj(c->b, file);
    }
    fputc(')', file);
    break;
  }
  case LITERAL_TAG: {
    auto lit_tag = get_imm_tag(obj);
    switch(lit_tag) {
    case CHAR_TAG: {
      printf("%c", to_char(obj));
      break;
    }
    case BOOL_TAG: {
      if (obj.value == TRUE_REP.value) {
	printf("#t");
      } else if (obj.value == FALSE_REP.value) {
	printf("#f");
      }
      break;
    }
    case NIL_TAG: {
      printf("'()");
      break;
    }
    case UNDEFINED_TAG: {
	printf("#<undef>");
	break;
    }
    default:
      printf("Unknown lit tag: %i\n", lit_tag);
      abort();
    }
    break;
  }
  case VECTOR_TAG: {
    auto v = to_vector(obj);
    printf("#(");
    for(uint64_t i = 0; i < to_fixnum(v->len); i++) {
      if (i != 0) {
	printf(" ");
      }
      display(v->v[i]);
    }
    printf(")");
    break;
  }
  default:
    printf("Unknown tag: %i\n", tag);
    abort();
  }
  return NIL;
}

#if 0
int main() {
  gc_obj res = double_to_gc(0.0);
  printf("Res was %lx, %f\n", res.value, to_double(res));
  res = double_to_gc(1.0);
  printf("Res was %lx, %f\n", res.value, to_double(res));
  res = double_to_gc(-1.0);
  printf("Res was %lx, %f\n", res.value, to_double(res));
  res = double_to_gc(11111111111111.11111111111111);
  printf("Res was %lx, %f\n", res.value, to_double(res));
  return 0;
}
#endif

////////////// MATH
#define likely(x) __builtin_expect(x, 1)
#define unlikely(x) __builtin_expect(x, 0)
#define NOINLINE __attribute__((noinline))
#define INLINE __attribute__((always_inline))

NOINLINE gc_obj SCM_ADD_SLOW(gc_obj a, gc_obj b) {
  double fa, fb;
  if (is_fixnum(a)) {
    fa = to_fixnum(a);
  } else {
    fa = to_double(a);
  }
  if (is_fixnum(b)) {
    fb = to_fixnum(b);
  } else {
    fb = to_double(b);
  }
  gc_obj res;
  if(double_to_gc(fa + fb, &res)) {
    return res;
  }
  abort();
}

INLINE gc_obj SCM_ADD(gc_obj a, gc_obj b) {
  if (likely((is_fixnum(a) & is_fixnum(b)) == 1)) {
    gc_obj res;
    if(likely(!__builtin_add_overflow(a.value, b.value, &res.value))) {
      return res;
    } else {
      [[clang::musttail]] return SCM_ADD_SLOW(a, b);
    }
  } else if (likely((is_flonum_fast(a) & is_flonum_fast(b)) == 1)) {
    gc_obj res;
    if(likely(double_to_gc(to_double_fast(a) + to_double_fast(b), &res))) {
      return res;
    } else {
      [[clang::musttail]] return SCM_ADD_SLOW(a, b);
    }
  } else {
    [[clang::musttail]] return SCM_ADD_SLOW(a, b);
  }
}

NOINLINE __attribute__((preserve_all)) gc_obj SCM_SUB_SLOW(gc_obj a, gc_obj b) {
  double fa, fb;
  if (is_fixnum(a)) {
    fa = to_fixnum(a);
  } else {
    fa = to_double(a);
  }
  if (is_fixnum(b)) {
    fb = to_fixnum(b);
  } else {
    fb = to_double(b);
  }
  gc_obj res;
  if(double_to_gc(fa - fb, &res)) {
    return res;
  }
  abort();
}

INLINE  gc_obj SCM_SUB(gc_obj a, gc_obj b) {
  if (likely((is_fixnum(a) & is_fixnum(b)) == 1)) {
    gc_obj res;
    if(likely(!__builtin_sub_overflow(a.value, b.value, &res.value))) {
      return res;
    } else {
      return SCM_SUB_SLOW(a, b);
    }
  } else if (likely((is_flonum_fast(a) & is_flonum_fast(b)) == 1)) {
    gc_obj res;
    if(likely(double_to_gc(to_double_fast(a) - to_double_fast(b), &res))) {
      return res;
    } else {
       return SCM_SUB_SLOW(a, b);
    }
  } else {
     return SCM_SUB_SLOW(a, b);
  }
}
// TODO check is_flonum

NOINLINE __attribute__((preserve_all)) gc_obj SCM_LT_SLOW(gc_obj a, gc_obj b) {
  double fa, fb;
  if (is_fixnum(a)) {
    fa = to_fixnum(a);
  } else {
    fa = to_double(a);
  }
  if (is_fixnum(b)) {
    fb = to_fixnum(b);
  } else {
    fb = to_double(b);
  }
  if (fa < fb) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

INLINE gc_obj SCM_LT(gc_obj a, gc_obj b) {
  if (likely((is_fixnum(a) & is_fixnum(b)) == 1)) {
    if(a.value < b.value) {
      return TRUE_REP;
    }
    return FALSE_REP;
  }
  if (likely((is_flonum_fast(a) & is_flonum_fast(b)) == 1)) {
    if(to_double_fast(a) < to_double_fast(b)) {
      return TRUE_REP;
    }
    return FALSE_REP;
  }
  return SCM_LT_SLOW(a, b);
}

NOINLINE gc_obj SCM_GT_SLOW(gc_obj a, gc_obj b) {
  double fa, fb;
  if (is_fixnum(a)) {
    fa = to_fixnum(a);
  } else {
    fa = to_double(a);
  }
  if (is_fixnum(b)) {
    fb = to_fixnum(b);
  } else {
    fb = to_double(b);
  }
  if (fa > fb) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

INLINE gc_obj SCM_GT(gc_obj a, gc_obj b) {
  if (likely((is_fixnum(a) & is_fixnum(b)) == 1)) {
    if(a.value > b.value) {
      return TRUE_REP;
    }
    return FALSE_REP;
  }
  if (likely((is_flonum_fast(a) & is_flonum_fast(b)) == 1)) {
    if(to_double_fast(a) > to_double_fast(b)) {
      return TRUE_REP;
    }
    return FALSE_REP;
  }
  [[clang::musttail]] return SCM_GT_SLOW(a, b);
}

NOINLINE gc_obj SCM_GTE_SLOW(gc_obj a, gc_obj b) {
  double fa, fb;
  if (is_fixnum(a)) {
    fa = to_fixnum(a);
  } else {
    fa = to_double(a);
  }
  if (is_fixnum(b)) {
    fb = to_fixnum(b);
  } else {
    fb = to_double(b);
  }
  if (fa >= fb) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

INLINE gc_obj SCM_GTE(gc_obj a, gc_obj b) {
  if (likely((is_fixnum(a) & is_fixnum(b)) == 1)) {
    if(a.value >= b.value) {
      return TRUE_REP;
    }
    return FALSE_REP;
  }
  if (likely((is_flonum_fast(a) & is_flonum_fast(b)) == 1)) {
    if(to_double_fast(a) >= to_double_fast(b)) {
      return TRUE_REP;
    }
    return FALSE_REP;
  }
  [[clang::musttail]] return SCM_GTE_SLOW(a, b);
}

NOINLINE gc_obj SCM_NUM_EQ_SLOW(gc_obj a, gc_obj b) {
  double fa, fb;
  if (is_fixnum(a)) {
    fa = to_fixnum(a);
  } else {
    fa = to_double(a);
  }
  if (is_fixnum(b)) {
    fb = to_fixnum(b);
  } else {
    fb = to_double(b);
  }
  if (fa == fb) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

INLINE gc_obj SCM_NUM_EQ(gc_obj a, gc_obj b) {
  if (likely((is_fixnum(a) & is_fixnum(b)) == 1)) {
    if(a.value == b.value) {
      return TRUE_REP;
    }
    return FALSE_REP;
  }
  if (likely((is_flonum_fast(a) & is_flonum_fast(b)) == 1)) {
    if(to_double_fast(a) == to_double_fast(b)) {
      return TRUE_REP;
    }
    return FALSE_REP;
  }
  [[clang::musttail]] return SCM_NUM_EQ_SLOW(a, b);
}

gc_obj car(gc_obj obj) {
  return to_cons(obj)->a;
}

gc_obj cdr(gc_obj obj) {
  return to_cons(obj)->b;
}

gc_obj setcar(gc_obj obj, gc_obj val) {
  to_cons(obj)->a = val;
  return UNDEFINED;
}

gc_obj setcdr(gc_obj obj, gc_obj val) {
  to_cons(obj)->b = val;
  return UNDEFINED;
}
gc_obj cons(gc_obj a, gc_obj b) {
  cons_s* c = rcimmix_alloc(sizeof(cons_s));
  c->a = a;
  c->b = b;
  return tag_cons(c);
}

gc_obj append(gc_obj a, gc_obj b) {
  if(is_cons(a)) {
    auto p = to_cons(a);
    return cons(p->a, append(p->b, b));
  }
  return b;
}

gc_obj SCM_GUARD(gc_obj a, int64_t type) {
  if (a.value == NIL.value) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

gc_obj make_vector(gc_obj obj) {
  vector_s* v = rcimmix_alloc(sizeof(vector_s) + to_fixnum(obj)*sizeof(gc_obj));
  v->len = obj;
  return tag_vector(v);
}

gc_obj vector_length(gc_obj vec) {
  return to_vector(vec)->len;
}

gc_obj vector_ref(gc_obj vec, gc_obj idx) {
  return to_vector(vec)->v[to_fixnum(idx)];
}

gc_obj vector_set(gc_obj vec, gc_obj idx, gc_obj val) {
  to_vector(vec)->v[to_fixnum(idx)] = val;
  return UNDEFINED;
}

gc_obj SCM_CLOSURE(gc_obj p, uint64_t len) {
  //  printf("make closure %li\n", len);
  closure_s* clo = rcimmix_alloc(sizeof(closure_s) + (len+1) * sizeof(gc_obj));
  clo->type = CLOSURE_TAG;
  clo->v[0] = p;
  return tag_closure(clo);
}

void SCM_CLOSURE_SET(gc_obj clo, gc_obj obj, uint64_t i) {
  //    printf("Closure set %li\n", i);
  to_closure(clo)->v[i + 1] = obj;
}

gc_obj SCM_CLOSURE_GET(gc_obj clo, gc_obj i) {
  //  printf("Closure get %li\n", to_fixnum(i));
  return to_closure(clo)->v[to_fixnum(i) + 1];
}
