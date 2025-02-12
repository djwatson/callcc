#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"

#define LOW_TAGS                                                               \
  X(FIXNUM, 0)                                                                 \
  X(FLONUM1, 1)                                                                \
  X(PTR, 2)                                                                    \
  X(CONS, 3)                                                                   \
  X(FLONUM2, 4)                                                                \
  X(FLONUM3, 5)                                                                \
  X(LITERAL, 6)                                                                \
  X(VECTOR, 7)

#define PTR_TAGS                                                               \
  X(STRING, 0x2)                                                               \
  X(RECORD, 0xa)                                                               \
  X(CLOSURE, 0x12)                                                             \
  X(SYMBOL, 0x1a)                                                              \
  X(CONT, 0x22)                                                                \
  X(FLONUM, 0x2a)

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
  void *slab;
  gc_obj v[];
} vector_s;

typedef struct record_s {
  uint64_t type;
  gc_obj v[];
} record_s;

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
record_s *to_record(gc_obj obj) { return (record_s *)(obj.value - PTR_TAG); }
closure_s *to_closure(gc_obj obj) { return (closure_s *)(obj.value - PTR_TAG); }
char to_char(gc_obj obj) { return (char)(obj.value >> 8); }

uint8_t get_tag(gc_obj obj) { return obj.value & TAG_MASK; }
uint8_t get_imm_tag(gc_obj obj) { return obj.value & IMMEDIATE_MASK; }
uint32_t get_ptr_tag(gc_obj obj) {
  return ((uint64_t *)(obj.value - PTR_TAG))[0];
}
bool is_char(gc_obj obj) { return get_imm_tag(obj) == CHAR_TAG; }
bool is_cons(gc_obj obj) { return get_tag(obj) == CONS_TAG; }
bool is_ptr(gc_obj obj) { return get_tag(obj) == PTR_TAG; }
bool is_closure(gc_obj obj) {
  return is_ptr(obj) && get_ptr_tag(obj) == CLOSURE_TAG;
}
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
gc_obj tag_symbol(symbol *s) {
  return (gc_obj){.value = ((int64_t)s + PTR_TAG)};
}
gc_obj tag_ptr(void *s) { return (gc_obj){.value = ((int64_t)s + PTR_TAG)}; }

#define TAG_SET2 ((1 << 5) | (1 << 4) | (1 << 1))
static bool has_tag_5_or_4_or_1(int64_t n) {
  // Note that unlike the paper, we need to explictly ensure n is
  // masked to 5 bits: shifting by more than 32 bits here is undefined
  // behavior, and clang will happily optimize everything out.
  return (((uint32_t)1 << (n & 0x1f)) & (~(uint32_t)0 / 0xff * TAG_SET2)) != 0;
}
static bool is_flonum_fast(gc_obj obj) {
  return has_tag_5_or_4_or_1(obj.value);
}

static bool is_flonum(gc_obj obj) {
  return is_flonum_fast(obj) || (is_ptr(obj) && get_ptr_tag(obj) == FLONUM_TAG);
}

gc_obj SCM_IS_FLONUM(gc_obj obj) {
  if (is_flonum(obj)) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

bool double_to_gc(double d, gc_obj *res) {
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

gc_obj double_to_gc_slow(double d) {
  uint64_t di;
  memcpy(&di, &d, sizeof(d));
  di = __builtin_rotateleft64(di, 4);
  di++; // Offset by one, so that we keep fixnum as 0 tag.
  if (has_tag_5_or_4_or_1(di)) {
    return (gc_obj){.value = di};
  }
  flonum_s *f = rcimmix_alloc(sizeof(flonum_s));
  f->type = FLONUM_TAG;
  f->x = d;
  return tag_ptr(f);
}

double to_double(gc_obj obj) {
  if (is_ptr(obj)) {
    flonum_s *f = to_raw_ptr(obj);
    return f->x;
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

static void display_double(gc_obj obj, int fd) {
  char buffer[40];
  double d = to_double(obj);
  snprintf(buffer, 40 - 3, "%g", d);
  if (strpbrk(buffer, ".eE") == nullptr) {
    size_t len = strlen(buffer);
    buffer[len] = '.';
    buffer[len + 1] = '0';
    buffer[len + 2] = '\0';
  }
  dprintf(fd, "%s", buffer);
}

gc_obj SCM_DISPLAY(gc_obj obj, gc_obj scmfd) {
  int fd = (int)to_fixnum(scmfd);
  auto tag = get_tag(obj);
  switch (tag) {
  case FIXNUM_TAG:
    dprintf(fd, "%li", to_fixnum(obj));
    break;
  case FLONUM1_TAG:
  case FLONUM2_TAG:
  case FLONUM3_TAG:
    display_double(obj, fd);
    break;
  case PTR_TAG: {
    auto ptr_tag = get_ptr_tag(obj);
    switch (ptr_tag) {
    case STRING_TAG: {
      auto str = to_string(obj);
      dprintf(fd, "%.*s", (int)to_fixnum(str->len), str->str);
      break;
    }
    case SYMBOL_TAG: {
      auto sym = to_symbol(obj);
      auto str = to_string(sym->name);
      dprintf(fd, "%.*s", (int)to_fixnum(str->len), str->str);
      break;
    }
    case RECORD_TAG: {
      dprintf(fd, "#<record>");
      break;
    }
    case CLOSURE_TAG: {
      dprintf(fd, "#<closure>");
      break;
    }
    case CONT_TAG: {
      dprintf(fd, "#<cont>");
      break;
    }
    case FLONUM_TAG: {
      display_double(obj, fd);
      break;
    }
    default:
      printf("Unknown ptr tag: %i\n", ptr_tag);
      abort();
    }
    break;
  }
  case CONS_TAG: {
    auto c = to_cons(obj);
    dprintf(fd, "(");
    while (is_cons(c->b)) {
      SCM_DISPLAY(c->a, scmfd);
      c = to_cons(c->b);
      dprintf(fd, " ");
    }
    SCM_DISPLAY(c->a, scmfd);
    if (c->b.value != NIL_TAG) {
      dprintf(fd, " . ");
      SCM_DISPLAY(c->b, scmfd);
    }
    dprintf(fd, ")");
    break;
  }
  case LITERAL_TAG: {
    auto lit_tag = get_imm_tag(obj);
    switch (lit_tag) {
    case CHAR_TAG: {
      dprintf(fd, "%c", to_char(obj));
      break;
    }
    case BOOL_TAG: {
      if (obj.value == TRUE_REP.value) {
        dprintf(fd, "#t");
      } else if (obj.value == FALSE_REP.value) {
        dprintf(fd, "#f");
      }
      break;
    }
    case NIL_TAG: {
      dprintf(fd, "()");
      break;
    }
    case UNDEFINED_TAG: {
      dprintf(fd, "#<undef>");
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
    dprintf(fd, "#(");
    for (uint64_t i = 0; i < to_fixnum(v->len); i++) {
      if (i != 0) {
        dprintf(fd, " ");
      }
      SCM_DISPLAY(v->v[i], scmfd);
    }
    dprintf(fd, ")");
    break;
  }
  default:
    printf("Unknown tag: %i\n", tag);
    abort();
  }
  return UNDEFINED;
}

////////////// MATH
#define likely(x) __builtin_expect(x, 1)
#define unlikely(x) __builtin_expect(x, 0)
#define NOINLINE __attribute__((noinline))
#define INLINE __attribute__((always_inline))

NOINLINE gc_obj SCM_LOAD_GLOBAL_FAIL(gc_obj a) {
  auto str = to_string(to_symbol(a)->name);
  printf("Attempting to load undefined sym: %.*s\n", (int)to_fixnum(str->len),
         str->str);
  abort();
}
INLINE gc_obj SCM_LOAD_GLOBAL(gc_obj a) {
  // assert(is_symbol(a));
  auto sym = to_symbol(a);
  auto val = sym->val;
#ifndef UNSAFE
  if (likely(val.value != UNDEFINED.value)) {
    return val;
  }
  [[clang::musttail]] return SCM_LOAD_GLOBAL_FAIL(a);
#else
  return val;
#endif
}

INLINE void SCM_SET_GLOBAL(gc_obj a, gc_obj b) {
  auto sym = to_symbol(a);
  sym->val = b;
  // gclog check if static, if not, quick set
  // printf("log global\n");
  gc_log((uint64_t)&sym->val);
}

NOINLINE void SCM_ARGCNT_FAIL() {
  printf("Call with invalid argcnt\n");
  abort();
}

NOINLINE void *SCM_LOAD_CLOSURE_PTR_FAIL(gc_obj a) {
  printf("Attempting to call non-closure:");
  SCM_DISPLAY(a, tag_fixnum(0));
  printf("\n");
  abort();
}
INLINE void *SCM_LOAD_CLOSURE_PTR(gc_obj a) {
#ifndef UNSAFE
  if (likely(is_closure(a))) {
#endif
    auto clo = to_closure(a);
    return (void *)clo->v[0].value;
#ifndef UNSAFE
  }
  [[clang::musttail]] return SCM_LOAD_CLOSURE_PTR_FAIL(a);
#endif
}

#define MATH_OVERFLOW_OP(OPNAME, OPLCNAME, OP, SHIFT)                          \
  NOINLINE __attribute__((preserve_most)) gc_obj SCM_##OPNAME##_SLOW(          \
      gc_obj a, gc_obj b) {                                                    \
    double fa, fb;                                                             \
    if (is_fixnum(a)) {                                                        \
      fa = to_fixnum(a);                                                       \
    } else if (is_flonum(a)) {                                                 \
      fa = to_double(a);                                                       \
    } else {                                                                   \
      printf(#OPNAME ": not a number:");                                       \
      SCM_DISPLAY(a, tag_fixnum(0));                                           \
      printf("\n");                                                            \
      abort();                                                                 \
    }                                                                          \
    if (is_fixnum(b)) {                                                        \
      fb = to_fixnum(b);                                                       \
    } else if (is_flonum(b)) {                                                 \
      fb = to_double(b);                                                       \
    } else {                                                                   \
      printf(#OPNAME ": not a number:");                                       \
      SCM_DISPLAY(b, tag_fixnum(0));                                           \
      printf("\n");                                                            \
      abort();                                                                 \
    }                                                                          \
    return double_to_gc_slow(OP(fa, fb));                                      \
  }                                                                            \
                                                                               \
  INLINE gc_obj SCM_##OPNAME(gc_obj a, gc_obj b) {                             \
    if (likely((is_fixnum(a) & is_fixnum(b)) == 1)) {                          \
      gc_obj res;                                                              \
      if (likely(!__builtin_##OPLCNAME##_overflow(a.value, SHIFT(b.value),     \
                                                  &res.value))) {              \
        return res;                                                            \
      } else {                                                                 \
        return SCM_##OPNAME##_SLOW(a, b);                                      \
      }                                                                        \
    } else if (likely((is_flonum_fast(a) & is_flonum_fast(b)) == 1)) {         \
      gc_obj res;                                                              \
      if (likely(                                                              \
              double_to_gc(OP(to_double_fast(a), to_double_fast(b)), &res))) { \
        return res;                                                            \
      } else {                                                                 \
        return SCM_##OPNAME##_SLOW(a, b);                                      \
      }                                                                        \
    } else {                                                                   \
      return SCM_##OPNAME##_SLOW(a, b);                                        \
    }                                                                          \
  }

#define MATH_ADD(a, b) ((a) + (b))
#define MATH_SUB(a, b) ((a) - (b))
#define MATH_MUL(a, b) ((a) * (b))
#define NOSHIFT(a) (a)
#define SHIFT(a) (a >> 3)
MATH_OVERFLOW_OP(ADD, add, MATH_ADD, NOSHIFT)
MATH_OVERFLOW_OP(SUB, sub, MATH_SUB, NOSHIFT)
MATH_OVERFLOW_OP(MUL, mul, MATH_MUL, SHIFT)

#define MATH_SIMPLE_OP(OPNAME, OP, FPOP)                                       \
                                                                               \
  NOINLINE gc_obj SCM_##OPNAME##_SLOW(gc_obj a, gc_obj b) {                    \
    double fa, fb;                                                             \
    if (is_fixnum(a)) {                                                        \
      fa = to_fixnum(a);                                                       \
    } else {                                                                   \
      fa = to_double(a);                                                       \
    }                                                                          \
    if (is_fixnum(b)) {                                                        \
      fb = to_fixnum(b);                                                       \
    } else {                                                                   \
      fb = to_double(b);                                                       \
    }                                                                          \
                                                                               \
    return double_to_gc_slow(FPOP(fa, fb));                                    \
  }                                                                            \
                                                                               \
  INLINE gc_obj SCM_##OPNAME(gc_obj a, gc_obj b) {                             \
    if (likely((is_fixnum(a) & is_fixnum(b)) == 1)) {                          \
      return tag_fixnum(OP(to_fixnum(a), to_fixnum(b)));                       \
    } else if (likely((is_flonum_fast(a) & is_flonum_fast(b)) == 1)) {         \
      gc_obj res;                                                              \
      if (likely(double_to_gc(FPOP(to_double_fast(a), to_double_fast(b)),      \
                              &res))) {                                        \
        return res;                                                            \
      } else {                                                                 \
        [[clang::musttail]] return SCM_##OPNAME##_SLOW(a, b);                  \
      }                                                                        \
    } else {                                                                   \
      [[clang::musttail]] return SCM_##OPNAME##_SLOW(a, b);                    \
    }                                                                          \
  }

#define MATH_DIV(a, b) ((a) / (b))
#define MATH_MOD(a, b) ((a) % (b))
#define MATH_FPMOD(a, b) (fmod((a), (b)))
MATH_SIMPLE_OP(DIV, MATH_DIV, MATH_DIV)
MATH_SIMPLE_OP(MOD, MATH_MOD, MATH_FPMOD)

#define MATH_COMPARE_OP(OPNAME, OP)                                            \
  NOINLINE __attribute__((preserve_most)) gc_obj SCM_##OPNAME##_SLOW(          \
      gc_obj a, gc_obj b) {                                                    \
    double fa, fb;                                                             \
    if (is_fixnum(a)) {                                                        \
      fa = to_fixnum(a);                                                       \
    } else if (is_flonum(a)) {                                                 \
      fa = to_double(a);                                                       \
    } else {                                                                   \
      printf(#OPNAME ": not a number:");                                       \
      SCM_DISPLAY(a, tag_fixnum(0));                                           \
      printf("\n");                                                            \
      abort();                                                                 \
    }                                                                          \
    if (is_fixnum(b)) {                                                        \
      fb = to_fixnum(b);                                                       \
    } else if (is_flonum(b)) {                                                 \
      fb = to_double(b);                                                       \
    } else {                                                                   \
      printf(#OPNAME ": not a number:");                                       \
      SCM_DISPLAY(b, tag_fixnum(0));                                           \
      printf("\n");                                                            \
      abort();                                                                 \
    }                                                                          \
    if (OP(fa, fb)) {                                                          \
      return TRUE_REP;                                                         \
    }                                                                          \
    return FALSE_REP;                                                          \
  }                                                                            \
                                                                               \
  INLINE gc_obj SCM_##OPNAME(gc_obj a, gc_obj b) {                             \
    if (likely((is_fixnum(a) & is_fixnum(b)) == 1)) {                          \
      if (OP(a.value, b.value)) {                                              \
        return TRUE_REP;                                                       \
      }                                                                        \
      return FALSE_REP;                                                        \
    }                                                                          \
    if (likely((is_flonum_fast(a) & is_flonum_fast(b)) == 1)) {                \
      if (OP(to_double_fast(a), to_double_fast(b))) {                          \
        return TRUE_REP;                                                       \
      }                                                                        \
      return FALSE_REP;                                                        \
    }                                                                          \
    return SCM_##OPNAME##_SLOW(a, b);                                          \
  }
#define MATH_LT(a, b) ((a) < (b))
#define MATH_LTE(a, b) ((a) <= (b))
#define MATH_GT(a, b) ((a) > (b))
#define MATH_GTE(a, b) ((a) >= (b))
#define MATH_EQ(a, b) ((a) == (b))
MATH_COMPARE_OP(LT, MATH_LT)
MATH_COMPARE_OP(LTE, MATH_LTE)
MATH_COMPARE_OP(GT, MATH_GT)
MATH_COMPARE_OP(GTE, MATH_GTE)
MATH_COMPARE_OP(NUM_EQ, MATH_EQ)

INLINE gc_obj SCM_CAR(gc_obj obj) {
#ifndef UNSAFE
  if (!is_cons(obj)) {
    abort();
  }
#endif
  return to_cons(obj)->a;
}

INLINE gc_obj SCM_CDR(gc_obj obj) {
#ifndef UNSAFE
  if (!is_cons(obj)) {
    abort();
  }
#endif
  return to_cons(obj)->b;
}

INLINE gc_obj SCM_SETCAR(gc_obj obj, gc_obj val) {
#ifndef UNSAFE
  if (!is_cons(obj)) {
    abort();
  }
#endif
  auto c = to_cons(obj);
  c->a = val;
  //  printf("log setcar\n");
  gc_log_fast((uint64_t)&c->a);
  return UNDEFINED;
}

INLINE gc_obj SCM_SETCDR(gc_obj obj, gc_obj val) {
#ifndef UNSAFE
  if (!is_cons(obj)) {
    abort();
  }
#endif
  auto c = to_cons(obj);
  c->b = val;
  //  printf("log setcdr\n");
  gc_log_fast((uint64_t)&c->b);
  return UNDEFINED;
}
INLINE gc_obj SCM_CONS(gc_obj a, gc_obj b) {
  cons_s *c = rcimmix_alloc(sizeof(cons_s));
  c->a = a;
  c->b = b;
  return tag_cons(c);
}

INLINE gc_obj SCM_GUARD(gc_obj a, int64_t type) {
  type /= 8;
  auto low_tag = type & TAG_MASK;
  if (low_tag == LITERAL_TAG) {
    if (type == get_imm_tag(a)) {
      return TRUE_REP;
    }
    return FALSE_REP;
  }
  if (low_tag != get_tag(a)) {
    return FALSE_REP;
  }
  if (low_tag == PTR_TAG) {
    if (type == get_ptr_tag(a)) {
      return TRUE_REP;
    }
    return FALSE_REP;
  }
  return TRUE_REP;
}

INLINE gc_obj SCM_MAKE_VECTOR(gc_obj obj) {
  auto res = rcimmix_alloc_with_slab(sizeof(vector_s) +
                                     to_fixnum(obj) * sizeof(gc_obj));
  vector_s *v = res.p;
  v->slab = res.slab;
  v->len = obj;
  return tag_vector(v);
}

INLINE gc_obj SCM_VECTOR_LENGTH(gc_obj vec) {
#ifndef UNSAFE
  if (unlikely(!is_vector(vec))) {
    abort();
  }
#endif
  return to_vector(vec)->len;
}

INLINE gc_obj SCM_VECTOR_REF(gc_obj vec, gc_obj idx) {
#ifndef UNSAFE
  if (unlikely(!is_fixnum(idx))) {
    abort();
  }
  if (unlikely(!is_vector(vec))) {
    abort();
  }
#endif
  auto v = to_vector(vec);
  auto i = to_fixnum(idx);
#ifndef UNSAFE
  if (unlikely(i >= to_fixnum(v->len))) {
    abort();
  }
#endif

  return v->v[i];
}

INLINE gc_obj SCM_VECTOR_SET(gc_obj vec, gc_obj idx, gc_obj val) {
#ifndef UNSAFE
  if (unlikely(!is_fixnum(idx))) {
    abort();
  }
  if (unlikely(!is_vector(vec))) {
    abort();
  }
#endif
  auto v = to_vector(vec);
  auto i = to_fixnum(idx);
#ifndef UNSAFE
  if (unlikely(i >= to_fixnum(v->len))) {
    abort();
  }
#endif
  v->v[i] = val;

  gc_log_with_slab((uint64_t)&v->v[i], v->slab);

  return UNDEFINED;
}
INLINE gc_obj SCM_VECTOR_SET_FAST(gc_obj vec, gc_obj idx, gc_obj val) {
  auto v = to_vector(vec);
  auto i = to_fixnum(idx);
  v->v[i] = val;

  return UNDEFINED;
}

INLINE gc_obj SCM_CLOSURE(gc_obj p, uint64_t len) {
  //  printf("make closure %li\n", len);
  assert(gc_is_small(sizeof(closure_s) + ((len + 1) * sizeof(gc_obj))));
  closure_s *clo =
      rcimmix_alloc(sizeof(closure_s) + (len + 1) * sizeof(gc_obj));
  clo->type = CLOSURE_TAG;
  clo->v[0] = p;
  return tag_closure(clo);
}

INLINE void SCM_CLOSURE_SET(gc_obj clo, gc_obj obj, uint64_t i) {
  //    printf("Closure set %li\n", i);
  auto c = to_closure(clo);
  c->v[i + 1] = obj;
  /* printf("log closure\n"); */
  gc_log_fast((uint64_t)&c->v[i + 1]);
}

INLINE void SCM_CLOSURE_SET_FAST(gc_obj clo, gc_obj obj, uint64_t i) {
  //    printf("Closure set %li\n", i);
  auto c = to_closure(clo);
  c->v[i + 1] = obj;
}

INLINE gc_obj SCM_CLOSURE_GET(gc_obj clo, gc_obj i) {
  //  printf("Closure get %li\n", to_fixnum(i));
  return to_closure(clo)->v[to_fixnum(i) + 1];
}

///////////// CALL cc

// MUST look like a closure.
typedef struct ccsave {
  uint64_t type;
  gc_obj v[1];

  struct ccsave *prev_link;
  size_t sz;
  int64_t stack[];
} ccsave;

ccsave *cur_link = NULL;
static uint8_t tmpstack[100];
static gc_obj ccresthunk(gc_obj unused, gc_obj n) {
  ccsave *c = (ccsave *)to_closure(unused);
  cur_link = c->prev_link;

  int64_t stack_bottom = (int64_t)gc_get_stack_top() - c->sz;
  void *saved_stack = c->stack;
  size_t saved_sz = c->sz;
  uint64_t tmpstackalign = ((uint64_t)tmpstack + 100) & ~15;

#if defined(__x86_64__)
  asm volatile(
      "mov %4, %%r12\n\t" // Save the return value in a callee-saved reg.
      "mov %1, %%r13\n\t" // save old sp
      "mov %0, %%rsp\n\t" // Switch to tmpstack.
      "mov %1, %%rdi\n\t" // Set up memcpy call args
      "mov %2, %%rsi\n\t"
      "mov %3, %%rdx\n\t"
      "call memcpy\n\t"      // call memcpy
      "mov %%r13, %%rsp\n\t" // Restore the old stack pointer.
      "pop %%rbp\n\t"        // Pop the old frame pointer.
      "mov %%r12, %%rax\n\t" // Move return value from callee-saved to rax.
      "ret\n\t"              // Return to caller of 'callcc'.
      :                      // output
      : "r"(tmpstackalign), "r"(stack_bottom), "r"(saved_stack), "r"(saved_sz),
        "r"(n)
      : "rsp", "rbp", "memory", "rax", "r12", "r13", "rdi", "rsi",
        "rdx" // clobbers
  );
#elif defined(__aarch64__)
  asm volatile("mov x19, %4\n\t" // Save the return value in a callee-saved reg.
               "mov x20, %1\n\t" // Save the old sp
               "mov SP, %0\n\t"  // Switch to tmpstack.
               "mov x0, %1\n\t"  // Set up memcpy call args
               "mov x1, %2\n\t"
               "mov x2, %3\n\t"
               "bl memcpy\n\t"   // call memcpy
               "mov x0, x19\n\t" // Restore the old stack pointer.
               "mov sp, x20\n\t" // Move return value from callee-saved to rax.
               "ldp x29, x30, [sp], #16\n\t" // leave
               "ret\n\t"                     // Return to caller of 'callcc'.
               :                             // output
               : "r"(tmpstack), "r"(stack_bottom), "r"(saved_stack),
                 "r"(saved_sz), "r"(n)
               : "memory", "x19", "x20", "x0", "x1", "x2");
#else
#error "Arch not supported for CALLCC"
#endif
  __builtin_unreachable();
  return n;
}

// We can 'preserve_none' here because we are explicitly loading the arg,
// and we don't need to preserve anything.
static __attribute__((preserve_none)) void need_more_frames() {
  gc_obj res;
  // This is called as a return point.  In x86_64, the return is in rax.
#if defined(__x86_64__)
  asm volatile("mov %%rax, %0\n\t" : "=r"(res) : : "rax");
  // In aarch return and arg1 are equal, so this could be
  // need_more_frames(int64_t res),
  // but let's do it like this for consistency.
#elif defined(__aarch64__)
  asm volatile("mov %0, x0\n\t" : "=r"(res) : : "x0");
#endif
  // assert(cur_link);
  ccresthunk(tag_closure((closure_s *)cur_link), res);
}

extern int64_t argcnt;
extern int64_t wanted_argcnt;

__attribute__((returns_twice, noinline, preserve_none)) gc_obj
SCM_CALLCC(gc_obj cont) {
  assert(is_closure(cont));
  auto clo = to_closure(cont);

  void *stack_bottom = __builtin_frame_address(0);
  void *stacktop = (void *)gc_get_stack_top();
  size_t stack_sz = stacktop - stack_bottom;
  ccsave *stack = rcimmix_alloc(sizeof(ccsave) + stack_sz);
  stack->sz = stack_sz;
  stack->type = CLOSURE_TAG;
  stack->v[0] = (gc_obj){.value = (int64_t)ccresthunk};
  stack->prev_link = cur_link;
  memcpy(stack->stack, stack_bottom, stack_sz);

  auto cc = tag_closure((closure_s *)stack);
  argcnt = 2; // Two args: Closure ptr & cc.

  gc_obj unused_res;

  cur_link = stack;
#if defined(__x86_64__)
  asm volatile("mov %1, %%rsp\n\t" // Reset stack pointer to stacktop
               "mov $0, %%rbp\n\t" // Clear out the frame pointer
               "push %2\n\t"       // Push return address of need_more_frames
               "mov %3, %%rdi\n\t" // Set up call thunk - closure arg
               "mov %4, %%rsi\n\t" // Set up call thunk - callcc cont arg
               "jmp *%5\n\t"       // Jump to thunk.
               : "=r"(unused_res)  // output
               : "r"(stacktop), "r"(need_more_frames), "r"(cont), "r"(cc),
                 "r"(clo->v[0])         // input
               : "rdi", "rsi", "memory" // clobbers
  );

#elif defined(__aarch64__)
  asm volatile("mov sp, %1\n\t"   // Reset stack pointer to stacktop
               "mov x29, 0\n\t"   // Clear out the frame pointer
               "mov x30, %2\n\t"  // Set return address of need_more_frames
               "mov x0, %3\n\t"   // Set up call thunk
               "mov x1, %4\n\t"   // and call argument
               "br %5\n\t"        // Jump to thunk.
               : "=r"(unused_res) // output
               : "r"(stacktop), "r"(need_more_frames), "r"(cont), "r"(cc),
                 "r"(clo->v[0])       // input
               : "x0", "x1", "memory" // clobbers
  );
#endif
  // This is unreachable, but if you use __builtin_unreachable(), callers
  // will not have valid return points ... so fake a call.

  // Also, the result *must* be unknowable at compile-time: clang is smart
  // enough to inline this result, not knowing there is no result.
  return unused_res;
}

/////////////////////// CONSARGS for varargs functions

static const uint64_t reg_arg_cnt = 6;
/* #if defined(__x86_64__) */
/* static const uint64_t reg_arg_cnt = 6; */
/* #elif defined(__aarch64__) */
/* static const uint64_t reg_arg_cnt = 8; */
/* #endif */

// TODO: gc shadow_stack
gc_obj shadow_stack[100];
gc_obj consargs_stub(gc_obj a0, gc_obj a1, gc_obj a2, gc_obj a3, gc_obj a4,
                     gc_obj a5) {
  auto cnt = argcnt - wanted_argcnt;
  auto cur = argcnt;
  gc_obj head = NIL;
  gc_obj *tail = &head;
  switch (wanted_argcnt) {
  case 0:
    if (cnt-- == 0) {
      return head;
    }
    *tail = SCM_CONS(a0, NIL);
    tail = &to_cons(*tail)->b;
  case 1:
    if (cnt-- == 0) {
      return head;
    }
    *tail = SCM_CONS(a1, NIL);
    tail = &to_cons(*tail)->b;
  case 2:
    if (cnt-- == 0) {
      return head;
    }
    *tail = SCM_CONS(a2, NIL);
    tail = &to_cons(*tail)->b;
  case 3:
    if (cnt-- == 0) {
      return head;
    }
    *tail = SCM_CONS(a3, NIL);
    tail = &to_cons(*tail)->b;
  case 4:
    if (cnt-- == 0) {
      return head;
    }
    *tail = SCM_CONS(a4, NIL);
    tail = &to_cons(*tail)->b;
  case 5:
    if (cnt-- == 0) {
      return head;
    }
    *tail = SCM_CONS(a5, NIL);
    tail = &to_cons(*tail)->b;
  default:
  }
  auto res = NIL;
  while (cur > reg_arg_cnt) {
    if (cur <= wanted_argcnt) {
      *tail = res;
      shadow_stack[cur - reg_arg_cnt] = head;
      return head;
    }
    res = SCM_CONS(shadow_stack[cur - reg_arg_cnt - 1], res);
    cur--;
  }
  *tail = res;
  return head;
}

INLINE gc_obj SCM_STRING_LENGTH(gc_obj obj) { return to_string(obj)->len; }

INLINE gc_obj SCM_EQ(gc_obj a, gc_obj b) {
  if (a.value == b.value) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

INLINE gc_obj SCM_MAKE_STRING(gc_obj len, gc_obj fill) {
  // Align.
  auto strlen = (to_fixnum(len) + 7) & ~7;
  string_s *str = rcimmix_alloc(sizeof(string_s) + strlen);
  str->type = STRING_TAG;
  str->len = len;
  if (fill.value != FALSE_REP.value) {
    memset(str->str, to_char(fill), to_fixnum(len));
  }
  return tag_string(str);
}

INLINE gc_obj SCM_CHAR_INTEGER(gc_obj ch) { return tag_fixnum(to_char(ch)); }

INLINE gc_obj SCM_INTEGER_CHAR(gc_obj i) { return tag_char(to_fixnum(i)); }

INLINE gc_obj SCM_SYMBOL_STRING(gc_obj sym) { return to_symbol(sym)->name; }

INLINE gc_obj SCM_STRING_REF(gc_obj str, gc_obj pos) {
  return tag_char(to_string(str)->str[to_fixnum(pos)]);
}

INLINE gc_obj SCM_STRING_SET(gc_obj str, gc_obj pos, gc_obj ch) {
  to_string(str)->str[to_fixnum(pos)] = to_char(ch);
  return UNDEFINED;
}

INLINE gc_obj SCM_MAKE_SYMBOL(gc_obj str) {
  symbol *sym = rcimmix_alloc(sizeof(symbol));
  sym->type = SYMBOL_TAG;
  sym->name = str;
  sym->val = UNDEFINED;
  return tag_symbol(sym);
}

INLINE gc_obj SCM_EXACT(gc_obj flo) { return tag_fixnum(to_double(flo)); }

INLINE gc_obj SCM_INEXACT(gc_obj fix) {
  if (is_flonum(fix)) {
    return fix;
  }
  gc_obj res;
  double d = (double)to_fixnum(fix);
  if (double_to_gc(d, &res)) {
    return res;
  }
  flonum_s *f = rcimmix_alloc(sizeof(flonum_s));
  f->type = FLONUM_TAG;
  f->x = d;
  return tag_ptr(f);
}
////// records
INLINE gc_obj SCM_MAKE_RECORD(gc_obj sz) {
  assert(gc_is_small(sizeof(record_s) + (to_fixnum(sz) * sizeof(gc_obj))));
  record_s *r =
      rcimmix_alloc(sizeof(record_s) + (to_fixnum(sz) * sizeof(gc_obj)));
  r->type = RECORD_TAG;

  return tag_ptr(r);
}

INLINE gc_obj SCM_RECORD_REF(gc_obj r, gc_obj idx) {
  return to_record(r)->v[to_fixnum(idx)];
}

INLINE gc_obj SCM_RECORD_SET(gc_obj r, gc_obj idx, gc_obj val) {
  auto rec = to_record(r);
  auto i = to_fixnum(idx);
  rec->v[i] = val;
  // printf("log record\n");
  gc_log_fast((uint64_t)&rec->v[i]);
  return UNDEFINED;
}

////////// Symbol table
extern gc_obj symbol_table;
INLINE gc_obj SCM_GET_SYM_TABLE() { return symbol_table; }

///////math
INLINE gc_obj SCM_SIN(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(sin(d));
}

INLINE gc_obj SCM_COS(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(cos(d));
}

INLINE gc_obj SCM_ASIN(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(asin(d));
}

INLINE gc_obj SCM_ACOS(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(acos(d));
}

INLINE gc_obj SCM_TAN(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(tan(d));
}

INLINE gc_obj SCM_ATAN(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(atan(d));
}

INLINE gc_obj SCM_SQRT(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(sqrt(d));
}

INLINE gc_obj SCM_ROUND(gc_obj f) {
  auto x = to_double(f);
  double rounded = round(x);
  if (fabs(x - rounded) == 0.5) {
    if (fmod(rounded, 2.0) != 0.0) {
      rounded = rounded + (x > 0 ? -1 : 1);
    }
  }
  return double_to_gc_slow(rounded);
}

INLINE gc_obj SCM_FLOOR(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(floor(d));
}

INLINE gc_obj SCM_CEILING(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(ceil(d));
}

INLINE gc_obj SCM_EXP(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(exp(d));
}

INLINE gc_obj SCM_LOG(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(log(d));
}

///// Shadow stack

INLINE void SCM_WRITE_SHADOW_STACK(gc_obj pos, gc_obj obj) {
  shadow_stack[to_fixnum(pos)] = obj;
}

INLINE gc_obj SCM_READ_SHADOW_STACK(uint64_t pos) { return shadow_stack[pos]; }

static uint64_t hashmix(uint64_t key) {
  key += (key << 10);
  key ^= (key >> 6);
  return key;
}
INLINE gc_obj SCM_EQ_HASH(gc_obj h) {
  return (gc_obj){.value = (long)hashmix(h.value) << 3};
}
static uint64_t stringhash(char *str, uint64_t len) {
  uint64_t hash = 401887359;
  uint64_t *strp = (void *)str;
  while (len > 8) {
    hash += *strp;
    hash = hashmix(hash);
    len -= 8;
    strp++;
    str += 8;
  }
  while (len > 0) {
    hash += *str;
    hash = hashmix(hash);
    len -= 1;
    str += 1;
  }
  return hash;
}

INLINE gc_obj SCM_STRING_HASH(gc_obj h) {
  auto str = to_string(h);
  auto hash = stringhash(str->str, to_fixnum(str->len));
  // auto hash = XXH3_64bits(str->str, to_fixnum(str->len));
  return tag_fixnum((int)hash);
}
INLINE gc_obj SCM_STRING_CPY(gc_obj tostr, gc_obj tostart, gc_obj fromstr,
                             gc_obj fromstart, gc_obj fromend) {
  auto from_pos = to_fixnum(fromstart);
  auto len = to_fixnum(fromend) - from_pos;
  auto start = to_fixnum(tostart);
  auto to = to_string(tostr);
  auto from = to_string(fromstr);
  memcpy(&to->str[start], &from->str[from_pos], len);
  return UNDEFINED;
}
INLINE gc_obj SCM_AND(gc_obj num, gc_obj mask) {
  return (gc_obj){.value = num.value & mask.value};
}

////////////// IO

#include <fcntl.h>
#include <unistd.h>

INLINE gc_obj SCM_OPEN_FD(gc_obj filename, gc_obj input) {
  auto str = to_string(filename);
  char name[256];
  memcpy(name, str->str, to_fixnum(str->len));
  assert(to_fixnum(str->len) < 255);
  name[to_fixnum(str->len)] = '\0';
  auto readonly = input.value == TRUE_REP.value;
  return tag_fixnum(
      open(name, readonly ? O_RDONLY : O_WRONLY | O_CREAT | O_TRUNC, 0777));
}

INLINE gc_obj SCM_READ_FD(gc_obj scmfd, gc_obj scmbuf) {
  auto buf = to_string(scmbuf);
  int fd = (int)to_fixnum(scmfd);
  auto res = read(fd, buf->str, to_fixnum(buf->len));
  if (res < 0) {
    printf("SCM_READ_FD error: %li\n", res);
    exit(-1);
  }
  return tag_fixnum(res);
}

INLINE gc_obj SCM_WRITE_FD(gc_obj scmfd, gc_obj scmlen, gc_obj scmbuf) {
  int fd = (int)to_fixnum(scmfd);
  auto len = to_fixnum(scmlen);
  auto buf = to_string(scmbuf);
  auto res = write(fd, buf->str, len);
  if (res != len) {
    printf("Could not write %li bytes to fd %i\n", len, fd);
    exit(-1);
  }
  return UNDEFINED;
}

#include <errno.h>

INLINE gc_obj SCM_CLOSE_FD(gc_obj fd) {
  auto res = close((int)to_fixnum(fd));
  if (res != 0) {
    printf("Error closing fd %li, res %i, errno %i\n", to_fixnum(fd), res,
           errno);
    perror("foo");
    exit(-1);
  }
  return tag_fixnum(res);
}

INLINE gc_obj SCM_FILE_EXISTS(gc_obj scmname) {
  auto str = to_string(scmname);
  char name[256];
  memcpy(name, str->str, to_fixnum(str->len));
  assert(to_fixnum(str->len) < 255);
  name[to_fixnum(str->len)] = '\0';
  auto res = access(name, F_OK);
  if (res == 0) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

INLINE gc_obj SCM_DELETE_FILE(gc_obj scmname) {
  auto str = to_string(scmname);
  char name[256];
  memcpy(name, str->str, to_fixnum(str->len));
  assert(to_fixnum(str->len) < 255);
  name[to_fixnum(str->len)] = '\0';
  return tag_fixnum(unlink(name));
}
/////// FLONUMS
INLINE gc_obj SCM_FLONUM_BOX(double d) { return double_to_gc_slow(d); }
INLINE double SCM_FLONUM_UNBOX(gc_obj d) { return to_double(d); }
INLINE double SCM_INEXACT_UNBOXED(gc_obj fix) {
  if (is_flonum(fix)) {
    return to_double(fix);
  }
  return (double)to_fixnum(fix);
}
