#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <gmp.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <time.h>
#include <unistd.h>

#include <utf8proc.h>

#include "util/util.h"

#include "gc.h"

#include "rodata_handler.h"
#include "unionfind.h"

enum : int64_t {
  // Low bit tags for gc_obj
  FIXNUM_TAG = 0,
  FLONUM1_TAG = 1,
  PTR_TAG = 2,
  CONS_TAG = 3,
  FLONUM2_TAG = 4,
  FLONUM3_TAG = 5,
  LITERAL_TAG = 6,
  VECTOR_TAG = 7,

  // ptr tags, used only in PTR_TAG types.
  STRING_TAG = 0x2,
  RECORD_TAG = 0xa,
  CLOSURE_TAG = 0x12,
  SYMBOL_TAG = 0x1a,
  BYTEVECTOR_TAG = 0x22,
  NUMBER_TAG = 0x2a,
  FLONUM_TAG = 0x2a,
  BIGNUM_TAG = 0x12a,
  RATNUM_TAG = 0x22a,
  COMPNUM_TAG = 0x32a,

  // Literal tags, used only for LITERAL_TAG types
  BOOL_TAG = 0x6,
  CHAR_TAG = 0x0e,
  NIL_TAG = 0x16,
  EOF_TAG = 0x1e,
  UNDEFINED_TAG = 0x26,

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

typedef struct bignum_s {
  uint64_t type;
  mpz_t x;
} bignum_s;

typedef struct ratnum_s {
  uint64_t type;
  gc_obj num;
  gc_obj denom;
} ratnum_s;

typedef struct compnum_s {
  uint64_t type;
  gc_obj real;
  gc_obj imag;
} compnum_s;

typedef struct string_s {
  uint64_t type;
  gc_obj len;
  gc_obj bytes;
  char *strdata;
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

typedef struct bytevector_s {
  uint64_t type;
  gc_obj len;
  uint8_t v[];
} bytevector_s;

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
static void *to_raw_ptr(gc_obj obj) { return (void *)(obj.value & ~TAG_MASK); }
static string_s *to_string(gc_obj obj) {
  return (string_s *)(obj.value - PTR_TAG);
}
static symbol *to_symbol(gc_obj obj) { return (symbol *)(obj.value - PTR_TAG); }
static int64_t to_fixnum(gc_obj obj) { return obj.value >> 3; }
static bignum_s *to_bignum(gc_obj obj) {
  return (bignum_s *)(obj.value - PTR_TAG);
}
static ratnum_s *to_ratnum(gc_obj obj) {
  return (ratnum_s *)(obj.value - PTR_TAG);
}
static compnum_s *to_compnum(gc_obj obj) {
  return (compnum_s *)(obj.value - PTR_TAG);
}
static cons_s *to_cons(gc_obj obj) { return (cons_s *)(obj.value - CONS_TAG); }
static vector_s *to_vector(gc_obj obj) {
  return (vector_s *)(obj.value - VECTOR_TAG);
}
static record_s *to_record(gc_obj obj) {
  return (record_s *)(obj.value - PTR_TAG);
}
static closure_s *to_closure(gc_obj obj) {
  return (closure_s *)(obj.value - PTR_TAG);
}
static bytevector_s *to_bytevector(gc_obj obj) {
  return (bytevector_s *)(obj.value - PTR_TAG);
}
static int32_t to_char(gc_obj obj) { return (int32_t)(obj.value >> 8); }

static uint8_t get_tag(gc_obj obj) { return obj.value & TAG_MASK; }
static uint8_t get_imm_tag(gc_obj obj) { return obj.value & IMMEDIATE_MASK; }
static uint32_t get_ptr_tag(gc_obj obj) {
  return ((uint64_t *)(obj.value - PTR_TAG))[0];
}
static bool is_char(gc_obj obj) { return get_imm_tag(obj) == CHAR_TAG; }
static bool is_cons(gc_obj obj) { return get_tag(obj) == CONS_TAG; }
static bool is_ptr(gc_obj obj) { return get_tag(obj) == PTR_TAG; }
static bool is_number(gc_obj obj) {
  return (get_tag(obj) == PTR_TAG) && ((uint8_t)get_ptr_tag(obj) == NUMBER_TAG);
}
static bool is_closure(gc_obj obj) {
  return is_ptr(obj) && get_ptr_tag(obj) == CLOSURE_TAG;
}
/* static bool is_literal(gc_obj obj) { return get_tag(obj) == LITERAL_TAG; } */
static bool is_string(gc_obj obj) {
  return is_ptr(obj) && get_ptr_tag(obj) == STRING_TAG;
}
static bool is_bytevector(gc_obj obj) {
  return is_ptr(obj) && get_ptr_tag(obj) == BYTEVECTOR_TAG;
}
/* static bool is_record(gc_obj obj) { */
/*   return is_ptr(obj) && get_ptr_tag(obj) == RECORD_TAG; */
/* } */
/* static bool is_undefined(gc_obj obj) { return get_imm_tag(obj) ==
 * UNDEFINED_TAG; } */
static bool is_vector(gc_obj obj) { return get_tag(obj) == VECTOR_TAG; }
static bool is_symbol(gc_obj obj) {
  return is_ptr(obj) && get_ptr_tag(obj) == SYMBOL_TAG;
}
static bool is_fixnum(gc_obj obj) { return get_tag(obj) == FIXNUM_TAG; }
static bool is_bignum(gc_obj obj) {
  return is_ptr(obj) && get_ptr_tag(obj) == BIGNUM_TAG;
}
static bool is_ratnum(gc_obj obj) {
  return is_ptr(obj) && get_ptr_tag(obj) == RATNUM_TAG;
}
static bool is_compnum(gc_obj obj) {
  return is_ptr(obj) && get_ptr_tag(obj) == COMPNUM_TAG;
}
/* static bool is_heap_object(gc_obj obj) { return !is_fixnum(obj) &&
 * !is_literal(obj); } */
static gc_obj tag_fixnum(int64_t num) {
  assert(((num << 3) >> 3) == num);
  return (gc_obj){.value = num << 3};
}
static gc_obj tag_string(string_s *s) {
  return (gc_obj){.value = ((int64_t)s + PTR_TAG)};
}
static gc_obj tag_cons(cons_s *s) {
  return (gc_obj){.value = ((int64_t)s + CONS_TAG)};
}
static gc_obj tag_vector(vector_s *s) {
  return (gc_obj){.value = ((int64_t)s + VECTOR_TAG)};
}
static gc_obj tag_bytevector(bytevector_s *s) {
  return (gc_obj){.value = ((int64_t)s + PTR_TAG)};
}
static gc_obj tag_closure(closure_s *s) {
  return (gc_obj){.value = ((int64_t)s + PTR_TAG)};
}
static gc_obj tag_char(uint32_t ch) {
  return (gc_obj){.value = (((int64_t)ch << 8) + CHAR_TAG)};
}
static gc_obj tag_symbol(symbol *s) {
  return (gc_obj){.value = ((int64_t)s + PTR_TAG)};
}
static gc_obj tag_ptr(void *s) {
  return (gc_obj){.value = ((int64_t)s + PTR_TAG)};
}

enum : uint64_t { TAG_SET2 = ((1 << 5) | (1 << 4) | (1 << 1)) };

static bool has_tag_5_or_4_or_1(uint64_t n) {
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

INLINE gc_obj SCM_EOF() { return EOF_OBJ; }

INLINE gc_obj SCM_IS_EOF(gc_obj a) {
  if (a.value == EOF_OBJ.value) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

gc_obj SCM_IS_FLONUM_SLOW(gc_obj obj) {
  if (is_flonum(obj)) {
    return TRUE_REP;
  }
  return FALSE_REP;
}
INLINE gc_obj SCM_IS_FLONUM(gc_obj obj) {
  if (!is_flonum_fast(obj) && !is_ptr(obj)) {
    return FALSE_REP;
  }
  [[clang::musttail]] return SCM_IS_FLONUM_SLOW(obj);
}

gc_obj SCM_IS_BIGNUM(gc_obj obj) {
  if (is_bignum(obj)) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

gc_obj SCM_IS_RATNUM(gc_obj obj) {
  if (is_ratnum(obj)) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

gc_obj SCM_IS_COMPNUM(gc_obj obj) {
  if (is_compnum(obj)) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

static bool double_to_gc(double d, gc_obj *res) {
  uint64_t di;
  memcpy(&di, &d, sizeof(d));
  di = __builtin_rotateleft64(di, 4);
  di++; // Offset by one, so that we keep fixnum as 0 tag.
  if (has_tag_5_or_4_or_1(di)) {
    *res = (gc_obj){.value = (int64_t)di};
    return true;
  }
  return false;
}

static gc_obj double_to_gc_slow(double d) {
  uint64_t di;
  memcpy(&di, &d, sizeof(d));
  di = __builtin_rotateleft64(di, 4);
  di++; // Offset by one, so that we keep fixnum as 0 tag.
  if (has_tag_5_or_4_or_1(di)) {
    return (gc_obj){.value = (int64_t)di};
  }
  flonum_s *f = gc_alloc(sizeof(flonum_s));
  f->type = FLONUM_TAG;
  f->x = d;
  return tag_ptr(f);
}

static double to_double(gc_obj obj) {
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

static double to_double_fast(gc_obj obj) {
  uint64_t r = obj.value - 1;
  r = __builtin_rotateright64(r, 4);
  double res;
  memcpy(&res, &r, sizeof(res));
  return res;
}

/*
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

// This is only for testing, display is implemented in scheme.
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
      dprintf(fd, "%.*s", (int)to_fixnum(str->bytes), str->strdata);
      break;
    }
    case SYMBOL_TAG: {
      auto sym = to_symbol(obj);
      auto str = to_string(sym->name);
      dprintf(fd, "%.*s", (int)to_fixnum(str->bytes), str->strdata);
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
    case FLONUM_TAG: {
      display_double(obj, fd);
      break;
    }
    case BIGNUM_TAG: {
      auto bn = to_bignum(obj);
      char *gstr = mpz_get_str(nullptr, 10, bn->x);
      dprintf(fd, "%s", gstr);
      break;
    }
    case RATNUM_TAG: {
      auto rat = to_ratnum(obj);
      abort(); // TODO
      break;
    }
    case COMPNUM_TAG: {
      auto c = to_compnum(obj);
      SCM_DISPLAY(c->real, scmfd);
      dprintf(fd, "+");
      SCM_DISPLAY(c->imag, scmfd);
      dprintf(fd, "i");
      break;
    }
    case BYTEVECTOR_TAG: {
      dprintf(fd, "#u8(");
      auto bv = to_bytevector(obj);
      for (int64_t i = 0; i < to_fixnum(bv->len); i++) {
        if (i != 0) {
          dprintf(fd, " ");
        }
        dprintf(fd, "%i", bv->v[i]);
      }
      dprintf(fd, ")");
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
      uint8_t buf[4];
      auto bytecnt = utf8proc_encode_char(to_char(obj), buf);
      dprintf(fd, "%.*s", (int)bytecnt, buf);
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
    for (int64_t i = 0; i < to_fixnum(v->len); i++) {
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
*/

////////////// MATH

_Noreturn void S_error(gc_obj closure, gc_obj msg, gc_obj arg);
static gc_obj from_c_str(char *str);

extern int64_t argcnt;
extern int64_t wanted_argcnt;

NOINLINE _Noreturn static void scm_runtime_error1(char *msg, gc_obj obj) {
  argcnt = 3;
  S_error(UNDEFINED, from_c_str(msg), obj);
}
NOINLINE _Noreturn void scm_runtime_error0(char *msg) {
  argcnt = 2;
  S_error(UNDEFINED, from_c_str(msg), UNDEFINED);
}
NOINLINE gc_obj SCM_LOAD_GLOBAL_FAIL(gc_obj a) {
  scm_runtime_error1("Attempting to load undefined sym:", a);
}
INLINE gc_obj SCM_LOAD_GLOBAL(gc_obj a) {
  auto sym = to_symbol(a);
  auto val = sym->val;
  // SCM_DISPLAY(a, tag_fixnum(0));
#ifndef UNSAFE
  if (likely(val.value != UNDEFINED.value)) {
    return val;
  }
  [[clang::musttail]] return SCM_LOAD_GLOBAL_FAIL(a);
#else
  return val;
#endif
}

// Quick check to determine if we need to gc_log a field: If it is not
// a pointer field, don't log (logging can be slow).
enum : uint64_t { PTR_TAG_SET = ((1 << 2) | (1 << 3) | (1 << 7)) };

static bool has_ptr_tag(gc_obj n) {
  return (((uint32_t)1 << (n.value & 0x1f)) &
          (~(uint32_t)0 / 0xff * PTR_TAG_SET)) != 0;
}
INLINE gc_obj SCM_SET_GLOBAL(gc_obj a, gc_obj b) {
  auto sym = to_symbol(a);
  sym->val = b;
  // gclog check if static, if not, quick set
  // printf("log global\n");
  if (has_ptr_tag(b)) {
    gc_log((uint64_t)&sym->val);
  }
  return UNDEFINED;
}

NOINLINE void SCM_ARGCNT_FAIL() {
  scm_runtime_error0("Call with invalid argcnt\n");
}

NOINLINE void *SCM_LOAD_CLOSURE_PTR_FAIL(gc_obj a) {
  scm_runtime_error1("Attempting to call non-closure:", a);
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

static gc_obj tag_bignum(mpz_t a) {
  // Simplify to fixnum if possible.
  if (mpz_fits_slong_p(a)) {
    auto sn = mpz_get_si(a);
    if (((sn << 3) >> 3) == sn) {
      return tag_fixnum(sn);
    }
  }
  bignum_s *res = gc_alloc(sizeof(bignum_s));
  res->type = BIGNUM_TAG;
  mpz_init_set(res->x, a);
  return tag_ptr(res);
}

gc_obj SCM_DIV(gc_obj a, gc_obj b);
gc_obj SCM_MUL(gc_obj a, gc_obj b);
gc_obj SCM_MOD(gc_obj a, gc_obj b);
gc_obj SCM_QUOTIENT(gc_obj a, gc_obj b);
gc_obj SCM_LT(gc_obj a, gc_obj b);
gc_obj SCM_GT(gc_obj a, gc_obj b);
gc_obj SCM_ADD(gc_obj a, gc_obj b);
gc_obj SCM_SUB(gc_obj a, gc_obj b);

// Simplify ratnums, allocate and tag them.
static bool SCM_neg(gc_obj a) {
  return SCM_LT(a, tag_fixnum(0)).value == TRUE_REP.value;
}
static gc_obj SCM_abs(gc_obj a) {
  if (SCM_neg(a)) {
    return SCM_MUL(a, tag_fixnum(-1));
  }
  return a;
}
static gc_obj gcd(gc_obj a, gc_obj b) {
  if (b.value == 0) {
    return SCM_abs(a);
  }
  [[clang::musttail]] return gcd(b, SCM_MOD(a, b));
}
static gc_obj tag_ratnum(ratnum_s num) {
  gc_obj a = num.num;
  gc_obj b = num.denom;
  bool neg = SCM_neg(a) ^ SCM_neg(b);
  a = SCM_abs(a);
  b = SCM_abs(b);
  if (a.value == 0) {
    return a;
  }

  auto gcdv = gcd(a, b);
  if (gcdv.value != tag_fixnum(1).value) {
    a = SCM_QUOTIENT(a, gcdv);
    b = SCM_QUOTIENT(b, gcdv);
  }
  if (b.value == tag_fixnum(1).value) {
    if (neg) {
      return SCM_MUL(tag_fixnum(-1), a);
    }
    return a;
  }
  ratnum_s *res = gc_alloc(sizeof(ratnum_s));
  res->type = RATNUM_TAG;
  res->num = a;
  if (neg) {
    res->num = SCM_MUL(tag_fixnum(-1), a);
  }
  res->denom = b;
  return tag_ptr(res);
}

static ratnum_s ratnum_add(ratnum_s a, ratnum_s b) {
  gc_obj num = SCM_ADD(SCM_MUL(a.num, b.denom), SCM_MUL(b.num, a.denom));
  gc_obj denom = SCM_MUL(a.denom, b.denom);
  return (ratnum_s){RATNUM_TAG, num, denom};
}
static ratnum_s ratnum_sub(ratnum_s a, ratnum_s b) {
  gc_obj num = SCM_SUB(SCM_MUL(a.num, b.denom), SCM_MUL(b.num, a.denom));
  gc_obj denom = SCM_MUL(a.denom, b.denom);
  return (ratnum_s){RATNUM_TAG, num, denom};
}
static ratnum_s ratnum_mul(ratnum_s a, ratnum_s b) {
  gc_obj num = SCM_MUL(a.num, b.num);
  gc_obj denom = SCM_MUL(a.denom, b.denom);
  return (ratnum_s){RATNUM_TAG, num, denom};
}

static ratnum_s ratnum_div(ratnum_s a, ratnum_s b) {
  gc_obj num = SCM_MUL(a.num, b.denom);
  gc_obj denom = SCM_MUL(a.denom, b.num);
  return (ratnum_s){RATNUM_TAG, num, denom};
}
static int ratnum_cmp(ratnum_s a, ratnum_s b) {
  gc_obj left = SCM_MUL(a.num, b.denom);
  gc_obj right = SCM_MUL(b.num, a.denom);
  if (SCM_LT(left, right).value == TRUE_REP.value) {
    return -1;
  }
  if (SCM_GT(left, right).value == TRUE_REP.value) {
    return 1;
  }
  return 0;
}

gc_obj SCM_MAKE_RECTANGULAR(gc_obj real, gc_obj imag) {
  compnum_s *r = gc_alloc(sizeof(compnum_s));
  r->type = COMPNUM_TAG;
  r->real = real;
  r->imag = imag;
  return tag_ptr(r);
}

static gc_obj pow2(int64_t exponent) {
  gc_obj result = tag_fixnum(1);
  gc_obj base = tag_fixnum(2);
  while (exponent > 0) {
    if (exponent % 2 == 1) {
      result = SCM_MUL(result, base);
    }
    base = SCM_MUL(base, base);
    exponent /= 2;
  }
  return result;
}

static gc_obj flonum_ratnum(double x) {
  uint64_t bits;
  memcpy(&bits, &x, sizeof(bits));
  bool sign = bits >> 63;
  int64_t exponent = (int64_t)(bits >> 52 & 0x7ff);
  int64_t mantissa = (int64_t)(bits & 0xFFFFFFFFFFFFF);
  if (exponent != 0) {
    mantissa += 0x10000000000000;
  }
  gc_obj denom = tag_fixnum(0x10000000000000);
  exponent -= 1023;
  if (sign) {
    mantissa *= -1;
  }
  auto numerator = tag_fixnum(mantissa);
  if (exponent < 0) {
    exponent *= -1;
    denom = SCM_MUL(denom, pow2(exponent));
  } else {
    numerator = SCM_MUL(numerator, pow2(exponent));
  }

  return tag_ratnum((ratnum_s){RATNUM_TAG, numerator, denom});
}

gc_obj SCM_EXACT(gc_obj flo) {
  if (is_compnum(flo)) {
    auto c = to_compnum(flo);
    return SCM_MAKE_RECTANGULAR(SCM_EXACT(c->real), SCM_EXACT(c->imag));
  }
  if (!is_flonum(flo)) {
    return flo;
  }
  assert(is_flonum(flo));
  return flonum_ratnum(to_double(flo));
}

gc_obj SCM_INEXACT(gc_obj fix) {
  if (is_compnum(fix)) {
    auto c = to_compnum(fix);
    return SCM_MAKE_RECTANGULAR(SCM_INEXACT(c->real), SCM_INEXACT(c->imag));
  }
  if (is_flonum(fix)) {
    return fix;
  }
  if (is_fixnum(fix)) {
    return double_to_gc_slow((double)to_fixnum(fix));
  }
  if (is_ratnum(fix)) {
    auto r = to_ratnum(fix);
    return SCM_QUOTIENT(SCM_INEXACT(r->num), SCM_INEXACT(r->denom));
  }
  if (is_bignum(fix)) {
    return double_to_gc_slow(mpz_get_d(to_bignum(fix)->x));
  }
  scm_runtime_error1("Not a number:", fix);
}

static void get_bignum(gc_obj obj, mpz_t *loc) {
  if (is_bignum(obj)) {
    mpz_init_set(*loc, to_bignum(obj)->x);
    return;
  }
  if (is_fixnum(obj)) {
    mpz_init_set_si(*loc, to_fixnum(obj));
    return;
  }
  scm_runtime_error1("Not a number: ", obj);
}

gc_obj SCM_NUMERATOR(gc_obj obj) { return to_ratnum(obj)->num; }
gc_obj SCM_DENOMINATOR(gc_obj obj) { return to_ratnum(obj)->denom; }

static ratnum_s get_ratnum(gc_obj obj) {
  if (is_ratnum(obj)) {
    auto r = to_ratnum(obj);
    return *r;
  }
  if (is_bignum(obj)) {
    return (ratnum_s){RATNUM_TAG, obj, tag_fixnum(1)};
  }
  if (is_fixnum(obj)) {
    return (ratnum_s){RATNUM_TAG, obj, tag_fixnum(1)};
  }
  scm_runtime_error1("Not a number:", obj);
}

static gc_obj get_compnum(gc_obj num) {
  if (is_compnum(num)) {
    return num;
  }
  return SCM_MAKE_RECTANGULAR(num, tag_fixnum(0));
}

gc_obj SCM_REAL_PART(gc_obj comp) {
  auto r = to_compnum(get_compnum(comp));
  return r->real;
}
gc_obj SCM_IMAG_PART(gc_obj comp) {
  auto r = to_compnum(get_compnum(comp));
  return r->imag;
}

static gc_obj compnum_add(gc_obj a, gc_obj b) {
  auto ca = to_compnum(get_compnum(a));
  auto cb = to_compnum(get_compnum(b));
  return SCM_MAKE_RECTANGULAR(SCM_ADD(ca->real, cb->real),
                              SCM_ADD(ca->imag, cb->imag));
}
static gc_obj compnum_sub(gc_obj a, gc_obj b) {
  auto ca = to_compnum(get_compnum(a));
  auto cb = to_compnum(get_compnum(b));
  return SCM_MAKE_RECTANGULAR(SCM_SUB(ca->real, cb->real),
                              SCM_SUB(ca->imag, cb->imag));
}
static gc_obj compnum_mul(gc_obj a, gc_obj b) {
  auto ca = to_compnum(get_compnum(a)); // a b
  auto cb = to_compnum(get_compnum(b)); // c d
  // ac - bd, ad + bc
  return SCM_MAKE_RECTANGULAR(
      SCM_SUB(SCM_MUL(ca->real, cb->real), SCM_MUL(ca->imag, cb->imag)),
      SCM_ADD(SCM_MUL(ca->real, cb->imag), SCM_MUL(ca->imag, cb->real)));
}

#define MATH_OVERFLOW_OP(OPNAME, OPLCNAME, OP, SHIFT)                          \
  NOINLINE __attribute__((preserve_most)) gc_obj SCM_##OPNAME##_SLOW(          \
      gc_obj a, gc_obj b) {                                                    \
    if (is_compnum(a) || is_compnum(b)) {                                      \
      return compnum_##OPLCNAME(a, b);                                         \
    }                                                                          \
    if (is_flonum(a) || is_flonum(b)) {                                        \
      return double_to_gc_slow(                                                \
          OP(to_double(SCM_INEXACT(a)), to_double(SCM_INEXACT(b))));           \
    }                                                                          \
    if (is_ratnum(a) || is_ratnum(b)) {                                        \
      ratnum_s ba = get_ratnum(a);                                             \
      ratnum_s bb = get_ratnum(b);                                             \
      return tag_ratnum(ratnum_##OPLCNAME(ba, bb));                            \
    }                                                                          \
    if (is_bignum(a) || is_bignum(b)) {                                        \
      mpz_t ba;                                                                \
      mpz_t bb;                                                                \
      mpz_t res;                                                               \
      mpz_init(res);                                                           \
      get_bignum(a, &ba);                                                      \
      get_bignum(b, &bb);                                                      \
      mpz_##OPLCNAME(res, ba, bb);                                             \
      return tag_bignum(res);                                                  \
    }                                                                          \
    if (is_fixnum(a) && is_fixnum(b)) {                                        \
      gc_obj res;                                                              \
      if (!__builtin_##OPLCNAME##_overflow(a.value, SHIFT(b.value),            \
                                           &res.value)) {                      \
        return res;                                                            \
      }                                                                        \
      /* make it a bignum, result overflowed */                                \
      mpz_t ba;                                                                \
      mpz_t bb;                                                                \
      mpz_t bres;                                                              \
      mpz_init(bres);                                                          \
      get_bignum(a, &ba);                                                      \
      get_bignum(b, &bb);                                                      \
      mpz_##OPLCNAME(bres, ba, bb);                                            \
      return tag_bignum(bres);                                                 \
    }                                                                          \
    scm_runtime_error1(#OPNAME ": not a number:", a);                          \
  }                                                                            \
                                                                               \
  INLINE gc_obj SCM_##OPNAME(gc_obj a, gc_obj b) {                             \
    if (likely((is_fixnum(a) & is_fixnum(b)) == 1)) {                          \
      gc_obj res;                                                              \
      if (likely(!__builtin_##OPLCNAME##_overflow(a.value, SHIFT(b.value),     \
                                                  &res.value))) {              \
        return res;                                                            \
      }                                                                        \
      return SCM_##OPNAME##_SLOW(a, b);                                        \
    }                                                                          \
    if (likely((is_flonum_fast(a) & is_flonum_fast(b)) == 1)) {                \
      gc_obj res;                                                              \
      if (likely(                                                              \
              double_to_gc(OP(to_double_fast(a), to_double_fast(b)), &res))) { \
        return res;                                                            \
      }                                                                        \
    }                                                                          \
    return SCM_##OPNAME##_SLOW(a, b);                                          \
  }

#define MATH_ADD(a, b) ((a) + (b))
#define MATH_SUB(a, b) ((a) - (b))
#define MATH_MUL(a, b) ((a) * (b))
#define NOSHIFT(a) (a)
#define SHIFT(a) ((a) >> 3)
MATH_OVERFLOW_OP(ADD, add, MATH_ADD, NOSHIFT)
MATH_OVERFLOW_OP(SUB, sub, MATH_SUB, NOSHIFT)
MATH_OVERFLOW_OP(MUL, mul, MATH_MUL, SHIFT)

#define MATH_SIMPLE_OP(OPNAME, OP, FPOP, BIGOP)                                \
                                                                               \
  NOINLINE gc_obj SCM_##OPNAME##_SLOW(gc_obj a, gc_obj b) {                    \
    if (is_compnum(a) || is_compnum(b)) {                                      \
      scm_runtime_error0("Compnums not supported for " #OPNAME);               \
    }                                                                          \
    if (is_flonum(a) || is_flonum(b)) {                                        \
      return double_to_gc_slow(                                                \
          FPOP(to_double(SCM_INEXACT(a)), to_double(SCM_INEXACT(b))));         \
    }                                                                          \
    if (is_bignum(a) || is_bignum(b)) {                                        \
      mpz_t ba;                                                                \
      mpz_t bb;                                                                \
      mpz_t res;                                                               \
      mpz_init(res);                                                           \
      get_bignum(a, &ba);                                                      \
      get_bignum(b, &bb);                                                      \
      mpz_tdiv_##BIGOP(res, ba, bb);                                           \
      return tag_bignum(res);                                                  \
    }                                                                          \
    if (is_fixnum(a) && is_fixnum(b)) {                                        \
      return tag_fixnum(OP(to_fixnum(a), to_fixnum(b)));                       \
    }                                                                          \
    scm_runtime_error0("Invalid types in " #OPNAME);                           \
  }                                                                            \
                                                                               \
  INLINE gc_obj SCM_##OPNAME(gc_obj a, gc_obj b) {                             \
    if (likely((is_fixnum(a) & is_fixnum(b)) == 1)) {                          \
      return tag_fixnum(OP(to_fixnum(a), to_fixnum(b)));                       \
    }                                                                          \
    if (likely((is_flonum_fast(a) & is_flonum_fast(b)) == 1)) {                \
      gc_obj res;                                                              \
      if (likely(double_to_gc(FPOP(to_double_fast(a), to_double_fast(b)),      \
                              &res))) {                                        \
        return res;                                                            \
      }                                                                        \
    }                                                                          \
    [[clang::musttail]] return SCM_##OPNAME##_SLOW(a, b);                      \
  }

#define MATH_DIV(a, b) ((a) / (b))
#define MATH_MOD(a, b) ((a) % (b))
#define MATH_FPMOD(a, b) (fmod((a), (b)))
MATH_SIMPLE_OP(QUOTIENT, MATH_DIV, MATH_DIV, q)
MATH_SIMPLE_OP(MOD, MATH_MOD, MATH_FPMOD, r)

NOINLINE gc_obj SCM_DIV_SLOW(gc_obj a, gc_obj b) {
  if (is_compnum(a) || is_compnum(b)) {
    scm_runtime_error0("DIV does not support compnum");
  }
  if (is_flonum(a) || is_flonum(b)) {
    return double_to_gc_slow(to_double(SCM_INEXACT(a)) /
                             to_double(SCM_INEXACT(b)));
  }
  ratnum_s ra = get_ratnum(a);
  ratnum_s rb = get_ratnum(b);
  return tag_ratnum(ratnum_div(ra, rb));
}

INLINE gc_obj SCM_DIV(gc_obj a, gc_obj b) {
  if (likely((is_flonum_fast(a) & is_flonum_fast(b)) == 1)) {
    gc_obj res;
    if (likely(double_to_gc(to_double_fast(a) / to_double_fast(b), &res))) {
      return res;
    }
  }
  [[clang::musttail]] return SCM_DIV_SLOW(a, b);
}

#define MATH_COMPARE_OP(OPNAME, OP, COMPCMP)                                   \
  NOINLINE __attribute__((preserve_most)) gc_obj SCM_##OPNAME##_SLOW(          \
      gc_obj a, gc_obj b) {                                                    \
    bool res;                                                                  \
    if (is_compnum(a) || is_compnum(b)) {                                      \
      res = COMPCMP(a, b);                                                     \
    } else if (is_flonum(a) || is_flonum(b)) {                                 \
      res = OP(to_double(SCM_INEXACT(a)), to_double(SCM_INEXACT(b)));          \
    } else if (is_ratnum(a) || is_ratnum(b)) {                                 \
      ratnum_s ba = get_ratnum(a);                                             \
      ratnum_s bb = get_ratnum(b);                                             \
      res = OP(ratnum_cmp(ba, bb), 0);                                         \
    } else if (is_bignum(a) || is_bignum(b)) {                                 \
      mpz_t ba;                                                                \
      mpz_t bb;                                                                \
      get_bignum(a, &ba);                                                      \
      get_bignum(b, &bb);                                                      \
      res = OP(mpz_cmp(ba, bb), 0);                                            \
    } else if (is_fixnum(a) && is_fixnum(b)) {                                 \
      res = OP(to_fixnum(a), to_fixnum(b));                                    \
    } else {                                                                   \
      scm_runtime_error0("Invalid type in " #OPNAME);                          \
    }                                                                          \
    if (res) {                                                                 \
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
static bool COMP_FAIL(gc_obj a, gc_obj b) {
  scm_runtime_error0("Not a real number!\n");
}

gc_obj SCM_NUM_EQ(gc_obj a, gc_obj b);
static bool COMP_CMP_EQ(gc_obj a, gc_obj b) {
  auto ca = to_compnum(get_compnum(a));
  auto cb = to_compnum(get_compnum(b));
  return SCM_NUM_EQ(ca->real, cb->real).value == TRUE_REP.value &&
         SCM_NUM_EQ(ca->imag, cb->imag).value == TRUE_REP.value;
}
MATH_COMPARE_OP(LT, MATH_LT, COMP_FAIL)
MATH_COMPARE_OP(LTE, MATH_LTE, COMP_FAIL)
MATH_COMPARE_OP(GT, MATH_GT, COMP_FAIL)
MATH_COMPARE_OP(GTE, MATH_GTE, COMP_FAIL)
MATH_COMPARE_OP(NUM_EQ, MATH_EQ, COMP_CMP_EQ)

gc_obj SCM_ISNAN(gc_obj obj) {
  auto f = to_double(obj);
  if (isnan(f)) {
    return TRUE_REP;
  }
  return FALSE_REP;
}
gc_obj SCM_ISINF(gc_obj obj) {
  auto f = to_double(obj);
  if (isinf(f)) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

INLINE gc_obj SCM_CAR(gc_obj obj) {
#ifndef UNSAFE
  if (!is_cons(obj)) {
    scm_runtime_error1("CAR: not a cons", obj);
  }
#endif
  return to_cons(obj)->a;
}

INLINE gc_obj SCM_CDR(gc_obj obj) {
#ifndef UNSAFE
  if (!is_cons(obj)) {
    scm_runtime_error1("CDR: not a cons", obj);
  }
#endif
  return to_cons(obj)->b;
}

INLINE gc_obj SCM_SETCAR(gc_obj obj, gc_obj val) {
#ifndef UNSAFE
  if (!is_cons(obj)) {
    scm_runtime_error1("setcar: not a cons:", obj);
  }
#endif
  auto c = to_cons(obj);
  c->a = val;
  //  printf("log setcar\n");
  if (has_ptr_tag(val)) {
    gc_log((uint64_t)&c->a);
  }
  return UNDEFINED;
}

INLINE gc_obj SCM_SETCDR(gc_obj obj, gc_obj val) {
#ifndef UNSAFE
  if (!is_cons(obj)) {
    scm_runtime_error1("setcdr: not a cons:", obj);
  }
#endif
  auto c = to_cons(obj);
  c->b = val;
  //  printf("log setcdr\n");
  if (has_ptr_tag(val)) {
    gc_log((uint64_t)&c->b);
  }
  return UNDEFINED;
}
INLINE gc_obj SCM_CONS(gc_obj a, gc_obj b) {
  cons_s *c = gc_alloc(sizeof(cons_s));
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
  vector_s *v = gc_alloc(sizeof(vector_s) + (to_fixnum(obj) * sizeof(gc_obj)));
  v->len = obj;
  return tag_vector(v);
}

INLINE gc_obj SCM_VECTOR_LENGTH(gc_obj vec) {
  if (unlikely(!is_vector(vec))) {
    scm_runtime_error1("vector-length: not a vector", vec);
  }
  return to_vector(vec)->len;
}

INLINE gc_obj SCM_VECTOR_REF(gc_obj vec, gc_obj idx) {
#ifndef UNSAFE
  if (unlikely(!is_fixnum(idx))) {
    scm_runtime_error1("vector-ref: not a valid idx", idx);
  }
  if (unlikely(!is_vector(vec))) {
    scm_runtime_error1("vector-ref: not a vector", vec);
  }
#endif
  auto v = to_vector(vec);
  auto i = to_fixnum(idx);
#ifndef UNSAFE
  if (unlikely(i >= to_fixnum(v->len))) {
    scm_runtime_error1("vector-ref: idx out of range", vec);
  }
#endif

  return v->v[i];
}

INLINE gc_obj SCM_VECTOR_SET(gc_obj vec, gc_obj idx, gc_obj val) {
#ifndef UNSAFE
  if (unlikely(!is_fixnum(idx))) {
    scm_runtime_error1("vector-set: not a valid idx", idx);
  }
  if (unlikely(!is_vector(vec))) {
    scm_runtime_error1("vector-set: not a vector", vec);
  }
#endif
  auto v = to_vector(vec);
  uint64_t i = to_fixnum(idx);
#ifndef UNSAFE
  if (unlikely(i >= (uint64_t)to_fixnum(v->len))) {
    scm_runtime_error1("vector-set: idx out of range", vec);
  }
#endif
  v->v[i] = val;

  if (has_ptr_tag(val)) {
    gc_log((uint64_t)&v->v[i]);
  }

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
  closure_s *clo = gc_alloc(sizeof(closure_s) + ((len + 1) * sizeof(gc_obj)));
  clo->type = CLOSURE_TAG;
  clo->v[0] = p;
  return tag_closure(clo);
}

INLINE void SCM_CLOSURE_SET(gc_obj clo, gc_obj obj, gc_obj idx) {
  //    printf("Closure set %li\n", i);
  // TODO fixme: why does closure_set_fast use direct idx, but set has fixnum?
  auto i = to_fixnum(idx);
  auto c = to_closure(clo);
  c->v[i + 1] = obj;
  /* printf("log closure\n"); */
  gc_log((uint64_t)&c->v[i + 1]);
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

extern char **environ;
void *get_stack_top() {
  // Ensure alignment, since we are resetting the stack to here.
  return (void *)((uintptr_t)gc_get_stack_top() & ~0x7);
}

ccsave *cur_link = nullptr;
static uint8_t tmpstack[100];
gc_obj ccresthunk(gc_obj unused, gc_obj n) {
  ccsave *c = (ccsave *)to_closure(unused);
  cur_link = c->prev_link;

  int64_t stack_bottom = (int64_t)(get_stack_top() - c->sz);
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
               : "r"(tmpstackalign), "r"(stack_bottom), "r"(saved_stack),
                 "r"(saved_sz), "r"(n)
               : "memory", "x19", "x20", "x0", "x1", "x2");
#else
#error "Arch not supported for CALLCC"
#endif
  __builtin_unreachable();
  return n;
}

#if defined(__x86_64__)
extern void need_more_frames();
#elif defined(__aarch64__)
static void need_more_frames(gc_obj res) {
  assert(cur_link);
  ccresthunk(tag_closure((closure_s *)cur_link), res);
}
#else
#error "Arch not supported for CALLCC"
#endif

void ccresthunk_oneshot();

__attribute__((returns_twice, noinline, preserve_none)) gc_obj
SCM_CALLCC(gc_obj cont) {
  if (!is_closure(cont)) {
    scm_runtime_error1("Not a continuation:", cont);
  }
  auto clo = to_closure(cont);

  void *stack_bottom = __builtin_frame_address(0);
  void *stacktop = get_stack_top();

  size_t stack_sz = stacktop - stack_bottom;
  ccsave *stack = gc_alloc(sizeof(ccsave) + stack_sz);
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

static const int64_t reg_arg_cnt = 6;
/* #if defined(__x86_64__) */
/* static const uint64_t reg_arg_cnt = 6; */
/* #elif defined(__aarch64__) */
/* static const uint64_t reg_arg_cnt = 8; */
/* #endif */

uint64_t shadow_stack_size = 0;
gc_obj *shadow_stack = nullptr;
///// Shadow stack

// TODO: Could split in fast/slowpath: only apply doesn't statically know
// the shadow stack size.
INLINE void SCM_WRITE_SHADOW_STACK(gc_obj pos, gc_obj obj) {
  while (unlikely((uint64_t)to_fixnum(pos) >= shadow_stack_size)) {
    for (uint64_t i = shadow_stack_size; i > 0; i--) {
      gc_pop_root((uint64_t *)&shadow_stack[i - 1]);
    }
    shadow_stack_size *= 2;
    if (shadow_stack_size == 0) {
      shadow_stack_size = 64;
    }
    auto new_shadow_stack =
        realloc(shadow_stack, shadow_stack_size * sizeof(gc_obj));
    if (!new_shadow_stack) {
      abort();
    }
    shadow_stack = new_shadow_stack;
    for (uint64_t i = 0; i < shadow_stack_size; i++) {
      gc_add_root((uint64_t *)&shadow_stack[i]);
    }
    assert(shadow_stack);
  }
  assert((uint64_t)to_fixnum(pos) < shadow_stack_size);
  shadow_stack[to_fixnum(pos)] = obj;
}

INLINE gc_obj SCM_READ_SHADOW_STACK(uint64_t pos) {
  assert(pos < shadow_stack_size);
  return shadow_stack[pos];
}

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
      SCM_WRITE_SHADOW_STACK(tag_fixnum(cur - reg_arg_cnt), head);
      // shadow_stack[cur - reg_arg_cnt] = head;
      return head;
    }
    res = SCM_CONS(SCM_READ_SHADOW_STACK(cur - reg_arg_cnt - 1), res);
    cur--;
  }
  *tail = res;
  return head;
}

INLINE gc_obj SCM_STRING_LENGTH(gc_obj obj) { return to_string(obj)->len; }

gc_obj SCM_STRING_CMP(gc_obj a, gc_obj b) {
  auto sa = to_string(a);
  auto sb = to_string(b);
  auto len = to_fixnum(sa->bytes);
  if (to_fixnum(sb->bytes) < len) {
    len = to_fixnum(sb->bytes);
  }
  auto res = strncmp(sa->strdata, sb->strdata, len);
  if (res == 0) {
    if (to_fixnum(sa->len) > to_fixnum(sb->len)) {
      return tag_fixnum(1);
    }
    if (to_fixnum(sa->len) < to_fixnum(sb->len)) {
      return tag_fixnum(-1);
    }
    return tag_fixnum(0);
  }
  return tag_fixnum(res);
}

INLINE gc_obj SCM_EQ(gc_obj a, gc_obj b) {
  if (a.value == b.value) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

gc_obj SCM_EQV_SLOW(gc_obj a, gc_obj b) {
  if (is_number(b)) {
    return SCM_NUM_EQ(a, b);
  }
  return FALSE_REP;
}

INLINE gc_obj SCM_EQV(gc_obj a, gc_obj b) {
  if (a.value == b.value) {
    return TRUE_REP;
  }
  // We already checked for fixnum and most flonums,
  // If ptr_tag low byte is NUMBER, do a more expensive test.
  //
  // Chez scheme has found checking only one of the comparison is
  // sufficient.
  if (unlikely(is_number(a))) {
    [[clang::musttail]] return SCM_EQV_SLOW(a, b);
  }
  return FALSE_REP;
}

static const int64_t kb = -20;
static const int64_t k0 = 200;
typedef struct {
  bool v;
  int64_t k;
} ep_result;
static ep_result ep(uf *ht, bool unused, gc_obj a, gc_obj b, int64_t k);
static ep_result equalp_interleave(uf *ht, bool fast, gc_obj a, gc_obj b,
                                   int64_t k) {
  // eq?
  if (a.value == b.value) {
    return (ep_result){true, k};
  }

  // Check cons, vector, string for equalp?
  // cons and vector check unionfind table for cycles.
  if (is_cons(a)) {
    if (is_cons(b)) {
      auto cell_a = to_cons(a);
      auto cell_b = to_cons(b);
      if (!fast && unionfind(ht, a.value, b.value)) {
        return (ep_result){true, 0};
      }
      // Decrement k once
      auto res = ep(ht, fast, cell_a->a, cell_b->a, k - 1);
      if (!res.v) {
        return res;
      }
      // And pass k through.
      [[clang::musttail]] return ep(ht, fast, cell_a->b, cell_b->b, res.k);
    }
    return (ep_result){false, k};
  }
  if (is_vector(a) && is_vector(b)) {
    auto va = to_vector(a);
    auto vb = to_vector(b);
    if (va->len.value != vb->len.value) {
      return (ep_result){false, k};
    }
    if (!fast && unionfind(ht, a.value, b.value)) {
      return (ep_result){true, 0};
    }
    // Decrement K once for the vector, but return same K value
    uint64_t lim = to_fixnum(va->len);
    for (uint64_t i = 0; i < lim; i++) {
      auto res = ep(ht, fast, va->v[i], vb->v[i], k - 1);
      if (!res.v) {
        return res;
      }
    }
    return (ep_result){true, k};
  }
  // string=?
  if (is_string(a)) {
    if (is_string(b)) {
      if (SCM_STRING_CMP(a, b).value == 0) {
        return (ep_result){true, k};
      }
    }
    return (ep_result){false, k};
  }
  // bytevector=?
  if (is_bytevector(a)) {
    if (is_bytevector(b)) {
      auto bva = to_bytevector(a);
      auto bvb = to_bytevector(b);
      if (bva->len.value != bvb->len.value) {
        return (ep_result){false, k};
      }
      if (memcmp(bva->v, bvb->v, to_fixnum(bva->len)) == 0) {
        return (ep_result){true, k};
      }
    }
    return (ep_result){false, k};
  }
  // eqvp?
  if (SCM_EQV(a, b).value == TRUE_REP.value) {
    return (ep_result){true, k};
  }
  return (ep_result){false, k};
}

static ep_result ep(uf *ht, bool unused, gc_obj a, gc_obj b, int64_t k) {
  if (k <= 0) {
    if (k == kb) {
      [[clang::musttail]] return equalp_interleave(ht, true, a, b, k0 * 2);
    } else {
      [[clang::musttail]] return equalp_interleave(ht, false, a, b, k);
    }
  } else {
    [[clang::musttail]] return equalp_interleave(ht, true, a, b, k);
  }
}

gc_obj SCM_EQUAL(gc_obj a, gc_obj b) {
  uf ht;
  uf_init(&ht);
  int64_t k = k0;

  ep_result res = ep(&ht, true, a, b, k);

  uf_free(&ht);
  if (res.v) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

gc_obj SCM_MAKE_STRING(gc_obj len, gc_obj fill) {
  // Align.
  auto strlen = align(to_fixnum(len), 8);
  uint8_t buf[4];
  auto ch = to_char(fill);
  long bytecnt = 1;
  if (ch >= 128) {
    bytecnt = utf8proc_encode_char(to_char(fill), buf);
  } else {
    buf[0] = ch;
  }
  auto totallen = strlen * bytecnt;
  // Note: Allocing data here first, so that modifications to
  // str don't require gc_log (since the second gc_alloc
  // won't promote str from young to old gen).
  auto data = gc_alloc(totallen);
  string_s *str = gc_alloc(sizeof(string_s));
  str->strdata = data;
  str->type = STRING_TAG;
  str->len = len;
  str->bytes = tag_fixnum(bytecnt * to_fixnum(len));
  if (fill.value != FALSE_REP.value) {
    if (bytecnt == 1) {
      memset(str->strdata, to_char(fill), to_fixnum(len));
    } else {
      for (uint64_t i = 0; i < totallen; i += bytecnt) {
        memcpy(&str->strdata[i], buf, bytecnt);
      }
    }
  } else {
    memset(str->strdata, 0, bytecnt * to_fixnum(len));
  }
  return tag_string(str);
}

INLINE gc_obj SCM_CHAR_INTEGER(gc_obj ch) { return tag_fixnum(to_char(ch)); }

INLINE gc_obj SCM_INTEGER_CHAR(gc_obj i) { return tag_char(to_fixnum(i)); }

INLINE gc_obj SCM_SYMBOL_STRING(gc_obj sym) {
  if (!is_symbol(sym)) {
    scm_runtime_error1("symbol->string: not a symbol", sym);
  }
  return to_symbol(sym)->name;
}

INLINE gc_obj SCM_STRING_REF(gc_obj str, gc_obj pos) {
#ifndef UNSAFE
  if (unlikely(!is_string(str))) {
    scm_runtime_error1("string-set: not a string", str);
  }
  if (unlikely(!is_fixnum(pos))) {
    scm_runtime_error1("string-set: not a valid idx", pos);
  }
#endif
  auto s = to_string(str);
  uint64_t i = to_fixnum(pos);
#ifndef UNSAFE
  if (unlikely(i >= (uint64_t)to_fixnum(s->len))) {
    scm_runtime_error1("string-set: idx out of range", pos);
  }
#endif
  if (unlikely(s->len.value != s->bytes.value)) {
    // Slow, utf8-path.
    int32_t codepoint;
    uint32_t bytepos = 0;
    for (uint64_t ch = 0; ch <= i; ch++) {
      auto res = utf8proc_iterate((const unsigned char *)&s->strdata[bytepos],
                                  to_fixnum(s->bytes), &codepoint);
      if (res < 0) {
        scm_runtime_error0("Tried to read invalid utf8");
      }
      bytepos += res;
    }
    return tag_char(codepoint);
  }
  return tag_char(s->strdata[i]);
}

INLINE gc_obj SCM_STRING_REF_FAST(gc_obj str, gc_obj pos) {
  auto s = to_string(str);
  uint64_t i = to_fixnum(pos);
  if (unlikely(s->len.value != s->bytes.value)) {
    // Slow, utf8-path.
    int32_t codepoint;
    uint32_t bytepos = 0;
    for (uint64_t ch = 0; ch <= i; ch++) {
      auto res = utf8proc_iterate((const unsigned char *)&s->strdata[bytepos],
                                  to_fixnum(s->bytes), &codepoint);
      if (res < 0) {
        scm_runtime_error0("Tried to read invalid utf8");
      }
      bytepos += res;
    }
    return tag_char(codepoint);
  }
  return tag_char(s->strdata[i]);
}

NOINLINE gc_obj SCM_STRING_SET_SLOW(gc_obj str, gc_obj pos, gc_obj scm_ch) {
  auto s = to_string(str);
  auto i = to_fixnum(pos);
  auto c = to_char(scm_ch);
  uint8_t buf[4];
  auto bytecnt = utf8proc_encode_char(c, buf);
  assert(bytecnt != 0);

  int32_t codepoint;
  uint32_t bytepos = 0;
  ssize_t res;
  for (int64_t ch = 0; ch <= i; ch++) {
    res = utf8proc_iterate((const unsigned char *)&s->strdata[bytepos],
                           to_fixnum(s->bytes), &codepoint);
    if (res < 0) {
      scm_runtime_error0("Tried to read invalid utf8");
    }
    if (ch == i) {
      break;
    }
    bytepos += res;
  }
  if (res != bytecnt) {
    auto old_data = s->strdata;
    auto new_bytes = to_fixnum(s->bytes) + bytecnt - res;
    auto new_bytes_aligned = align(new_bytes, 8);
    s->strdata = gc_alloc(new_bytes_aligned);
    gc_log((uint64_t)&s->strdata);
    memcpy(s->strdata, old_data, bytepos);
    memcpy(&s->strdata[bytepos], buf, bytecnt);
    memcpy(&s->strdata[bytepos + bytecnt], &old_data[bytepos + res],
           to_fixnum(s->bytes) - bytepos - res);
    s->bytes = tag_fixnum(new_bytes);
    return UNDEFINED;
  }
  memcpy(&s->strdata[bytepos], buf, bytecnt);
  return UNDEFINED;
}
INLINE gc_obj SCM_STRING_SET(gc_obj str, gc_obj pos, gc_obj scm_ch) {
#ifndef UNSAVE
  if (unlikely(!is_string(str))) {
    scm_runtime_error1("string-set: not a string", str);
  }
  if (unlikely(!is_fixnum(pos))) {
    scm_runtime_error1("string-set: not a valid idx", pos);
  }
  if (unlikely(!is_char(scm_ch))) {
    scm_runtime_error1("string-set: not a char", scm_ch);
  }
#endif
  auto s = to_string(str);
  uint64_t i = to_fixnum(pos);
  uint32_t c = to_char(scm_ch);
#ifndef UNSAFE
  if (unlikely(i >= (uint64_t)to_fixnum(s->len))) {
    scm_runtime_error1("string-set: idx out of range", pos);
  }
#endif
  if (unlikely(s->len.value != s->bytes.value || c >= 128)) {
    [[clang::musttail]] return SCM_STRING_SET_SLOW(str, pos, scm_ch);
  }
  s->strdata[i] = (char)c;
  return UNDEFINED;
}
INLINE gc_obj SCM_STRING_SET_FAST(gc_obj str, gc_obj pos, gc_obj scm_ch) {
  auto s = to_string(str);
  uint64_t i = to_fixnum(pos);
  uint32_t c = to_char(scm_ch);
  if (unlikely(s->len.value != s->bytes.value || c >= 128)) {
    [[clang::musttail]] return SCM_STRING_SET(str, pos, scm_ch);
  }
  s->strdata[i] = (char)c;
  return UNDEFINED;
}

INLINE gc_obj SCM_MAKE_SYMBOL(gc_obj str) {
  symbol *sym = gc_alloc(sizeof(symbol));
  sym->type = SYMBOL_TAG;
  sym->name = str;
  sym->val = UNDEFINED;
  return tag_symbol(sym);
}

////// records
INLINE gc_obj SCM_MAKE_RECORD(gc_obj sz) {
  record_s *r = gc_alloc(sizeof(record_s) + (to_fixnum(sz) * sizeof(gc_obj)));
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

  if (has_ptr_tag(val)) {
    gc_log((uint64_t)&rec->v[i]);
  }
  return UNDEFINED;
}

INLINE gc_obj SCM_RECORD_SET_FAST(gc_obj r, gc_obj idx, gc_obj val) {
  auto rec = to_record(r);
  auto i = to_fixnum(idx);
  rec->v[i] = val;

  return UNDEFINED;
}

////////// Symbol table
extern gc_obj symbol_table;
INLINE gc_obj SCM_GET_SYM_TABLE() { return symbol_table; }

///////math
gc_obj SCM_SIN(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(sin(d));
}

gc_obj SCM_COS(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(cos(d));
}

gc_obj SCM_ASIN(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(asin(d));
}

gc_obj SCM_ACOS(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(acos(d));
}

gc_obj SCM_TAN(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(tan(d));
}

gc_obj SCM_ATAN(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(atan(d));
}

gc_obj SCM_SQRT(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(sqrt(d));
}

gc_obj SCM_ROUND(gc_obj f) {
  auto x = to_double(f);
  double rounded = round(x);
  if (fabs(x - rounded) == 0.5) {
    if (fmod(rounded, 2.0) != 0.0) {
      rounded = rounded + (x > 0 ? -1 : 1);
    }
  }
  return double_to_gc_slow(rounded);
}

gc_obj SCM_FLOOR(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(floor(d));
}

gc_obj SCM_CEILING(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(ceil(d));
}

gc_obj SCM_EXP(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(exp(d));
}

gc_obj SCM_LOG(gc_obj f) {
  double d = to_double(f);
  return double_to_gc_slow(log(d));
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
  auto hash = stringhash(str->strdata, to_fixnum(str->bytes));
  return tag_fixnum((int)hash);
}

gc_obj SCM_STRING_CPY(gc_obj tostr, gc_obj tostart, gc_obj fromstr,
                      gc_obj fromstart, gc_obj fromend) {
  auto to = to_string(tostr);
  auto from = to_string(fromstr);

  uint64_t from_pos = to_fixnum(fromstart);
  auto len = to_fixnum(fromend) - from_pos;
  // If either string  is non-ascii, run slowpath.
  if (from->bytes.value != from->len.value ||
      to->bytes.value != to->len.value) {
    // UTF8.  Re-calculate fromstart/fromend ... slowly.
    int64_t pos = 0;
    uint64_t bytepos = 0;
    int32_t codepoint;
    while (pos < to_fixnum(fromend)) {
      if (pos == to_fixnum(fromstart)) {
        from_pos = bytepos;
      }
      auto res =
          utf8proc_iterate((const unsigned char *)&from->strdata[bytepos],
                           to_fixnum(from->bytes), &codepoint);
      if (res < 0) {
        scm_runtime_error0("Tried to read invalid utf8");
      }
      pos++;
      bytepos += res;
    }
    len = bytepos - from_pos;
    // Conservatively allocate a large new string
    auto new_bytes = to_fixnum(to->bytes) + len;
    auto new_bytes_aligned = align(new_bytes, 8);
    auto new_strdata = gc_alloc(new_bytes_aligned);
    // Now iterate dst string, copying to new.
    pos = 0;
    bytepos = 0;
    while (pos < to_fixnum(tostart)) {
      auto res = utf8proc_iterate((const unsigned char *)&to->strdata[bytepos],
                                  to_fixnum(to->bytes), &codepoint);
      if (res < 0) {
        scm_runtime_error0("Tried to read invalid utf8");
      }
      pos++;
      bytepos += res;
    }
    // Ok, we are at the start.  Copy in old start
    memcpy(new_strdata, to->strdata, bytepos);
    // Copy in new data.
    memcpy(&new_strdata[bytepos], &from->strdata[from_pos], len);
    uint64_t newend = bytepos + len;
    // Advance past dst
    auto len_in_chars = to_fixnum(fromend) - to_fixnum(fromstart);
    while (pos < (to_fixnum(tostart) + len_in_chars)) {
      auto res = utf8proc_iterate((const unsigned char *)&to->strdata[bytepos],
                                  to_fixnum(to->bytes), &codepoint);
      if (res < 0) {
        scm_runtime_error0("Tried to read invalid utf8");
      }
      pos++;
      bytepos += res;
    }
    // Now copy in the tail.
    memcpy(&new_strdata[newend], &to->strdata[bytepos],
           to_fixnum(to->bytes) - bytepos);
    to->strdata = new_strdata;
    gc_log((uint64_t)&to->strdata);
    to->bytes = tag_fixnum((int64_t)(newend + to_fixnum(to->bytes) - bytepos));

    return UNDEFINED;
  }
  auto start = to_fixnum(tostart);
  memcpy(&to->strdata[start], &from->strdata[from_pos], len);
  return UNDEFINED;
}
INLINE gc_obj SCM_AND(gc_obj num, gc_obj mask) {
  return (gc_obj){.value = num.value & mask.value};
}

////////////// IO

// Null terminate a scheme string (by copying)
static char *to_c_str(gc_obj scm_str) {
  auto str = to_string(scm_str);
  char *res = gc_alloc(align(to_fixnum(str->bytes) + 1, 8));
  memcpy(res, str->strdata, to_fixnum(str->bytes));
  res[to_fixnum(str->bytes)] = 0;
  return res;
}

gc_obj SCM_OPEN_FD(gc_obj filename, gc_obj input) {

  auto name = to_c_str(filename);
  auto readonly = input.value == TRUE_REP.value;
  return tag_fixnum(
      open(name, readonly ? O_RDONLY : O_WRONLY | O_CREAT | O_TRUNC, 0777));
}

static uint64_t utf8_count_bytes(char *buf, uint64_t len) {
  uint64_t bytes = 0;
  int32_t codepoint_res;
  for (uint64_t codepoint = 0; codepoint < len; codepoint++) {
    auto res = utf8proc_iterate((unsigned char *)&buf[bytes], (long)(len * 4),
                                &codepoint_res);
    bytes += res;
  }
  return bytes;
}

static bool is_ascii(uint8_t *data, uint32_t bytes) {
  uint8_t *ptr = data;
  uint8_t *end = data + bytes;
  // Ensure we have 8 valid bytes for fastpath.
  uint8_t *end_sentinel = data + bytes - 7;
  while (ptr < end_sentinel) {
    uint64_t d;
    memcpy(&d, ptr, 8);
    if (d & 0x8080808080808080) {
      return false;
    }
    ptr += 8;
  }
  for (; ptr < end; ptr++) {
    if (*ptr & 0x80) {
      return false;
    }
  }
  return true;
}

static uint32_t count_utf8(uint8_t *data, uint32_t bytes,
                           bool abort_on_invalid) {
  if (likely(is_ascii(data, bytes))) {
    return bytes;
  }
  uint32_t bytepos = 0;
  int32_t codepoint;
  uint32_t chars = 0;
  while (bytepos < bytes) {
    auto res = utf8proc_iterate(&data[bytepos], bytes, &codepoint);
    if (res < 0) {
      if (abort_on_invalid) {
        scm_runtime_error0("Tried to read invalid utf8");
      } else {
        return chars;
      }
    }
    chars++;
    bytepos += res;
  }
  if (unlikely(bytepos != bytes && abort_on_invalid)) {
    scm_runtime_error0("Tried to read invalid utf8");
  }
  return chars;
}

gc_obj SCM_READ_FD(gc_obj scmfd, gc_obj scmbuf) {
  auto buf = to_string(scmbuf);

  // The buffer is unique: The codepoint count may not match
  // the byte count: Extra bytes are unfinished unicode codepoints.
  uint64_t remaining = 0;
  if (buf->bytes.value != buf->len.value) {
    auto bytecnt = utf8_count_bytes(buf->strdata, to_fixnum(buf->len));
    remaining = to_fixnum(buf->bytes) - bytecnt;
    memcpy(buf->strdata, &buf->strdata[bytecnt], remaining);
  }

  int fd = (int)to_fixnum(scmfd);
  auto res =
      read(fd, &buf->strdata[remaining], to_fixnum(buf->bytes) - remaining);
  if (res == 0) {
    assert(remaining == 0);
    return tag_fixnum(0);
  }
  if (res < 0) {
    printf("SCM_READ_FD error: %li\n", res);
    exit(-1);
  }
  buf->bytes = tag_fixnum((int64_t)(res + remaining));
  auto len = count_utf8((uint8_t *)buf->strdata, to_fixnum(buf->bytes), false);
  buf->len = tag_fixnum(len);

  return buf->len;
}

gc_obj SCM_WRITE_FD(gc_obj scmfd, gc_obj scmbuf) {
  int fd = (int)to_fixnum(scmfd);
  auto buf = to_bytevector(scmbuf);
  auto len = to_fixnum(buf->len);
  auto res = write(fd, buf->v, len);
  if (res != len) {
    printf("Could not write %li bytes to fd %i\n", len, fd);
    exit(-1);
  }
  return UNDEFINED;
}

gc_obj SCM_CLOSE_FD(gc_obj fd) {
  auto res = close((int)to_fixnum(fd));
  if (res != 0) {
    printf("Error closing fd %li, res %i, errno %i\n", to_fixnum(fd), res,
           errno);
    perror("foo");
    exit(-1);
  }
  return tag_fixnum(res);
}

gc_obj SCM_FILE_EXISTS(gc_obj scmname) {
  auto name = to_c_str(scmname);
  auto res = access(name, F_OK);
  if (res == 0) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

gc_obj SCM_DELETE_FILE(gc_obj scmname) {
  auto name = to_c_str(scmname);
  return tag_fixnum(unlink(name));
}
/////// FLONUMS
gc_obj SCM_BIGNUM_STR(gc_obj b) {
  auto bignum = to_bignum(b);
  // +2 per manual for null-termination and -
  auto len = mpz_sizeinbase(bignum->x, 10) + 2;
  // Align.
  len = align(len, 8);
  auto data = gc_alloc(len);
  string_s *str = gc_alloc(sizeof(string_s));
  str->strdata = data;
  mpz_get_str(str->strdata, 10, bignum->x);
  str->len = tag_fixnum((int64_t)strlen(str->strdata));
  str->bytes = tag_fixnum((int64_t)strlen(str->strdata));
  str->type = STRING_TAG;
  return tag_string(str);
}

gc_obj SCM_BIGNUM_SQRT(gc_obj b) {
  mpz_t res;
  mpz_init(res);
  mpz_sqrt(res, to_bignum(b)->x);
  return tag_bignum(res);
}

gc_obj SCM_FLONUM_STR(gc_obj b) {
  auto data = gc_alloc(40);
  string_s *str = gc_alloc(sizeof(string_s));
  str->strdata = data;
  str->type = STRING_TAG;
  double d = to_double(b);
  snprintf(str->strdata, 40 - 3, "%g", d);
  if (strpbrk(str->strdata, ".eE") == nullptr) {
    size_t len = strlen(str->strdata);
    str->strdata[len] = '.';
    str->strdata[len + 1] = '0';
    str->strdata[len + 2] = '\0';
  }
  str->len = tag_fixnum((int64_t)strlen(str->strdata));
  str->bytes = tag_fixnum((int64_t)strlen(str->strdata));

  return tag_string(str);
}

gc_obj SCM_DOUBLE_AS_U64(gc_obj b) {
  double d = to_double(b);
  int64_t di;
  memcpy(&di, &d, sizeof(di));
  mpz_t res;
  mpz_init_set_si(res, di);
  return tag_bignum(res);
}

// process-context

static int argc;
static char **argv;

static gc_obj from_c_str(char *str) {
  auto len = strlen(str);
  gc_obj res = SCM_MAKE_STRING(tag_fixnum((int64_t)len), FALSE_REP);

  memcpy(to_string(res)->strdata, str, len);
  return res;
}

gc_obj SCM_COMMAND_LINE() {
  gc_obj tail = NIL;
  for (int i = argc; i > 0; i--) {
    tail = SCM_CONS(from_c_str(argv[i - 1]), tail);
  }
  return tail;
}
gc_obj SCM_EXIT(gc_obj code) { exit((int)to_fixnum(code)); }

gc_obj SCM_GET_ENV_VARS() {
  gc_obj tail = NIL;

  char **p = environ;
  while (*p) {
    char *split = strchr(*p, '=');
    if (split) {
      long len = split - *p;
      gc_obj var = SCM_MAKE_STRING(tag_fixnum(len), FALSE_REP);
      string_s *s = to_string(var);
      for (int64_t i = 0; i < len; i++) {
        s->strdata[i] = (*p)[i];
      }

      gc_obj val = from_c_str(split + 1);
      tail = SCM_CONS(SCM_CONS(var, val), tail);
      p++;
    }
  }
  return tail;
}
//// time

gc_obj SCM_CURRENT_JIFFY() {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  int64_t us = (ts.tv_sec * 1000000) + (ts.tv_nsec / 1000);
  return tag_fixnum(us);
}

gc_obj SCM_CURRENT_SECOND() {
  struct timespec ts;
  clock_gettime(CLOCK_TAI, &ts);
  double offset = 37; // TODO should be based on leap seconds.
  double f = (double)ts.tv_sec + ((double)ts.tv_nsec / 1000000000.0) + offset;
  return double_to_gc_slow(f);
}

gc_obj SCM_SYSTEM(gc_obj strn) {
  auto str = to_string(strn);
  auto len = to_fixnum(str->len);
  auto align_len = align(len + 1, 8);
  char *tmp = gc_alloc(align_len);
  tmp[len] = '\0';
  strncpy(tmp, str->strdata, len);
  int res = system(tmp);
  return tag_fixnum(res);
}

gc_obj SCM_STRING_UTF8(gc_obj str) {
  if (unlikely(!is_string(str))) {
    scm_runtime_error1("string->utf8: not a string", str);
  }
  auto s = to_string(str);
  auto bytes_aligned = align(to_fixnum(s->bytes), 8);
  bytevector_s *utf8 = gc_alloc(sizeof(bytevector_s) + bytes_aligned);
  utf8->type = BYTEVECTOR_TAG;
  utf8->len = s->bytes;
  memcpy(utf8->v, s->strdata, to_fixnum(s->bytes));
  return tag_bytevector(utf8);
}

gc_obj SCM_UTF8_STRING(gc_obj scm_bv) {
  if (unlikely(!is_bytevector(scm_bv))) {
    scm_runtime_error1("utf8->string: not a bytevector", scm_bv);
  }
  auto bv = to_bytevector(scm_bv);
  auto bytes_aligned = align(to_fixnum(bv->len), 8);
  auto data = gc_alloc(bytes_aligned);
  string_s *str = gc_alloc(sizeof(string_s));
  str->type = STRING_TAG;
  str->bytes = bv->len;
  str->strdata = data;
  memcpy(str->strdata, bv->v, to_fixnum(bv->len));
  str->len = tag_fixnum(count_utf8(bv->v, to_fixnum(bv->len), true));
  return tag_string(str);
}

gc_obj SCM_BYTEVECTOR_REF(gc_obj scm_bv, gc_obj idx) {
  if (unlikely(!is_fixnum(idx))) {
    scm_runtime_error1("bytevector-ref: not a valid idx", idx);
  }
  if (unlikely(!is_bytevector(scm_bv))) {
    scm_runtime_error1("bytevector-ref: not a bytevector", scm_bv);
  }
  auto bv = to_bytevector(scm_bv);
  uint64_t i = to_fixnum(idx);
  if (unlikely(i >= (uint64_t)to_fixnum(bv->len))) {
    scm_runtime_error1("bytevector-ref: idx out of range", idx);
  }
  return tag_fixnum(bv->v[i]);
}
gc_obj SCM_BYTEVECTOR_SET(gc_obj scm_bv, gc_obj idx, gc_obj val) {
  if (unlikely(!is_fixnum(idx))) {
    scm_runtime_error1("bytevector-ref: not a valid idx", idx);
  }
  if (unlikely(!is_bytevector(scm_bv))) {
    scm_runtime_error1("bytevector-ref: not a bytevector", scm_bv);
  }
  auto bv = to_bytevector(scm_bv);
  uint64_t i = to_fixnum(idx);
  if (unlikely(i >= (uint64_t)to_fixnum(bv->len))) {
    scm_runtime_error1("bytevector-ref: idx out of range", idx);
  }
  if (unlikely(!is_fixnum(val))) {
    scm_runtime_error1("bytevector-ref: val not a u8", val);
  }
  uint64_t v = to_fixnum(val);
  if (unlikely(v >= 256)) {
    scm_runtime_error1("bytevector-ref: val not a u8", val);
  }
  bv->v[i] = v;
  return UNDEFINED;
}
gc_obj SCM_BYTEVECTOR_LENGTH(gc_obj scm_bv) {
  if (!is_bytevector(scm_bv)) {
    scm_runtime_error1("bytevector-length: not a bytevector", scm_bv);
  }
  auto bv = to_bytevector(scm_bv);
  return bv->len;
}
gc_obj SCM_MAKE_BYTEVECTOR(gc_obj scm_len, gc_obj init) {
  auto len = to_fixnum(scm_len);
  auto len_aligned = align(len, 8);
  bytevector_s *bv = gc_alloc(sizeof(bytevector_s) + len_aligned);
  bv->type = BYTEVECTOR_TAG;
  bv->len = scm_len;
  memset(bv->v, (uint8_t)to_fixnum(init), len);
  return tag_bytevector(bv);
}

//////// unicode char

gc_obj SCM_CHAR_ALPHABETIC(gc_obj ch) {
  utf8proc_category_t category = utf8proc_category(to_char(ch));
  if (category == UTF8PROC_CATEGORY_LU || // Uppercase letter
      category == UTF8PROC_CATEGORY_LL || // Lowercase letter
      category == UTF8PROC_CATEGORY_LT || // Titlecase letter
      category == UTF8PROC_CATEGORY_LM || // Modifier letter
      category == UTF8PROC_CATEGORY_LO) { // Other letter
    return TRUE_REP;
  }
  return FALSE_REP;
}

gc_obj SCM_CHAR_UPPERCASE(gc_obj ch) {
  if (utf8proc_isupper(to_char(ch))) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

gc_obj SCM_CHAR_LOWERCASE(gc_obj ch) {
  if (utf8proc_islower(to_char(ch))) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

gc_obj SCM_CHAR_NUMERIC(gc_obj ch) {
  utf8proc_category_t category = utf8proc_category(to_char(ch));
  if (category == UTF8PROC_CATEGORY_ND || category == UTF8PROC_CATEGORY_NL ||
      category == UTF8PROC_CATEGORY_NO) {
    return TRUE_REP;
  }
  return FALSE_REP;
}

gc_obj SCM_CHAR_WHITESPACE(gc_obj scm_ch) {
  auto ch = to_char(scm_ch);
  utf8proc_category_t category = utf8proc_category(ch);
  if (category == UTF8PROC_CATEGORY_ZS || category == UTF8PROC_CATEGORY_ZL ||
      category == UTF8PROC_CATEGORY_ZP || ch == 0x0009 || // Tab
      ch == 0x000A ||                                     // Line feed
      ch == 0x000B ||                                     // Vertical tab
      ch == 0x000C ||                                     // Form feed
      ch == 0x000D ||                                     // Carriage return
      ch == 0x0085 ||                                     // Next line (NEL)
      ch == 0x2028 ||                                     // Line separator
      ch == 0x2029) {                                     // Paragraph separator
    return TRUE_REP;
  }
  return FALSE_REP;
}

gc_obj SCM_UPCASE(gc_obj ch) { return tag_char(utf8proc_toupper(to_char(ch))); }
gc_obj SCM_DOWNCASE(gc_obj ch) {
  return tag_char(utf8proc_tolower(to_char(ch)));
}
gc_obj SCM_FOLDCASE(gc_obj ch) {
  return tag_char(utf8proc_tolower(utf8proc_toupper(to_char(ch))));
}

int SCM_MAIN();

extern uint64_t *complex_constants[];
extern uint64_t complex_constants_len;

static void *gmp_alloc_align(size_t sz) {
  return gc_alloc(align(sz, sizeof(void *)));
}
static void *gmp_realloc_align(void *p, size_t old_sz, size_t new_sz) {
  auto res = gmp_alloc_align(new_sz);
  auto copy_sz = old_sz;
  if (new_sz < old_sz) {
    copy_sz = new_sz;
  }
  memcpy(res, p, copy_sz);
  return res;
}

static void gmp_free(void *ptr, size_t sz) {}

int main(int argc_in, char *argv_in[]) {

  // grab the stack top: this is used for both callcc
  // stack replacement, *and* GC conservative collection.
  auto fp = __builtin_frame_address(0);
  // Save command line args for get-command-line
  argc = argc_in;
  argv = argv_in;

  auto lim = (struct rlimit){RLIM_INFINITY, RLIM_INFINITY};
  auto res = setrlimit(RLIMIT_STACK, &lim);
  if (res) {
    perror("Setrlimit error");
  }
  gc_init(fp);
  // Add the static GC roots.

  // Symbol table
  auto vec = to_vector(symbol_table);
  for (int64_t i = 0; i < to_fixnum(vec->len); i++) {
    auto sym = to_symbol(vec->v[i]);
    gc_add_root((uint64_t *)&sym->val);
  }
  // stack call/cc link.
  gc_add_root((uint64_t *)&cur_link);
  // static complex constants
  for (uint64_t i = 0; i < complex_constants_len; i++) {
    gc_add_root(complex_constants[i]);
  }

  // Set up memory functions for gmp
  mp_set_memory_functions(gmp_alloc_align, gmp_realloc_align, gmp_free);

  // Set up the .rodata write handler.
  setup_sigsegv_handler();

  SCM_MAIN();
}
