#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

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
#define TRUE_REP (gc_obj){.value = 0x0104}
#define FALSE_REP (gc_obj){.value = 0x0004}
#define EOF_OBJ                                                                \
  (gc_obj) { .value = EOF_TAG }

typedef struct {
  int64_t value;
} gc_obj;

typedef struct gc_header {
  union {
    struct {
      uint32_t type;
      uint32_t rc;
    };
    uint64_t fwdtag;
  };
  struct gc_header *fwd;
} gc_header;

typedef struct flonum_s {
  uint32_t type;
  uint32_t rc;
  double x;
} flonum_s;

typedef struct string_s {
  uint32_t type;
  uint32_t rc;
  gc_obj len;
  char str[];
} string_s;

typedef struct symbol {
  uint32_t type;
  uint32_t rc;
  gc_obj name; // string_s PTR_TAG'd value
  gc_obj val;
} symbol;

typedef struct vector_s {
  uint32_t type;
  uint32_t rc;
  gc_obj len;
  gc_obj v[];
} vector_s;

typedef struct cons_s {
  uint32_t type;
  uint32_t rc;
  gc_obj a;
  gc_obj b;
} cons_s;

typedef struct closure_s {
  uint32_t type;
  uint32_t rc;
  gc_obj len;
  gc_obj v[];
} closure_s;

#define TAG_SET ((1 <<4)|(1 <<3)|(1 <<0))
static bool has_tag_4_or_3_or_0 ( int64_t n ) {
  // Note that unlike the paper, we need to explictly ensure n is
  // masked to 5 bits: shifting by more than 32 bits here is undefined
  // behavior, and clang will happily optimize everything out.
  return ((( uint32_t )1 << (n&0x1f) ) & (~( uint32_t )0/0xff * TAG_SET )) != 0;
}

gc_obj double_to_gc(double d) {
  uint64_t di;
  memcpy(&di, &d, sizeof(d));
  di = __builtin_rotateleft64(di, 4);
  if (has_tag_4_or_3_or_0(di)) {
    di++; // Offset by one, so that we keep fixnum as 0 tag.
    return (gc_obj){.value = di};
  }
  abort();
}

bool is_ptr(gc_obj obj) {
  return (obj.value & TAG_MASK) == PTR_TAG;
}

double to_double(gc_obj obj) {
  if (is_ptr(obj)) {
    abort();
  }
  uint64_t r = obj.value - 1;
  assert(has_tag_4_or_3_or_0(r));
  r = __builtin_rotateright64(r, 4);
  double res;
  memcpy(&res, &r, sizeof(res));
  return res;
}

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
