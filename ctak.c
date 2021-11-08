#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

// TODO: Use a callcc start or a sentinel to get rid of if (tail)

typedef struct frame_s {
  int (*fp)(struct frame_s*, struct frame_s*, int, int, int);
  int state[5];
  int res;
  int x,y,z;
  struct frame_s* cont;
  
  int loc;
  struct frame_s* next;
} frame;

static frame* head = NULL;
static frame* tail = NULL;
static frame* prev_head = NULL;

static frame* frame_cache = NULL;

static int g_x, g_y, g_z;

static frame* get_frame() {
  if (frame_cache) {
    frame* f = frame_cache;
    frame_cache = f->next;
    f->next = NULL;
    return f;
  }

  frame* f = malloc(sizeof(frame));
  f->next = NULL;
  return f;
}

static void push_frame(frame* f) {
  f->next = frame_cache;
  frame_cache = f;
}


static int (*getsk)(struct frame_s*, struct frame_s*, int, int, int) = NULL;

static int callcc(  int (*fp)(struct frame_s*, struct frame_s*, int, int, int)) {
  assert(!getsk);
  getsk = fp;
  prev_head = head;
  head = NULL;
  return -1;
}


static int invoke(frame* k, int res) {
  assert(res >= 0);
  assert(k);
  assert(!tail);
  assert(!prev_head);
  head = k;
  head->res = res;
  return -2;
}

static int ctak(frame* k, frame* cont, int x, int y, int z);
static int v1(frame* new_cont, frame* a, int b , int c, int d) {
  return ctak(NULL, new_cont, g_x - 1, g_y, g_z);
}
static int v2(frame* new_cont, frame* a, int b , int c, int d) {
  return ctak(NULL, new_cont, g_y - 1, g_z, g_x);
}
static int v3(frame* new_cont, frame* a, int b , int c, int d) {
  return ctak(NULL, new_cont, g_z - 1, g_x, g_y);
}

static int ctakaux(frame* k, frame* cont, int x, int y, int z) {
  int res;
  int newx, newy, newz;
  
  if (k) {
    if (k->loc == 0) {
      res = k->res;
      goto loc0;
    }
    if (k->loc == 1) {
      res = k->res;
      newx = k->state[0];
      goto loc1;
    }
    if (k->loc == 2) {
      res = k->res;
      newx = k->state[0];
      newy = k->state[1];
      goto loc2;
    }
  }
  
  g_x = x;
  g_y = y;
  g_z = z;
  res = callcc(&v1);
  if (res == -1) {
    frame* f = get_frame();
    f->x = x;
    f->y = y;
    f->z = z;
    f->cont = cont;
    f->loc = 0;
    f->fp = &ctakaux;
    if (tail) {
      tail->next = f;
      tail = f;
    } else {
      tail = f;
      head = f;
    }
    return res;
  } else if (res == -2) {
    return res;
  } 
 loc0:
  newx = res;

  g_x = x;
  g_y = y;
  g_z = z;
  res = callcc(&v2);
  if (res == -1) {
    frame* f = get_frame();
    f->x = x;
    f->y = y;
    f->z = z;
    f->cont = cont;
    f->loc = 1;
    f->fp = &ctakaux;
    f->state[0] = newx;
    if (tail) {
      tail->next = f;
      tail = f;
    } else {
      tail = f;
      head = f;
    }
    return res;
  } else if (res == -2) {
    return res;
  } 
 loc1:
  newy = res;
  
  g_x = x;
  g_y = y;
  g_z = z;
  res = callcc(&v3);
  if (res == -1) {
    frame* f = get_frame();
    f->x = x;
    f->y = y;
    f->z = z;
    f->cont = cont;
    f->loc = 2;
    f->fp = &ctakaux;
    f->state[0] = newx;
    f->state[1] = newy;
    if (tail) {
      tail->next = f;
      tail = f;
    } else {
      tail = f;
      head = f;
    }
    return res;
  } else if (res == -2) {
    return res;
  } 
 loc2:
  newz = res;

  return ctak(NULL, cont, newx, newy, newz);
  
}

static int v4(frame* new_cont, frame* a, int b , int c, int d) {
  return  ctakaux(NULL, new_cont, g_x, g_y, g_z) ;
}

static int ctak(frame* k, frame* cont, int x, int y, int z) {
  if (!(y < x)) {
    return invoke(cont, z);
  }

  g_x = x;
  g_y = y;
  g_z = z;
  return callcc(&v4);
}

static int v5(frame* new_cont, frame* a, int b , int c, int d) {
      return ctak(NULL, new_cont, g_x, g_y, g_z);
}

static int ctakstart(frame* k, frame* cont, int x, int y, int z) {
  assert(!cont);
  if (k) {
    return k->res;
  }

  g_x = x;
  g_y = y;
  g_z = z;
  int res = callcc(&v5);
  if (res == -1) {
    frame* f = get_frame();
    f->x = x;
    f->y = y;
    f->z = z;
    f->cont = cont;
    f->fp = &ctakstart;
    if (tail) {
      tail->next = f;
      tail = f;
    } else {
      tail = f;
      head = f;
    }
    return res;
  } else if (res == -2) {
    return res;
  }

  return res;
}

static void runloop() {
  while(head || getsk) {
    int res;
    if (getsk) {
      int (*gk)(struct frame_s*, struct frame_s*, int, int, int) = getsk;
      getsk = NULL;
      res = gk(head, NULL, 0, 0, 0);
    } else {
      frame* cur = head;
      head = head->next;
      push_frame(cur);
      res = cur->fp(cur, cur->cont, cur->x, cur->y, cur->z);
    }
    if (res == -1) { // Get the continuation. Bottom out.
      assert(getsk);
	if (tail) {
	  tail->next = prev_head;
	} else {
	  head = prev_head;
	}
      assert(head);
      prev_head = NULL;
      tail = NULL;
    } else if (res == -2) { // invoke the continuation, nothing to do.
      
    } else {
      // Normal return: Either we have more frames, or we're done.
      if (head) {
	head->res = res;
      } else {
	printf("%i\n", res);
	return;
      }
    }
  }
}

static int t1(frame* new_cont, frame* a, int b , int c, int d) {
  return ctakstart(NULL, NULL, 9, 6, 3);
}
static int t2(frame* new_cont, frame* a, int b , int c, int d) {
  return ctakstart(NULL, NULL, 18, 12, 6);
}
static int t3(frame* new_cont, frame* a, int b , int c, int d) {
  return ctakstart(NULL, NULL, 29, 21, 9);
}

int main() {
  getsk = &t1;
  runloop();
  getsk = &t2;
  runloop();
  getsk = &t3;
  runloop();

  //6
  //7
  //18
  return 0;
}


