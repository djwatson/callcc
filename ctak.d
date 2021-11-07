import std.stdio;

struct frame {
  int function(frame*, frame*, int, int, int) fp;
  int[5] state;
  int res;
  int x,y,z;
  frame* cont;
  
  int loc;
  frame* next = null;
};

__gshared frame* head = null;
__gshared frame* tail = null;
__gshared frame* prev_head = null;

__gshared int delegate(frame* k) getsk = null;

int callcc(int delegate(frame* k) fp) {
  assert(!getsk);
  getsk = fp;
  prev_head = head;
  head = null;
  return -1;
}


/* int takaux(int x, int y, int z) { */
/*   if (!(y < x)) { */
/*     return z; */
/*   } else { */
/*     return tak(tak(x - 1, y, z), */
/* 	       tak(y - 1, z, x), */
/* 	       tak(z - 1, x, y)); */
/*   } */
/* } */

int invoke(frame* k, int res) {
  assert(res >= 0);
  assert(k);
  assert(!tail);
  assert(!prev_head);
  head = k;
  head.res = res;
  return -2;
}

int ctakaux(frame* k, frame* cont, int x, int y, int z) {
  int res;
  int newx, newy, newz;
  
  if (k) {
    if (k.loc == 0) {
      res = k.res;
      goto loc0;
    }
    if (k.loc == 1) {
      res = k.res;
      newx = k.state[0];
      goto loc1;
    }
    if (k.loc == 2) {
      res = k.res;
      newx = k.state[0];
      newy = k.state[1];
      goto loc2;
    }
  }
  
  res = callcc((frame* new_cont) {return ctak(null, new_cont, x - 1, y, z); });
  if (res == -1) {
    frame* f = new frame;
    f.x = x;
    f.y = y;
    f.z = z;
    f.cont = cont;
    f.loc = 0;
    f.fp = &ctakaux;
    if (tail) {
      tail.next = f;
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

  res = callcc((frame* new_cont) {return ctak(null, new_cont, y - 1, z, x);});
  if (res == -1) {
    frame* f = new frame;
    f.x = x;
    f.y = y;
    f.z = z;
    f.cont = cont;
    f.loc = 1;
    f.fp = &ctakaux;
    f.state[0] = newx;
    if (tail) {
      tail.next = f;
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
  
  res = callcc((frame* new_cont) {return ctak(null, new_cont, z - 1, x, y);});
  if (res == -1) {
    frame* f = new frame;
    f.x = x;
    f.y = y;
    f.z = z;
    f.cont = cont;
    f.loc = 2;
    f.fp = &ctakaux;
    f.state[0] = newx;
    f.state[1] = newy;
    if (tail) {
      tail.next = f;
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

  return ctak(null, cont, newx, newy, newz);
  
}

int ctak(frame* k, frame* cont, int x, int y, int z) {
  if (!(y < x)) {
    //return invoke(cont, z);
    return z;
  }

  return callcc((frame* new_cont) {
      return ctakaux(null, new_cont, x, y, z);
    });
    
}

int ctakstart(frame* k, frame* cont, int x, int y, int z) {
  assert(!cont);
  if (k) {
    return k.res;
  }
  
  int res = callcc((frame* new_cont) {return ctak(null, new_cont, x, y, z);});
  if (res == -1) {
    frame* f = new frame;
    f.x = x;
    f.y = y;
    f.z = z;
    f.cont = cont;
    f.fp = &ctakstart;
    if (tail) {
      tail.next = f;
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

void runloop() {
  while(head || getsk) {
    int res;
    if (getsk) {
      auto gk = getsk;
      getsk = null;
      res = gk(head);
    } else {
      frame* cur = head;
      head = head.next;
      res = cur.fp(cur, cur.cont, cur.x, cur.y, cur.z);
    }
    if (res == -1) { // Get the continuation. Bottom out.
      assert(getsk);
      if (prev_head) {
	if (tail) {
	  tail.next = prev_head;
	} else {
	  head = prev_head;
	}
      }
      assert(head);
      prev_head = null;
      tail = null;
    } else if (res == -2) { // invoke the continuation, nothing to do.
      
    } else {
      // Normal return: Either we have more frames, or we're done.
      if (head) {
	head.res = res;
      } else {
	writeln(res);
	return;
      }
    }
  }
}

void main() {
  getsk = (frame* k) { return ctakstart(null, null, 9, 6, 3);};
  runloop();
  getsk = (frame* k) { return ctakstart(null, null, 18, 12, 6);};
  runloop();
  getsk = (frame* k) { return ctakstart(null, null, 29, 21, 9);};
  runloop();

  //6
  //7
  //18
}


