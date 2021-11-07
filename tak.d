import std.stdio;

int tak(int x, int y, int z) {
  if (!(y < x)) {
    return z;
  } else {
    return tak(tak(x - 1, y, z),
	       tak(y - 1, z, x),
	       tak(z - 1, x, y));
  }
}

void main() {
  writeln(tak(9,6,3));
  writeln(tak(18, 12,  6));
  writeln(tak(29, 21, 9));
}


