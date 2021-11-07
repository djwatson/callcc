all:ctak

ctak: ctak.d
	ldc2 -O5 -g ctak.d -release  -L=-E -fthread-model=local-exec --link-defaultlib-shared=0 -L=-lz

clean:
	rm -rf ctak
