all:ctak

ctak: ctak.c
	gcc -O3 -g ctak.c -o ctak

clean:
	rm -rf ctak
