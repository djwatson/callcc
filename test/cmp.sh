name=${1%.*}
case "$1" in
    "../test/r7rs-tests.scm" | "../test/argtype.scm")
	../bin/callcc --exe $1 2>/dev/null > $name
	;;
    *)
	../bin/callcc -fno-eval --exe $1 2>/dev/null > $name
	;;
esac    
diff -Naur <(./$name) $name.out
