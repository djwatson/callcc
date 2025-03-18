name=${1%.*}
case "$1" in
    "../test/r7rs-tests.scm" | "../test/argtype.scm")
	../callcc --exe $1 2>/dev/null > $name.ll
	;;
    *)
	../callcc -fno-eval --exe $1 2>/dev/null > $name.ll
	;;
esac    
diff -Naur <(./$name) $name.out
