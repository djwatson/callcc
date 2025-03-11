name=${1%.*}
if [ "$1" = "../test/r7rs-tests.scm" ]; then
    ../callcc --exe $1 2>/dev/null > $name.ll
else
    ../callcc -fno-eval --exe $1 2>/dev/null > $name.ll
fi
diff -Naur <(./$name) $name.out
