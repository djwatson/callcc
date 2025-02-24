name=${1%.*}
cd ../lib; gosh -I. bc.gauche.scm ../test/$1 > ../test/$name.ll && cd ../test
clang -g -o $name $name.ll ../libcallcc.a -lm -lgmp 
diff -Naur <(./$name) $name.out
