name=${1%.*}
../callcc --exe $1 2>/dev/null > $name.ll
diff -Naur <(./$name) $name.out
