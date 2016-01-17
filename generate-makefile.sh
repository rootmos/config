#!/bin/bash -x

classpath=$(cat .classpath | tr ':' ' ')
root=.megattags
mkdir -p $root
makefile=$root/Makefile
disassembled_targets=

log()
{
    echo "INFO:" $*
}

warning()
{
    >&2 echo "WARNING:" $*
}

error()
{
    >&2 echo "ERROR:" $*
    exit 3
}

handle_jar()
{
    jar=$1
    name=$(basename $jar)
    container=$name
    target=${name}.java
    disassembled_targets+=( "$target" )

    cat <<EOF >> $makefile
$target: $jar
	mkdir -p $container
	cd $container; jar xf $jar
	find $container -name "*.class" | xargs javap > \$@

EOF
}

rm -i $makefile
echo "CLASSPATH=$classpath" >> $makefile
echo "all: tags" >> $makefile

for path in $classpath; do
    if [ -f $path ]; then
        handle_jar $path
    elif [ -d $path ]; then
        waring "Don't know how to handle a directory $path"
    else
        error "Unexpected kind of path: $path"
    fi
done

echo >> $makefile
echo -n "tags:" >> $makefile
for target in "${disassembled_targets[@]}"; do
    echo -n " $target" >> $makefile
done
echo >> $makefile
cat <<EOF >> $makefile
	find -name "*.jar.java" | xargs ctags -f \$@ --fields=+l
EOF

