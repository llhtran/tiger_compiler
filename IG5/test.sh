#!/bin/bash
cat << EOF > temprun.sml
CM.make "sources.cm";
Main.compile "$1";
EOF
sml < temprun.sml
cat -n $1
echo "************************"
cat -n $1".s"
gcc -g -w -m32 $1".s" runtime.c
