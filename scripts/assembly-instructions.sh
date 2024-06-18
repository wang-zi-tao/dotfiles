#!/usr/bin/env bash
file=/tmp/wangzi/assembly-instructions.asm
object_file=/tmp/wangzi/a.out
mkdir -p /tmp/wangzi
echo $1>$file
as $file
objdump -d $object_file | grep '0:'
# rm -f $file $object_file
