#!/usr/bin/env bash
if [[ $# != 1 ]]; then
  dirs=("$@")
else 
  dirs=($PWD)
fi
for dir in dirs
do
  hash=$(echo $dir | sha1sum -)
  repository="${HOME}/.build/${hash:1:63}-$(basename $(realpath $dir))"
  mkdir -p $repository
  if [[ -e $dir/target ]];then
    rm -vr $dir/target
  fi
  ln -vs $repository $dir/target
done
