#!/usr/bin/env bash
if [[ -n $1 ]]; then
  dir=$1
else 
  dir=$PWD
fi
hash=$(echo $dir | sha256sum -)
repository="${HOME}/.build/${hash:1:63}-$(basename $dir)"
mkdir -p $repository
if [[ -e $dir/target ]];then
  rm -vr $dir/target
fi
ln -vs $repository $dir/target
