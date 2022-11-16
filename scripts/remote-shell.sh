#!/usr/bin/env bash
if [[ $HOSTNAME == "wangzi-nuc" ]]; then
	tmuxinator s "$1"
else
	ssh wangzi@192.168.16.12 -o ConnectTimeout=4 -X -Y -t "tmuxinator s $1" ||
		ssh wangzi@192.168.17.12 -o ConnectTimeout=4 -X -Y -t "tmuxinator s $1" ||
		ssh wangzi@192.168.31.220 -o ConnectTimeout=4 -X -Y -t "tmuxinator s $1" ||
		ssh wangzi@192.168.31.69 -o ConnectTimeout=4 -X -Y -t "tmuxinator s $1" ||
		ssh wangzi@192.168.32.1 -o ConnectTimeout=4 -X -Y -t "tmuxinator s $1" ||
		tmuxinator s "$1"
fi
