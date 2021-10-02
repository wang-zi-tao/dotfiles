#!/usr/bin/env python3
import random
import sys
def generate(length=8):
    return ''.join(random.choices('1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',k=length))

def main(argv):
    if len(argv)==2:
        password=generate(length=int(argv[1]))
    else:
        password=generate()
    print(password)

if __name__ == "__main__":
    main(sys.argv)
