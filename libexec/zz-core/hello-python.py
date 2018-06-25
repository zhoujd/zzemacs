#!/usr/bin/env python

## This a test for zz.py

def main():
    print "hello python!\n"

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        print "\nUser Press Ctrl+C, exit\n"
