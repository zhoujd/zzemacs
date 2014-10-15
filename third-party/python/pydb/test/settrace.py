#!/usr/bin/python
"""Towers of Hanoi"""
import sys, os

top_builddir = ".."
if top_builddir[-1] != os.path.sep:
    top_builddir += os.path.sep
sys.path.insert(0, os.path.join(top_builddir, 'pydb'))
top_srcdir = ".."
if top_srcdir[-1] != os.path.sep:
    top_srcdir += os.path.sep
sys.path.insert(0, os.path.join(top_srcdir, 'pydb'))

def hanoi(n,a,b,c):
    if n-1 > 0:
       hanoi(n-1, a, c, b) 
    print "Move disk %s to %s" % (a, b)
    if n-1 > 0:
       hanoi(n-1, c, b, a)

i_args=len(sys.argv)
if i_args > 3:
    print "usage %s [disks [cmdfile]]" % sys.argv[0]
    sys.exit(1)

n=3
if i_args > 1:
  try: 
    n = int(sys.argv[1])
  except ValueError, msg:
    print "** Expecting an integer, got: %s" % repr(sys.argv[1])
    sys.exit(2)

if n < 1 or n > 100: 
    print "*** number of disks should be between 1 and 100" 
    sys.exit(2)

dbg_cmds=['set basename on',
          'where',
          'list',
          'step',
          'step',
          'where',
          'info locals',
          'set linetrace on',
          'continue']

import pydb
pydb.debugger(dbg_cmds)

hanoi(n, "a", "b", "c")
