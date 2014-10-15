#!/usr/bin/python -t
# $Id: except.py.in,v 1.4 2007/01/25 19:04:45 rockyb Exp $
"""Test to see if handles an exception when an error is raised."""
import os, sys

top_builddir = ".."
if top_builddir[-1] != os.path.sep:
    top_builddir += os.path.sep
sys.path.insert(0, os.path.join(top_builddir, 'pydb'))
top_srcdir = ".."
if top_srcdir[-1] != os.path.sep:
    top_srcdir += os.path.sep
sys.path.insert(0, os.path.join(top_srcdir, 'pydb'))

print __doc__

import pydb
pydb.debugger()  # we go into the debugger
print "Returning from the debugger"
# Set a variable so we can show the state is
# somewhere after the above set_trace()
z='After set_trace'
try:
    # The below statement should not enter the debugger
    x=2/0 
except:
    pass 
# We should enter the debugger on the next statement.
y=1/0  # Bullwinkle: This time, for sure!
pass




