#!/usr/bin/env python
#
# Copyright (C) 2006 Rocky Bernstein <rockyb@users.sourceforge.net>
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose with or without fee is hereby granted,
# provided that the above copyright notice and this permission notice
# appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND NOMINUM DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL NOMINUM BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
# OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

from optparse import OptionParser
import sys, os

def do_cmd(cmd):
    """Run a command and possibly print it out first.
If the command fails, we die!"""
    global opts
    if opts.verbose: print cmd
    exit_status = os.system(cmd)
    if exit_status != 0: sys.exit(exit_status)

print """Note: we don't do python-style install yet.
But as a service we'll try to transfer your call, but don't expect too much."""

optparser = OptionParser()
optparser.add_option("--prefix", "", dest="prefix", action="store", 
                help="--prefix option to pass to configure", 
                metavar='[prefix directory]')

optparser.add_option("--install-scripts", "", dest="bindir",
                     action="store",
                     help="--bindir opton to pass to configure", 
                     metavar='[executable directory in PATH]')

optparser.add_option("--verbose", "-v", dest="verbose",
                     action="store_true", default=False,
                     help="lame attempt at verbosity ")

(opts, args) = optparser.parse_args()

do_install = do_build = False
for arg in args:
    if arg=='install':
        do_build=True
        do_install=True
    if arg=='build':
        do_build=True

## Okay, now time to configure, make, make install
configure_cmd='./configure '
if opts.prefix != None: config_opts += "--prefix %s" % opts.prefix
if opts.bindir != None: config_opts += "--bindir %s" % opts.bindir

do_cmd(configure_cmd)

if do_build: do_cmd("make")
if do_install: do_cmd("make install")   

sys.exit(0)

### Maybe someday we'll do this:

if False:
    from distutils.core import setup

    setup(
        name = "pydb",
        version = "1.26",
        description = "Improved Python Debugger",
        long_description = \
        """pydb is an expanded version of the Python debugger loosely
        based on the gdb command set. It also has all of the features
        found in an earlier version of pydb.py that was distributed
        with the debugger GUI ddd.

        Some (but by no means all) of the features and changes from pdb.py:

        * thread debugging. (Experimental)
        * signal handling (similar to gdb's).
        * non-interactive POSIX-shell like line tracing.
        * disassembly of instructions
        * ipython support
        * ability to flexibly redirect debugger output to a file (useful for example in cgi's)
        * Many gdb commands such as the set/show/info commands
        * gdb-like help with subcommand-specific help available on info, set, or show subcommands; e.g. help info line works
        * Restart program saving settings (gdb's run) or a pure exec restart. (The latter reloads the entire debugger and will cause imports to get reloaded.)
        * debugger is installed in binary directory; i.e. one runs "pydb script..." rather than "python pdb.py script..."
        * Accepts debugger command options; e.g. redirecting debugger output, initial debugger script, or batch mode.
        * Perl's "examine" command to show info about a symbol. For functions, methods, classes and modules the documentation string if any is printed. For functions, we also show the argument list. For objects, instance variables of the class and object are shown.
        * More extensive and complete documentation.
        * Comes with regression tests. Can also be subclassed to capture debugger output.
         * Works with GUI frontend ddd. (For now a testing version of ddd is required.) 
        """,
        
        author = "Rocky Bernstein",
        author_email = "rockyb@users.sourceforge.net",
        license = "BSD-like",
        url = "http://bashdb.sourceforge.net/pydb",
        packages = ['pydb']
        )
### 

    
