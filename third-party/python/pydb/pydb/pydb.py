#!/usr/bin/python -t
"""A Python debugger. Contains user-callable routines, e.g.
exception_hook, run, runl, runv, debugger (AKA set_trace), post_mortem,
or runevel.

(See also pydb.doc for documentation.)

$Id: pydb.py.in,v 1.163 2009/01/19 18:12:43 rockyb Exp $"""

# This part contains user entry subroutines.

__title__ = 'pydb'

# The name of the debugger we are currently going by.
__version__ = '1.26'

import inspect, os, re, sys, traceback, types

import fns

from optparse import OptionParser

from bdb import BdbQuit
from gdb import Gdb
from gdb import Restart

__all__ = ["Pdb",     "debugger", "exception_hook",
           "help",    "pm",       "post_mortem",   "run",
           "runcall",  "runeval", "runl",          "runv",
           "set_trace"]

old_handler = None

def process_options(p, debugger_name, pkg_version, option_list=None):
    """Handle debugger options. Use option_list if you want are writing
    another main program and want to extend the existing set of debugger
    options.

    The options dicionary from opt_parser is return. Global sys.argv is
    also updated."""
    usage_str="""%prog [debugger-options] [python-script [script-options...]]

       Runs the extended python debugger"""

    optparser = OptionParser(usage=usage_str, option_list=option_list,
                             version="%%prog version %s" % pkg_version)

    optparser.add_option("-X", "--trace", dest="linetrace",
                         action="store_true", default=False, 
                         help="Show lines before executing them. " +
                         "This option also sets --batch")
    optparser.add_option("-F", "--fntrace", dest="fntrace",
                         action="store_true", default=False, 
                         help="Show functions before executing them. " +
                         "This option also sets --batch")
    optparser.add_option("--batch", dest="noninteractive",
                         action="store_true", default=False, 
                         help="Don't run interactive commands shell on "+
                         "stops.")
    optparser.add_option("--basename", dest="basename",
                         action="store_true", default=False, 
                         help="Filenames strip off basename, (e.g. for regression tests)"
                         )
    optparser.add_option("-x", "--command", dest="command",
                         action="store", type='string', metavar='FILE',
                         help="Execute commands from FILE.")
    optparser.add_option("--cd", dest="cd",
                         action="store", type='string', metavar='DIR',
                         help="Change current directory to DIR.")
    optparser.add_option("--error", dest="errors", metavar='FILE',
                         action="store", type='string',
                         help="Write debugger's error output "
                         + "(stderr) to FILE")
    optparser.add_option("-e", "--exec", dest="execute", type="string",
                         help="list of debugger commands to " +
                         "execute. Separate the commands with ;;")
    optparser.add_option("-n", "--nx", dest="noexecute",
                         action="store_true", default=False, 
                         help="Don't execute commands found in any " +
                         "initialization files")
##    optparser.add_option("--pdbserver", dest="pdbserver",
##                         help="Start the debugger and execute the pdbserver " \
##                         + "command. The arguments should be of the form," \
##                         + " 'protocol address scriptname'."),
    optparser.add_option("-o", "--output", dest="output", metavar='FILE',
                         action="store", type='string',
                         help="Write debugger's output (stdout) "
                         + "to FILE")
##    optparser.add_option("--pid", dest="pid",
##                         help="Attach to running process PID.")
    optparser.add_option("--sigcheck", dest="sigcheck",
                         action="store_true", default=False, 
                         help="Set to watch for signal handler changes")
    optparser.add_option("-t", "--target", dest="target",
                         help="Specify a target to connect to. Arguments" \
                         + " should be of form, 'protocol address'."),
    optparser.add_option("-T", "--threading", dest="threading",
                         action="store_true", default=False,
                         help="Start off with threading debug support")

    # annotate option produces annotations, used in pydb.el for a better emacs
    # integration. Annotations are similar in purpose to those of GDB (see
    # that manual for a description), although the syntax is different.
    # they have the following format:
    #
    # ^Z^Zannotname
    # <arbitrary text>
    # ^Z^Z
    #
    # where ^Z is the ctrl-Z character, and "annotname" is the name of the
    # annotation. A line with only two ^Z ends the annotation (no nesting
    # allowed). See pydb.el for the usage
    optparser.add_option("--annotate", default=0, type="int",
                         help="Use annotations (to work inside emacs)")

    # Set up to stop on the first non-option because that's the name
    # of the script to be debugged on arguments following that are
    # that scripts options that should be left untouched.  We would
    # not want to interpret and option for the script, e.g. --help, as
    # one one of our own, e.g. --help.

    optparser.disable_interspersed_args()

    # execfile() runs out of Bdb.py and uses sys.argv, so we have to
    # clobber it and make it what the debugged script expects. Also
    # the debugged script probably wants to look at sys.argv.
    # So we'll change sys.argv to look like the program were invoked
    # directly

    # Save the original just for use in restart (via exec)
    p._sys_argv = list(sys.argv)   

    (opts, sys.argv) = optparser.parse_args()

##     if opts.target:
##         target(opts.target, opts, p)
##         sys.exit()
##     elif opts.pdbserver:
##         pdbserver(opts.pdbserver, p)
##         sys.exit()
    if opts.threading:
        import threaddbg
        tpdb           = threaddbg.threadDbg()
        tpdb._sys_argv = p._sys_argv
        p              = tpdb
        # Get us out of <module>() called from file '<string>' at line 1
        p.step_ignore  = 1  
##     elif opts.pid:
##         target(opts.pid, opts, p)
##         sys.exit()

    if opts.linetrace or opts.fntrace: opts.noninteractive = True
    p.fntrace = opts.fntrace
    p.linetrace = opts.linetrace
    p.noninteractive = opts.noninteractive

    # --nx or -n ?
    if not opts.noexecute:
        # Read debugger startup file(s), e.g. $HOME/.pydbrc and ./.pydbrc
        debugger_startup = ".%src" % debugger_name
        if 'HOME' in os.environ:
            debug_startup_home = os.path.join(os.environ['HOME'],
                                              debugger_startup)
            p.setup_source(debug_startup_home)
            
        p.setup_source(debugger_startup)

    if opts.cd:
        os.chdir(opts.cd)

    if opts.basename: p.basename = True

    # As per gdb, first we execute user initialization files and then
    # we execute any file specified via --command.
    if opts.command:
        p.setup_source(os.path.expanduser(opts.command), True);

    if opts.execute:
        p.cmdqueue = list(opts.execute.split(';;'))

    if opts.sigcheck:
        p.cmdqueue.insert(0, "set sigcheck on")
    else:
        p.cmdqueue.insert(0, "set sigcheck off")

    if opts.output:
        try: 
            p.stdout = open(opts.output, 'w')

        except IOError, (errno, strerror):
            print "I/O in opening debugger output file %s" % opts.output
            print "error(%s): %s" % (errno, strerror)
        except ValueError:
            print "Could not convert data to an integer."
        except:
            print "Unexpected error in opening debugger output file %s" % \
                  opts.output
            print sys.exc_info()[0]
            sys.exit(2)

    if opts.errors:
        try: 
            p.stderr = open(opts.errors, 'w')
        except IOError, (errno, strerror):
            print "I/O in opening debugger output file %s" % opts.errors
            print "error(%s): %s" % (errno, strerror)
        except ValueError:
            print "Could not convert data to an integer."
        except:
            print "Unexpected error in opening debugger output file %s" % \
                  opts.errors
            print sys.exc_info()[0]
            sys.exit(2)

    p.annotate = opts.annotate
            
    return (opts, p)

class Pdb(Gdb):

    """Right now this is basically the same thing as the Gdb class. In the
    future though we may break out more stand-alone aspects and put them here
    versues an embedded debugger which is what Gdb would be used for.

    See the Gdb class documentation for more information and information on
    the meanings of the parameters.

    If there is something you want to run before/after on each stopping
    point, set precmd_hook and/or postcmd_hook
    """

    def __init__(self, completekey='tab', stdin=None, stdout=None,
                 siglist=None):
        Gdb.__init__(self, completekey, stdin, stdout, siglist)
        self.annotate = 0     # if > 0, print annotations (for better emacs
                              # integration)
        self.__show_breakpoints_postcmd = set(["break", "b", "tbreak", 
                                               "disable", "enable", "condition",
                                               "cl", "clear", "delete"])
        self.__show_annotations_preloop = set(["s", "step", "c", "continue",
                                               "n", "next", "return", "jump"])
        self.__show_annotations_postcmd = set(["down", "frame", "up"])
        self.__show_annotations = True
        self.precmd_hooks = []
        self.preloop_hooks = []
        self.postcmd_hooks = []
        self.postloop_hooks = []
        return

    def __annotation(self, label, cmd, *args):
        self.msg(chr(26) + chr(26) + label) # chr(26) is Ctrl-Z
        cmd(*args)
        self.msg(chr(26) + chr(26))


    def precmd(self, line):
        fns.runhooks(self, self.precmd_hooks, line)
        return line
        
    def preloop(self):
        fns.runhooks(self, self.preloop_hooks)
        if self.annotate > 0:
            if self.__show_annotations:
                # if we are here, the stack frames have changed outside the
                # command loop (e.g. after a "continue" command), so we show
                # the annotations again
                self.__annotation('breakpoints', self.do_info, 'breakpoints')
                self.__annotation('stack', self.do_where, 0)
                self.__annotation('locals', self.do_info, 'locals')

    def postcmd(self, stop, line):
        if self.annotate > 0:
            cmd = ""
            if line: cmd = line.split()[0].strip()
            if not cmd: cmd = self.lastcmd 
            if cmd in self.__show_breakpoints_postcmd:
                self.__annotation('breakpoints', self.do_info, 'breakpoints')
            self.__show_annotations = cmd in self.__show_annotations_preloop
            if cmd in self.__show_annotations_postcmd:
                self.__annotation('stack', self.do_where, 0)
                self.__annotation('locals', self.do_info, 'locals')
        fns.runhooks(self, self.postcmd_hooks, stop, line)
        return stop

    def postloop(self):
        fns.runhooks(self, self.postloop_hooks)
        return

# -- end class Pdb

#########################################################
# Other top-level routines
#########################################################

# Start here: note status is new and params have changed...
def debugger(dbg_cmds=None, add_exception_hook=True, add_threaddbg=False,
             status='start', stdout=None):
    """Enter the debugger at the calling stack frame.  This can be
    used as a way to hard-code a breakpoint in a program, even if the
    code is not otherwise being debugged (e.g., when an assertion
    fails).  Leaving the debugger, i.e. issuing a "quit" command,
    terminates the program.

    dbg_cmds, a list of strings, is an optional list of debugger
    commands you want to run.

    If add_exception is True (the default), we will install an exception hook
    to call the post-mortem debugger on an unhandled exception.

    If the debugger was called previously, normally we use that. This
    way any previous debugger state that was set gets used.

    Setting the parameter `status' to the string 'new' will cause a
    new instance to be created which might wiping out other
    instances which might contain information like breakpoints or
    debugger settings.
    
    If instead `status' is set to 'continue', an existing instance
    will resume. Omitting status or setting it to 'start' will start
    the debugger which might be a 'new' depending on whether or not
    an instance has been created. 

    In all cases a string indicating what was done is returned.

    Since debugging slows down a program, one may want to stop and
    continue it at various points in your program.
    """
    global _pydb_trace

    if types.BooleanType == type(status):
        if status: status = 'new' 
        else: status = 'start'
        print "debugger() interface has changed see docstring."

    try:
        if not isinstance(_pydb_trace, Pdb):
            print "Your program should not use _pydb_trace"
            return '_pydb_trace conflict'
    except NameError:
        status = 'new'
    except:
        print "Unknown error"
        return 'Unknown error'

    back_frame = inspect.currentframe().f_back
    if 'new' == status:
        if add_threaddbg:
            import threaddbg
            _pydb_trace = threaddbg.threadDbg()
        else:
            _pydb_trace = Pdb(stdout=stdout)
        _pydb_trace._program_sys_argv = list(sys.argv)
        _pydb_trace._sys_argv = list(_pydb_trace._program_sys_argv)
        _pydb_trace._sys_argv[:0] = [__title__]
        _pydb_trace.main_dirname = os.path.dirname(sys.argv[0])

    _pydb_trace.curframe  = back_frame
    _pydb_trace.stopframe = back_frame
    if 'continue' == status:
        pass
    elif _pydb_trace.running:
        return 'running'
    _pydb_trace.running = True

    # We don't support "run" so we'll make "run" and "R" be "restart"
    _pydb_trace.do_R = _pydb_trace.do_run = _pydb_trace.do_restart

    _pydb_trace.curframe  = back_frame
    _pydb_trace.stopframe = back_frame

    # Dispatcher needs to field BdbQuit exception
    _pydb_trace.field_BdbQuit   = True

    if dbg_cmds != None:
        _pydb_trace.cmdqueue = list(dbg_cmds)

    if add_exception_hook:
        sys.excepthook = exception_hook

    _pydb_trace.set_trace(_pydb_trace.curframe)
    return status

def exception_hook(type, value, tb, dbg_cmds=None, cmdfile=None):
    """An exception hook to call pydb's post-mortem debugger.

    cmdfile is an optional debugger command file you want to run
    "source" on.

    dbg_cmds is a list of debugger commands you want to run.

    To use add this to your Python program
       import sys
       sys.excepthook = pydb.exception_hook
    """
    traceback.print_exception(type, value, tb)
    post_mortem(tb, dbg_cmds, cmdfile, 0)
    return

# print help
def help():
    doc_file='%s.doc' % __title__
    for dirname in sys.path:
        fullname = os.path.join(dirname, __title__, doc_file)
        if os.path.isfile(fullname):
            sts = os.system('${PAGER-more} '+fullname)
            if sts: print 'Pager exit status: %s' % str(sts)
            break
    else:
        print ("Sorry, can't find the help file '%s' along the "
                  + "Python search path") % doc_file
    return

## def pdbserver(addr, p):
##     """Set up to allow a program to be debugged from outside of the
##     processes. To take control a debugger front end connects to
##     'addr', which a protocol-specific address, i.e.  tcp = 'tcp
##     mydomainname.com:9876' serial = 'serial /dev/ttyC0'.

##     The name is reminiscent of gdbserver.
##     """
##     p.do_pdbserver(addr)
##     while True:
##         try:
##             p._runscript(p.mainpyfile)
##             if p._user_requested_quit: break
##         except Restart:
##             sys.argv = list(p._program_sys_argv)
##             p.msg('Restarting')

# Post-Mortem interface

def pm(dbg_cmds=None, frameno=1, p=None):

    """Set up post-mortem debugging using the last traceback.  But if
    there is no traceback, we'll assume that sys.exc_info() contains
    what we want and frameno is the index location of where we want
    to start.

    'dbg_cmds' is an optional list of debugger commands you want to run.
    'p', is an optional pydb.Pdb object.
    """

    tb = fns.get_last_tb_or_frame_tb()
    post_mortem(tb, dbg_cmds=dbg_cmds, p=p)
    return

def post_mortem(t=None, dbg_cmds=None, cmdfile=None, frameno=1, 
                p=None, stdout=None):
    """Enter debugger read loop after your program has crashed.

    If no traceback parameter, t, is supplied, the last traceback and
    if that doesn't exist either we'll assume that sys.exc_info()
    contains what we want and frameno is the index location of where
    we want to start.

    'dbg_cmds' is an optional list of debugger commands you want to run.

    'cmdfile' is an optional debugger command file you want to run
    "source" on.

    'frameno' specifies how many frames to ignore in the traceback.  The
    default is 1 - we don't need the call to post_mortem. If you have
    wrapper functions that call this one, you may want to increase
    frameno.
    """

    if p == None:
        p = Pdb(stdout=stdout)
        pass
    p.reset()
    p.running = False
    re_bogus_file = re.compile("^<.+>$")

    if t is None:
        # frameno+1 because we are about to add one more level of call
        # in get_last_tb_or_frame_tb
        t = fns.get_last_tb_or_frame_tb()
        if t is None:
            print "Can't find traceback for post_mortem " + \
                  "in sys.last_traceback or sys.exec_info()"
            return

    # t has least-recent traceback entry first. We want the most-recent
    # entry. Also we'll pick out a mainpyfile name if it hasn't previously
    # been set.
    while t.tb_next is not None:
        filename = t.tb_frame.f_code.co_filename
        if 0 == len(p.mainpyfile) and not re_bogus_file.match(filename):
            p.mainpyfile = filename
        t = t.tb_next
    p.curframe = t.tb_frame

    if dbg_cmds != None:
        p.cmdqueue = list(dbg_cmds)

    if 0 == len(p._program_sys_argv):
        # Fake program (run command) args since we weren't called with any
        p._program_sys_argv = list(sys.argv[1:])
        p._program_sys_argv[:0] = [p.mainpyfile]

    if 0 == len(p._sys_argv):
        # Fake script invocation (restart) args since we don't have any
        p._sys_argv = list(p._program_sys_argv)
        p._sys_argv[:0] = [__title__]

    if cmdfile is not None:
        p.do_source(cmdfile)

    try:

        # FIXME: This can be called from except hook in which case we
        # need this. Dunno why though.
        try:
            _pydb_trace.set_trace(t.tb_frame)
        except:
            pass

        # Possibly a bug in Python 2.5. Why f.f_lineno is
        # not always equal to t.tb_lineno, I don't know.
        f = t.tb_frame
        if f and f.f_lineno != t.tb_lineno : f = f.f_back
        p.interaction(f, t)
    except Restart:
        while True:
            sys.argv = list(p._program_sys_argv)
            p.msg("Restarting %s with arguments:\n\t%s"
                  % (p.filename(p.mainpyfile),
                     " ".join(p._program_sys_argv[1:])))
            try:
                p._runscript(p.mainpyfile)
                if p._user_requested_quit:
                    break
                if p.noninteractive: break
            except Restart:
                pass
    except BdbQuit:
        pass
    return

def uncaught_exception(p):
    traceback.print_exc()
    print "Uncaught exception. Entering post mortem debugging"
    t = sys.exc_info()[2]
    p.interaction(t.tb_frame,t)
    print "Post mortem debugger finished."
    return None


def run(statement, globals=None, locals=None, stdout=None):

    """Execute the statement (given as a string) under debugger
    control starting with the statement subsequent to the place that
    this call appears in your program.

    The debugger prompt appears before any code is executed;
    you can set breakpoints and type 'continue', or you can step
    through the statement using 'step' or 'next'

    The optional globals and locals arguments specify the environment
    in which the code is executed; by default the dictionary of the
    module __main__ is used."""

    p = Pdb(stdout=stdout)
    p.running=True
    p.run(statement, globals, locals)
    return

def runeval(expression, globals=None, locals=None, stdout=None):

    """Evaluate the expression (given as a string) under debugger
    control starting with the statement subsequent to the place that
    this appears in your program.

    When runeval() returns, it returns the value of the expression.
    Otherwise this function is similar to run()."""

    p = Pdb(stdout=stdout)
    p.running=True
    try:
        return p.runeval(expression, globals, locals)
    except:
        uncaught_exception(p)

def runcall(*args, **kwds):

    """Call the function (a function or method object, not a string)
    with the given arguments starting with the statement subsequent to
    the place that this appears in your program.

    When runcall() returns, it returns whatever the function call
    returned.  The debugger prompt appears as soon as the function is
    entered."""

    p = Pdb()
    p.running=True
    try:
        return p.runcall(*args, **kwds)
    except:
        uncaught_exception(p)

def runl(*args):

    """Run debugger as though you typed something like
          os.system("pydb " + " ".join(args))

    It is also the same as runv((args)).
    For example, in:

        runl("--threading", "--nx",
              "myscript", "--my-first-option" "myfirstarg")

    --threading and --nx go to the debugger and "myscript" is the name
    of the program to run and gets the remaining parameters.

    This function may be useful inside Python shells.
    """
    runv(args)
    return

def runv(argv, p=None):

    """Run debugger as though you typed something like
          os.system("pydb " + " ".join(argv))

    More precisely the debugger setting sys.argv from argv, which
    is in fact what's done. (sys.argv[0] is pydb).

    Note that options to pydb can come before the Python script to be
    debugged. For example:

        args = ("--threading", "--nx", "myscript", "--my-first-option"
                "myfirstarg")
        runv(argv)

    --threading and --nx go to the debugger and "myscript" is the name
    of the program to run and gets the remaining parameters.

    This function may be useful inside Python shells.

    Parameter p allows you to pass in your own Pdb object.
    """
    ## FIXME: redo so we don't call main and don't have to save/restore
    ### sys.argv              
    sys_argv     = list(sys.argv)
    sys.argv[0] = 'pydb'
    sys.argv[1:] = list(argv)
    main(p)
    sys.argv     = sys_argv
    return

# This is the older name for debugger and is compatible with pdb's name.
set_trace=debugger

## def signal_handler(signum, frame):
##     """ This signal handler replaces the program's signal handler
##     for the 'signum' signal (by default SIGUSR1). When a program
##     receives this signal, it creates a pdbserver.
##     Debugger clients can then attach to this pdbserver via it's pid.
##     """
##     p = Pydb()
##     p._sys_argv = list(sys.argv)
##     p.reset()
##     p.running = True
##     p.currentframe = frame

##     # Clear up namespace
##     del frame.f_globals['pydb']

##     p.do_pdbserver(pdbserver_addr)
##     p.set_trace(frame)

##     import signal
##     global oldhandler
##     if oldhandler is not None:
##         # Pass along the signal
##         signal.signal(signum, old_handler)

## # The value of 'opts' dictates whether we call do_target or do_attach, there
## # were two separate top-level routines for these options, but apart from
## # choosing which do_* to call, the code was the same so it made sense to merge.
## def target(addr, opts, p):
##     """ Connect this debugger to a pdbserver at 'addr'. 'addr' is
##     a protocol-specific address. i.e.
##     tcp = 'tcp mydomainname.com:9876'
##     serial = 'serial /dev/ttyC0'

##     'opts' an the OptionParser object. If opts.target is True, call do_target.
##     If opts.pid is true, call do_attach.
##     """
##     p.reset()
##     if opts.target:
##         p.do_target(addr)
##     elif opts.pid:
##         pid = addr[:addr.find(' ')]
##         addr = addr[addr.find(' ')+1:]
##         p.do_set('target ' + addr)
##         p.do_attach(pid)
##     while True:
##         try:
##             p.cmdloop()
##             if p._user_requested_quit: break
##         except:
##             break

#########################################################
# Main program
#########################################################

def main(p=None):
    """Routine which gets run if we were invoked directly"""

    if p is None:
        p = Pdb()
    (opts, p) = process_options(p, __title__, __version__)

    # process_options has munged sys.argv to remove any options that
    # options that belong to this debugger. The original options to
    # invoke the debugger and script are in global sys_argv

    if len(sys.argv) == 0:
        # No program given to debug. Set to go into a command loop
        # anyway
        mainpyfile = None
        p._program_sys_argv = []

    else:
        # Save the DEBUGGED programs arguments. This is in case
        # the debugged script munges these, we have a good copy to use
        # for restart
        p._program_sys_argv = list(sys.argv)

        mainpyfile = p._program_sys_argv[0] # Get script filename.
        if not os.path.isfile(mainpyfile):
            mainpyfile=fns.whence_file(mainpyfile)
            if not os.path.exists(mainpyfile):
                print "pydb: Python script file '%s' does not exist" \
                      % mainpyfile
                sys.exit(1)
                return

        # If mainpyfile is an optimized Python script try to find and
        # use non-optimized alternative.
        mainpyfile_noopt = fns.file_pyc2py(mainpyfile)
        if mainpyfile != mainpyfile_noopt \
               and os.path.exists(mainpyfile_noopt):
            print "pydb: Compiled Python script given and we can't use that."
            print "pydb: Substituting non-compiled name: %s" % mainpyfile_noopt
            mainpyfile = mainpyfile_noopt

        # Replace pydb's dir with script's dir in front of
        # module search path.
        sys.path[0] = p.main_dirname = os.path.dirname(mainpyfile)

    # XXX If a signal has been received we continue in the loop, otherwise
    # the loop exits for some reason.
    p.sig_received = False

    while True:

        # Run the debugged script over and over again until we get it
        # right.

        try:
            if p._program_sys_argv and mainpyfile:
                p._runscript(mainpyfile)
                if p.sig_received: 
                    p.sig_received = False
                    continue
            else:
                p._wait_for_mainpyfile = True
                p.interaction(None, None)

            if p._user_requested_quit: break
            if p.noninteractive: break
            if p.fntrace: 
               # fn tracing does something to not put a stop at the beginning.
                p.msg("The program finished")
                p.interaction(None, None)
            else:
                p.msg("The program finished and will be restarted")
        except Restart:
            if p._program_sys_argv:
                sys.argv = list(p._program_sys_argv)
                p.msg("Restarting %s with arguments:\n\t%s"
                      % (p.filename(mainpyfile),
                         " ".join(p._program_sys_argv[1:])))
            else: break
        except SystemExit:
            # In most cases SystemExit does not warrant a post-mortem session.
            p.msg("The program exited via sys.exit(). Exit status: %s" %
                     str(sys.exc_info()[1]))
            p.running = False;
            if p.noninteractive or hasattr(p,'desired_thread'): break
        except:
            traceback.print_exc(file=p.stdout)
            if p.noninteractive:
                p.errmsg("Uncaught exception.")
                break
            p.errmsg("Uncaught exception. Entering post mortem debugging")
            if mainpyfile:
                p.errmsg( "Running 'c' or 'step' will restart the program")
            p.running = False;
            t = sys.exc_info()[2]

            # I believe the commented code below discards information
            # about where the exception was originally raised and just
            # keeps us at the point where the exception was handled. I
            # think though we want to this information.

#             while t.tb_next is not None:
#                 t = t.tb_next
            try:
                p.interaction(t.tb_frame,t)
            except Restart:
                sys.argv = list(p._program_sys_argv)
                p.msg("Restarting %s with arguments:\n\t%s"
                         % (p.filename(mainpyfile),
                            " ".join(p._program_sys_argv[1:])))
            else:
                p.msg("Post mortem debugger finished.")
                if mainpyfile:
                    p.msg(mainpyfile + " will be restarted")
        p.step_ignore = 0

    if p.stdout != sys.stdout:
        p.stdout.close()  # In case someone is waiting on this.
    # Restore old sys.argv
    sys.argv = p._program_sys_argv
    return

# When invoked as main program, invoke the debugger on a script
if __name__=='__main__':
    main()

#
# Local variables:
#  mode: Python
# End:
