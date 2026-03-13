## commond.gdb

## allow loading the .gdbinit from anywhere on the system
set auto-load safe-path /

## history
set history filename ~/.gdb_history
set history save on

## print
set print object on
set print array-indexes on
set print pretty on

## common
set breakpoint pending on
set pagination off
set step-mode on
set confirm off

## multi-thread
set non-stop on
set print thread-events off

## multi-process
#set detach-on-fork on
#set follow-fork-mode [parent|child]
#set follow-exec-mode [new|same]
