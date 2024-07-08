## commond.gdb

## save history
set history filename ~/.gdb_history
set history save on

## better print
set print object on
set print array-indexes on
set print pretty on

## common debug
set breakpoint pending on
set pagination off
set step-mode on
set confirm off

## multi-thread
set non-stop on

## multi-process
set detach-on-fork on
set follow-fork-mode child
