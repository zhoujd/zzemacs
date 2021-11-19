CPU
===

## How to find out which CPU core a process is running on

    ## https://www.xmodulo.com/cpu-core-process-is-running.html
    ## Method One: taskset
    $ taskset -c -p <pid>
    $ taskset -c -p 5357

    ## Method Two: ps (under PSR column)
    $ ps -o pid,psr,comm -p <pid>

    ## Method Three: top
    ## First, launch top command with p option. Then press f key, and add Last used CPU column to the display.
    ## The currently used CPU core will appear under P (or PSR) column.
    $ top -p 5357

    ## Method Four: htop
    ## Launch htop from the command line. Press <F2> key, go to Columns, and add PROCESSOR under Available Columns.
