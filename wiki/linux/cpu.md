CPU
===

## How to find out which CPU core a process is running on

    ## https://www.xmodulo.com/cpu-core-process-is-running.html
    ## Method One: taskset
    $ taskset -c -p <pid>
    $ taskset -c -p 5357

    ## Method Two: ps (under PSR column)
    $ ps -o pid,psr,comm -p <pid>
    $ watch -tdn0.5 ps -mo pid,tid,%cpu,psr -p \`pgrep emacs\`

    ## Method Three: top
    ## First, launch top command with p option. Then press f key, and add Last used CPU column to the display.
    ## The currently used CPU core will appear under P (or PSR) column.
    $ top -p 5357

    ## Method Four: htop
    ## Launch htop from the command line. Press <F2> key, go to Columns, and add PROCESSOR under Available Columns.

## How to run program or process on specific CPU cores on Linux

    ## https://s905060.gitbooks.io/site-reliability-engineer-handbook/content/taskset.html
    $ sudo apt-get install util-linux
    $ sudo yum install util-linux

    ## View the CPU Affinity of a Running Process
    $ taskset -p <PID>
    $ taskset -p 2915
    pid 2915's current affinity mask: ff
    $ taskset -cp 2915
    pid 2915's current affinity list: 0-7

    ## Pin a Running Process to Particular CPU Core(s)
    ## User must have CAP_SYS_NICE capability. Any user can view the affinity mask of a process.
    $ taskset -p <COREMASK> <PID>
    $ taskset -cp <CORE-LIST> <PID>
    $ taskset -p 0x11 9030
    pid 9030's current affinity mask: ff
    pid 9030's new affinity mask: 11
    $ taskset -cp 0,4 9030

    ## Launch a Program on Specific CPU Cores
    $ taskset <COREMASK> <EXECUTABLE>
    $ taskset 0x1 vlc  ##launch vlc program on a CPU core 0

    ## Dedicate a Whole CPU Core to a Particular Program
    ## Use "isolcpus" kernel parameter, which allows you to reserve the CPU core during boot.
    ## Then the Linux scheduler will not schedule any regular process on the reserved CPU core(s),
    ## unless specifically requested with taskset. For example, to reserve CPU cores 0 and 1, add "isolcpus=0,1" kernel parameter.
    ## Upon boot, then use taskset to safely assign the reserved CPU cores to your program.
