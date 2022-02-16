Python
======

## Build python from source

    $ wget https://www.python.org/ftp/python/2.7.14/Python-2.7.14.tgz
    $ tar xf Python-2.7.14.tgz
    $ cd Python-2.7.14
    $ ./configure --prefix=$HOME/local
    $ make -j 4
    $ make install

## Remove old versions of Python and pip

    $ python2 -m pip uninstall pip
    $ python3 -m pip uninstall pip

    ## 18.04
    $ sudo apt-get remove python-pip
    ## 20.04
    $ sudo apt-get remove python3-pip

## pip2 installation on ubuntu 20.04

    $ wget https://bootstrap.pypa.io/pip/2.7/get-pip.py
    ## To /usr/local/bin
    $ sudo python2 get-pip.py
    ## To ~/.local/bin
    $ python2 get-pip.py

## Python virtualenv

    ##https://docs.python.org/3/tutorial/venv.html
    ##https://python.land/virtual-environments/virtualenv
    $ python3 -m venv <MYVENV>
    $ source <MYVENV>/bin/activate

## How to debug a “Segmentation fault” in Python

    ## https://blog.richard.do/2018/03/18/how-to-debug-segmentation-fault-in-python/
    ## First add the following to the top of your module.
    import faulthandler; faulthandler.enable()
    ## Then re-run your program with the faulthandler startup flag.
    ## pass as an argument
    python -Xfaulthandler my_program.py
    ## Or as an environment variable.
    PYTHONFAULTHANDLER=1 python my_program.py

## Example of a segmentation fault on Linux with and without enabling the fault handler

    ## https://docs.python.org/3/library/faulthandler.html
    $ python3 -c "import ctypes; ctypes.string_at(0)"
    Segmentation fault

    $ python3 -q -X faulthandler
    >>> import ctypes
    >>> ctypes.string_at(0)
    Fatal Python error: Segmentation fault

    Current thread 0x00007fb899f39700 (most recent call first):
      File "/home/python/cpython/Lib/ctypes/__init__.py", line 486 in string_at
      File "<stdin>", line 1 in <module>
    Segmentation fault

    $ cat test.py
    import faulthandler
    faulthandler.enable()
    import ctypes
    ctypes.string_at(0)
    $ ./test.py
    Fatal Python error: Segmentation fault

    Current thread 0x00007fb764032740 (most recent call first):
      File "/lib/python3.8/ctypes/__init__.py", line 514 in string_at
      File "./test.py", line 29 in <module>
    Segmentation fault

## Debugging segmentation faults using gdb

    ## https://scipy-lectures.org/advanced/debugging/index.html#debugging-segmentation-faults-using-gdb
    $ gdb python
    (gdb) run segfault.py
    (gdb) up
    (gdb) up
    (gdb) pyframe
