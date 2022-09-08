Python
======

## Build python from source

    $ VERSION=2.7.14
    $ wget https://www.python.org/ftp/python/${VERSION}/Python-${VERSION}.tgz
    $ tar xf Python-${VERSION}.tgz
    $ cd Python-${VERSION}
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

## Absolute vs Relative Imports in Python

    ## https://realpython.com/absolute-vs-relative-python-imports/
    └── project
        ├── package1
        │   ├── module1.py
        │   └── module2.py
        └── package2
            ├── __init__.py
            ├── module3.py
            ├── module4.py
            └── subpackage1
                └── module5.py

    ## Absolute Imports
    from package1 import module1
    from package1.module2 import function1
    from package2 import class1
    from package2.subpackage1.module5 import function2

    ## Relative Imports
    from .some_module import some_class
    from ..some_package import some_function
    from . import some_class

    # package1/module1.py
    from .module2 import function1

    # package2/module3.py
    from . import class1
    from .subpackage1.module5 import function2

## What does -> mean in Python function definitions

    ## https://stackoverflow.com/questions/14379753/what-does-mean-in-python-function-definitions
    ## https://peps.python.org/pep-3107/

    ## The -> int just tells that f() returns an integer (but it doesn't force the function to return an integer)
    ## It is called a return annotation, and can be accessed as f.__annotations__['return']
    def f(x) -> int:
        return int(x)

    ## Python also supports parameter annotations:
    def f(x: float) -> int:
        return int(x)

## Create dymanic library (*.so)

    $ cat setup.py
    from distutils.core import setup
    from Cython.Build import cythonize
    setup(ext_modules = cythonize('add.py'))

    $ cat add.py
    # cython: language_level=3
    def add_number(a, b):
        return a + b

    ## https://pypi.org/project/Cython/
    $ pip3 install Cython

    $ python setup.py build_ext --inplace
    from <.so> import <function>
    or
    import <.so>

    ## test
    import add
    print(add.add_number(5, 10))
