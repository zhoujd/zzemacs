Python
======

## URLs

    ## watchdog(http://packages.python.org/watchdog/)
    ## pyquery(http://packages.python.org/pyquery/)
    ## MySQLdb(http://mysql-python.sourceforge.net/MySQLdb.html)
    ## fuzzywuzzy(https://github.com/seatgeek/fuzzywuzzy)

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

    $ cat > add.py <<EOF
    # cython: language_level=3
    def add_number(a, b):
        return a + b
    EOF

    ## https://pypi.org/project/Cython/
    $ pip3 install Cython

    $ python setup.py build_ext --inplace
    from <.so> import <function>
    or
    import <.so>

    ## test
    import add
    print(add.add_number(5, 10))

## Compile py to so library

    $ cat > add.py <<EOF
    def add_number(a, b):
        return a + b
    EOF

    $ cat > compile.py <<EOF
    from distutils.core import setup
    from distutils.extension import Extension
    from Cython.Distutils import build_ext
    ext_modules = [
        Extension("test",  ["add.py"]),
    ]
    setup(
        name = "test",
        cmdclass = {'build_ext': build_ext},
        ext_modules = ext_modules
    )
    EOF

    $ python3 compile.py build_ext --inplace

## Python format

    >>> X = '{motto}, {0} and {food}'.format(42, motto=3.14, food=[1, 2])
    >>> X
    '3.14, 42 and [1, 2]'
    >>> X.split(' and ')
    ['3.14, 42', '[1, 2]']
    >>> Y = X.replace('and', 'but under no circumstances')
    >>> Y
    '3.14, 42 but under no circumstances [1, 2]'

## Pip on Arm

    ## https://pypi.org/search/?q=aarch64

## path.py(http://pypi.python.org/pypi/path.py)

    from path import path
    d = path('~/bin')
    for f in d.files('*.py'):
        f.chmod(0755)

## sh(http://amoffat.github.com/sh/)

    from sh import git, ls, wc

    # checkout master branch
    git(checkout="master")

    # print(the contents of this directory
    print(ls("-l"))

    # get the longest line of this file
    longest_line = wc(__file__, "-L")

## Create wheel

    $ pip3 install setuptools wheel
    $ cat setup.py <<EOF
    from setuptools import setup, find_packages

    setup(
        name='your_package_name',
        version='0.1.0',
        packages=find_packages(),
        # Add other metadata like author, description, dependencies, etc.
        author='Your Name',
        description='A short description of your package',
        install_requires=[
            'dependency1',
            'dependency2>=1.0',
        ],
    )
    EOF

    $ python3 setup.py bdist_wheel --dist-dir ${DIST_DIR}
    or
    $ rm -rf dist
    $ python3 setup.py bdist_wheel
    $ pip3 install dist/triton*.whl

    $ pip3 wheel . --no-deps --wheel-dir dist
    $ pip3 wheel SomePackage
    $ pip3 wheel -r requirements.txt

    ## use twine to upload your package to PyPI
    $ twine upload dist/*

    ## https://python-poetry.org/
    $ cat pyproject.toml <<EOF
    [tool.poetry]
    name = "my-project"
    version = "0.1.0"
    description = "some longer description"
    authors = ["Some Author <some@author.io>"]

    [tool.poetry.dependencies]
    python = "*"

    [tool.poetry.dev-dependencies]
    pytest = "^3.4"
    EOF
    $ poetry build

    $ python3 -m build --wheel python --no-isolation
    $ ls python/dist/*.whl
