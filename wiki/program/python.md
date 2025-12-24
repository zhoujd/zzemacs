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

## Decorators

    ## curl http://localhost:5000/

    from flask import Flask, request, abort

    app = Flask(__name__)

    def only_user_agent(user_agent):
        def inner_decorator(f):
            def wrapped(*args, **kwargs):
                if user_agent not in request.user_agent.string.lower():
                    abort(404)
                return f(*args, **kwargs)
            return wrapped
        return inner_decorator

    @app.route('/')
    @only_user_agent('curl')
    def index():
        return 'Hello Curl!'

## Run piped commands in Python

```
## Method 1: Chaining subprocess.Popen (Recommended for advanced piping)
import subprocess

# Example: Run 'ps -ef | head -n 5'

# 1. Run the first command 'ps -ef'
# The standard output (stdout) is directed to a pipe
ps_cmd = subprocess.Popen(["ps", "-ef"], stdout=subprocess.PIPE)

# 2. Run the second command 'head -n 5'
# The standard input (stdin) takes the stdout of the first command
# The standard output is again directed to a pipe to capture the final result
head_cmd = subprocess.Popen(
    ["head", "-n", "5"],
    stdin=ps_cmd.stdout,
    stdout=subprocess.PIPE,
    encoding="utf-8", # Decodes the output as text
)

# 3. Close the output stream from ps_cmd to allow head_cmd to complete
ps_cmd.stdout.close()

# 4. Get the output and error messages from the final command
stdout, stderr = head_cmd.communicate()

# Print the result
print(stdout)

## Method 2: Using subprocess.run with shell=True (Simpler, but less secure)
import subprocess

# Example: Run 'ls -l | grep ".py"' using shell=True
command_string = "ls -l | grep \".py\""
try:
    result = subprocess.run(
        command_string,
        shell=True,
        capture_output=True,
        text=True, # Decodes output as text
        check=True # Raises an exception if the command fails
    )
    print(result.stdout)
except subprocess.CalledProcessError as e:
    print(f"Command failed with error: {e.stderr}")
```

## Run linux command with multi pipes

```
import shlex
from subprocess import PIPE, Popen

def run_pipes(cmds):
    """
    Run commands in PIPE, return the last process in chain
    """
    cmds = map(shlex.split, cmds)
    first_cmd, *rest_cmds = cmds
    procs = [Popen(first_cmd, stdout=PIPE)]
    for cmd in rest_cmds:
        last_stdout = procs[-1].stdout
        proc = Popen(cmd, stdin=last_stdout, stdout=PIPE)
        procs.append(proc)
    return procs[-1]

cmds = [
    "ping -c1 5.9.16.40",
    "grep ttl",
    "awk '{print $4}'",
    "sed 's/.$//'",
]

last_proc = run_pipes(cmds)
stdout = last_proc.stdout
for line in stdout:
    line = line.decode()
    print(line, end="")
```
