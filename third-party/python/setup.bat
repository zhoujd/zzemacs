@echo off

rem http://www.python.org/
rem http://www.ipython.org/
rem http://archive.ipython.org/release/
rem https://pypi.python.org/pypi/pyreadline

set CURRENT_ROOT=%CD%

echo for python develop start ...

# virtualenv
python -m pip install virtualenv epc

# Either of these
python -m pip install rope
python -m pip install jedi
# flake8 for code checks
python -m pip install flake8
# importmagic for automatic imports
python -m pip install importmagic
# and autopep8 for automatic PEP8 formatting
python -m pip install autopep8
# and yapf for code formatting
python -m pip install yapf


echo for python develop end   ...
set CURRENT_ROOT=

pause
@echo on
