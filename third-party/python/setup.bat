@echo off

rem http://www.python.org/
rem http://www.ipython.org/
rem http://archive.ipython.org/release/
rem https://pypi.python.org/pypi/pyreadline

set CURRENT_ROOT=%CD%

echo for python develop start ...

cd %CURRENT_ROOT%\pymacs
python pppp -C ppppconfig.py pppp.rst.in pymacs.el.in pymacs.rst.in Pymacs.py.in contrib tests
python setup.py install
cd %CURRENT_ROOT%

cd %CURRENT_ROOT%\rope
python setup.py install
cd %CURRENT_ROOT%

cd %CURRENT_ROOT%\ropemode
python setup.py install
cd %CURRENT_ROOT%

cd %CURRENT_ROOT%\ropemacs
python setup.py install
cd %CURRENT_ROOT%

cd %CURRENT_ROOT%\pyreadline
python setup.py install
cd %CURRENT_ROOT%

rem not support pydb use pdb instead

cd %CURRENT_ROOT%\pycomplete
python setup.py install
cd %CURRENT_ROOT%

echo for python develop end   ...
set CURRENT_ROOT=

pause
@echo on
