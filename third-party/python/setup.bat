@echo off

rem http://www.python.org/
rem http://www.ipython.org/
rem http://archive.ipython.org/release/
rem https://pypi.python.org/pypi/pyreadline

set ORIGIN_PATH=%PATH%
set PATH="C:\python27";%ORIGIN_PATH%
set CURRENT_ROOT="C:\zznix\home\zhoujd\zzemacs\third-party\python"
set PYTHON_LIB_ROOT="C:\Python27\Lib\site-packages"

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

echo install pycomplete.py to python lib
copy %CURRENT_ROOT%\pycomplete.py %PYTHON_LIB_ROOT% > nul

echo for python develop end   ...
set CURRENT_ROOT=
set PYTHON_LIB_ROOT=
set PATH=%ORIGIN_PATH%
set ORIGIN_PATH=

pause
@echo on
