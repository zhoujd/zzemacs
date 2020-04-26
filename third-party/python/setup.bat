@echo off
echo setup to %OS% start ...

set CURRENT_DIR=%CD%

echo setup python deps via pip
pip install -r %CURRENT_DIR%\py3.txt

set CURRENT_DIR=

echo setup to %OS% end ...
pause

@echo on
