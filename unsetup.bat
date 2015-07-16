@echo off
echo setup to %OS% start ...

set MYCFG_PATH="%APPDATA%\.emacs"
set MYHOME="c:/zznix/home/zhoujd"
set CURRENT_DIR=%CD%

if exist %MYCFG_PATH% del %MYCFG_PATH%

echo setup python environment ...
@cd third-party\python
@if exist unsetup.bat call unsetup.bat
@echo off & cd %CURRENT_DIR%

set MYHOME=
set MYCFG_PATH=
set CURRENT_DIR=

echo setup to %OS% end ...
pause

@echo on
