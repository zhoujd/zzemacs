@echo off
echo unsetup to %OS% start ...

set MYCFG_PATH="%APPDATA%\.emacs"
set CURRENT_DIR=%CD%

if exist %MYCFG_PATH% del %MYCFG_PATH%

echo unsetup python environment ...
@cd third-party\python
@if exist unsetup.bat call unsetup.bat
@echo off & cd %CURRENT_DIR%

set MYCFG_PATH=
set CURRENT_DIR=

echo unsetup to %OS% end ...
pause

@echo on
