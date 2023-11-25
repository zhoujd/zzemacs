@echo off
echo setup to %OS% start ...

set MYCFG_PATH="%APPDATA%\.emacs"
set MYHOME="c:/zznix/home/jiandon"
set CURRENT_DIR=%CD%

if exist %MYCFG_PATH% del %MYCFG_PATH%

echo ;;;This is _emacs for %USERNAME% on %COMPUTERNAME%>> %MYCFG_PATH%
echo (setenv "HOME" %MYHOME%)>> %MYCFG_PATH%
echo (setq default-directory "~")>> %MYCFG_PATH%
echo (if (file-exists-p "~/zzemacs/.emacs")>> %MYCFG_PATH%
echo     (load-file "~/zzemacs/.emacs")>> %MYCFG_PATH%
echo     (message "zzemacs has not install"))>> %MYCFG_PATH%

echo setup python environment ...
@cd third-party\python
@call setup.bat
@echo off & cd %CURRENT_DIR%

set MYHOME=
set MYCFG_PATH=
set CURRENT_DIR=

echo setup to %OS% end ...
pause

@echo on
