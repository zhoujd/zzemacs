@echo off
echo setup to %OS% start ...
set MYCFG_PATH="%APPDATA%\.emacs"
set MYHOME="C:/zznix/home/zhoujd"

if exist %MYCFG_PATH% del %MYCFG_PATH%

echo ;;;This is _emacs for %USERNAME% on %COMPUTERNAME%>> %MYCFG_PATH%
echo (setenv "HOME" %MYHOME%)>> %MYCFG_PATH%
echo (setq default-directory "~")>> %MYCFG_PATH%
echo (load-file "~/zzemacs/.emacs")>> %MYCFG_PATH%

set MYHOME=
set MYCFG_PATH=
echo setup to %OS% end
pause
@echo on
