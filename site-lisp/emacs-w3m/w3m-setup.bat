@echo off
echo build emacs-w3m start ...

set EMACS_PATH=C:\emacs-24.3\bin\emacs

cd ..\site-lisp\emacs-w3m
echo clean *.elc files
if exist *.elc del *.elc

echo compile files
%EMACS_PATH% -batch -q -no-site-file -l w3mhack.el NONE -f w3mhack-nonunix-install

echo build emacs-w3m end ...

pause
@echo on
