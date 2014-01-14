@echo off

del *.elc
emacs -batch -q -no-site-file -l w3mhack.el NONE -f w3mhack-nonunix-install

pause
@echo on
