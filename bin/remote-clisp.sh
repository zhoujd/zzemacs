#!/bin/sh

##Get script path
SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

clisp -i ~/.clisprc $SCRIPT_ROOT/../third-party/lisp/slime-remote.lisp
