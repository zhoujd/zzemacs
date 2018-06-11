#!/bin/sh

##Get script path
SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

ecl -load $SCRIPT_ROOT/../lib/lisp/slime-remote.lisp
