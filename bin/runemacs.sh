#!/bin/sh

if [ "$OS" = "Windows_NT" ] ; then
    SCRIPT_HOME=$(cd $(dirname $0) && pwd -W)
    ZZEMACS_ROOT=$(cd $SCRIPT_HOME/.. && pwd -W)
else
    SCRIPT_HOME=$(cd $(dirname $0) && pwd)
    ZZEMACS_ROOT=$(cd $SCRIPT_HOME/.. && pwd)
fi

emacs --no-site-file -q \
      --eval "(setq zzemacs-path \"${ZZEMACS_ROOT}\")" \
      --eval "(load-file \"${ZZEMACS_ROOT}/.emacs\")"  \
      --eval "(message \"start emacs finished.\")"     \
      $* >/dev/null 2>&1 &
