#!/bin/sh

### parameter desc
## $1 => $LOCAL
## $2 => $REMOTE

# Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    ZZEMACS_ROOT=$(cd $ZZNIX_HOME && pwd -W)/home/zhoujd/zzemacs
else
    ZZEMACS_ROOT=$(cd $(dirname $0)/.. && pwd)
fi

EMACS="emacs"

$EMACS --no-site-file -q \
       --eval "(load-file \"$ZZEMACS_ROOT/elisp/ediff-sample.el\")" \
       --eval "(ediff-sample-diff \"$1\" \"$2\")" \
       --eval "(message \"emacs diff finished.\")"
