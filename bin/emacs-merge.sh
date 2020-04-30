#!/bin/sh

### parameter desc
## $1 => $BASE
## $2 => $LOCAL
## $3 => $REMOTE
## $4 => $MERGED

# Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    ZZEMACS_ROOT=$(cd $ZZNIX_HOME && pwd -W)/home/jiandon/zzemacs
else
    ZZEMACS_ROOT=$(cd $(dirname $0)/.. && pwd)
fi

EMACS="emacs"

$EMACS --quick \
	   --eval "(load-file \"$ZZEMACS_ROOT/elisp/ediff-sample.el\")" \
       --eval "(ediff-merge-files \"$2\" \"$3\" \"$1\" \"$4\")" \
       --eval "(message \"emacs merge finished.\")"
