#!/bin/sh

### parameter desc
## $1 => $LOCAL
## $2 => $REMOTE

EMACS="emacs"

$EMACS --no-site-file -q \
       --eval "(load-file \"~/zzemacs/elisp/ediff-sample.el\")" \
       --eval "(ediff-sample-diff \"$1\" \"$2\")" \
