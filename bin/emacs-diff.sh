#!/bin/sh

### parameter desc
## $1 => $LOCAL
## $2 => $REMOTE

EMACS="emacs"

$EMACS -q --no-site-file \
		  --eval "(load-file \"~/zzemacs/elisp/ediff-sample.el\")" \
          --eval "(ediff-sample-files \"$1\" \"$2\")" \

