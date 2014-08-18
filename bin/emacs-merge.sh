#!/bin/sh

### parameter desc
## $1 => $BASE
## $2 => $LOCAL
## $3 => $REMOTE
## $4 => $MERGED

EMACS="emacs"

$EMACS -q --no-site-file \
		  --eval "(load-file \"~/zzemacs/elisp/ediff-sample.el\")" \
          --eval "(ediff-merge-files \"$2\" \"$3\" \"$1\" \"$4\")" \
          --eval "(message \"emacs merge finished.\")"
