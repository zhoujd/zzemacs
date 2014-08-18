#!/bin/sh

### parameter desc
## $1 => $LOCAL
## $2 => $REMOTE

EMACS="emacs"

if [ -d $1 ] ; then
    $EMACS --no-site-file -q \
		   --eval "(load-file \"~/zzemacs/elisp/ediff-sample.el\")" \
           --eval "(ediff-sample-dirs \"$1\" \"$2\")" \
        
else    
    $EMACS --no-site-file -q \
		   --eval "(load-file \"~/zzemacs/elisp/ediff-sample.el\")" \
           --eval "(ediff-sample-files \"$1\" \"$2\")" \
        
fi
