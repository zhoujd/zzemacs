#!/bin/sh

EMACS="emacs"  ##"emacs"

$EMACS -q --no-site-file \
          --eval \
          "(progn
			(setq emerge-temp-local-file  \"$2\"
				  emerge-temp-base-file   \"$3\"
				  emerge-temp-remote-file \"$1\")
			(ediff-merge-files-with-ancestor \"$2\" \"$3\" \"$1\" nil \"$4\"))"

