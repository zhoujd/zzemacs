#!/bin/sh

EMACS="emacs"

$EMACS -q --no-site-file \
          --eval "(setq ediff-quit-hook 'kill-emacs)" \
          --eval "(ediff-files \"$1\" \"$2\"))" \

