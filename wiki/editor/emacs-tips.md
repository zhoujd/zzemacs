Emacs Tips
==========

## How to display Unicode UTF-8 as Unicode?

    ## https://emacs.stackexchange.com/questions/10223/how-to-display-unicode-utf-8-as-unicode
    ## exmpale: \342, \200, \230
    ## Option 1
    ## force Emacs to reopen the file as UTF-8 by running the command
    ## C-x RET r (revert-buffer-with-coding-system) and entering utf-8.
    ## Option 2
    ## https://www.gnu.org/software/emacs/manual/html_node/emacs/Specify-Coding.html#index-coding-1719
    ## put something like -*-coding: utf-8-*- on the first line
    ## put something like this near the end of the file
    ## (you can replace # by any prefix, but Local Variables: and End: must appear exactly like this with the trailing colon):
    ## exmpale -
    # Local Variables:
    # coding: utf-8
    # End:

## How can I replace a character with a newline in Emacs

    M-x replace-string RET ; RET C-q C-j.
    C-q for quoted-insert,
    C-j is a newline.

## Removing all lines that don't match

    ## Delete all lines except those containing matches for REGEXP
    (keep-lines REGEXP &optional RSTART REND INTERACTIVE)

    M-x flush-lines, which removes lines matching a regexp
    M-x flush-lines RET ^$ RET
    M-x flush-lines RET ^\s-*$ RET

    M-x delete-non-matching-lines
    M-x delete-matching-lines

## Regexp

    https://wikemacs.org/wiki/Regexp

## Problems with FTP connection via Tramp

    C-x C-f /ftp:foo@android.local#2221:/storage/0123-4567
    C-x d /ftp:foo@android.local#2221:/storage/0123-4567

## Regarding .dir-locals.el

    ;; Syntax: ((MODE (VAR . VAL) ...) ...)
    ;; MODE is a symbol like `c-mode', or `nil' for all modes.
    ((c-mode
      (indent-tabs-mode . nil)
      (c-basic-offset . 4))
     (c++-mode
      (indent-tabs-mode . nil)
      (c-basic-offset . 4)))


## The 'M-o' in Emacs 28.1

    The 'M-o' ('facemenu-keymap') global binding has been removed.
    To restore the old binding, say something like:

    (require 'facemenu)
    (define-key global-map "\M-o" 'facemenu-keymap)
    (define-key facemenu-keymap "\es" 'center-line)
    (define-key facemenu-keymap "\eS" 'center-paragraph)
