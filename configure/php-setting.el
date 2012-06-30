;;php programme setting

(require 'php-mode)

;;(require 'php-mode-new)
;;(setq php-manual-path "c:/php-manual")

;; $ cd /usr/share/doc/php-manual/en/html
;; $ ls -1 function*.html \
;; | sed -e 's/^function\.\([-a-zA-Z_0-9]*\)\.html/\1/' \
;; | tr - _ \
;; > ~/.emacs.d/php/php-completion-file

(setq php-completion-file "~/.emacs.d/php/php-completion-file")

;;To use abbrev-mode, add lines like this:
(add-hook 'php-mode-hook  '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

(zz-load-path "site-lisp/geben")
(autoload 'geben "geben" "PHP Debugger on Emacs" t)

(speedbar-add-supported-extension "\\.php$")

(provide 'php-setting)

;;; php-setting.el end here
