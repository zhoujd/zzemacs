;;Lisp programme setting

;;only with slime
(zz-load-path "site-lisp/slime")
;; Common Lisp indentation.
(autoload 'common-lisp-indent-function "cl-indent")
(setq lisp-indent-function 'common-lisp-indent-function)

(setq slime-lisp-implementations
      '(
        (sbcl  ("sbcl" "--noinform") :coding-system utf-8-unix)
        (clisp ("clisp"))
        (ecl   ("ecl"))
        ))

;;(setq inferior-lisp-program "sbcl --noinform") ; your Lisp system
;;(setq inferior-lisp-program "sbcl.exe --noinform") ; your Lisp system

(require 'slime)
(slime-setup '(slime-fancy))
(slime-setup '(slime-repl))

;;set slime coding
(setq slime-net-coding-system 'utf-8-unix)
;;my slime-mode setting
(defun zz-slime-mode-hook ()
    (setq tab-width 4 indent-tabs-mode nil)
    ;;(define-key slime-mode-map [(tab)] 'slime-complete-symbol)
    (define-key slime-mode-map [(tab)] 'slime-indent-and-complete-symbol))
(add-hook 'slime-mode-hook 'zz-slime-mode-hook)

;;hpperspec.el
(require 'hyperspec)

(provide 'lisp-setting)

;; lisp-setting.el end here
