;;Lisp programme setting
;;only with slime
(zz-load-path "site-lisp/slime")
;; Common Lisp indentation.
(autoload 'common-lisp-indent-function "cl-indent")
(setq lisp-indent-function 'common-lisp-indent-function)

(setq slime-lisp-implementations
	'(
      (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
      (clisp ("clisp"))
       ))

;;(setq inferior-lisp-program "sbcl --noinform") ; your Lisp system
;;(setq inferior-lisp-program "sbcl.exe --noinform") ; your Lisp system

(require 'slime)
(slime-setup '(slime-repl))

;;set slime coding
(setq slime-net-coding-system 'utf-8-unix)

;;my slime-mode setting
(defun my-slime-mode-hook ()
    (setq tab-width 4 indent-tabs-mode nil)
    ;;(define-key slime-mode-map [(tab)] 'slime-complete-symbol)
    (define-key slime-mode-map [(tab)] 'slime-indent-and-complete-symbol))

(add-hook 'slime-mode-hook 'my-slime-mode-hook)

;; scheme complete
(autoload 'scheme-smart-complete "scheme-complete" nil t)
 (eval-after-load 'scheme
  '(define-key scheme-mode-map "\e\t" 'scheme-smart-complete))

(setq scheme-program-name "mzscheme")
(setq auto-mode-alist
 (append
  (list
   ;; insert entries for other modes here if needed.
   (cons "\\.rkt$" 'scheme-mode))
  auto-mode-alist))

(provide 'lisp-setting)

;; lisp-setting.el end here
