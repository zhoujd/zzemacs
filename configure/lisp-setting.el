;;Lisp programme setting

;;only with slime
(zz-load-path "site-lisp/slime")
;; Common Lisp indentation.
(autoload 'common-lisp-indent-function "cl-indent")
(setq lisp-indent-function 'common-lisp-indent-function)

;;add common lisp configure file mode alias
(setq auto-mode-alist
   (append
    (list (cons "\\.sbclrc$" 'lisp-mode))  ;;sbcl configure file
    (list (cons "\\.eclrc$"  'lisp-mode))  ;;ecl configure file
    auto-mode-alist))

(setq slime-lisp-implementations
      '(
        (ecl   ("ecl"))
        (sbcl  ("sbcl" "--noinform") :coding-system utf-8-unix)
        (clisp ("clisp"))
        ))

;;(setq inferior-lisp-program "sbcl --noinform") ; your Lisp system
;;(setq inferior-lisp-program "sbcl.exe --noinform") ; your Lisp system

;;reset slime temp directory
(setq temporary-file-directory (concat (getenv "HOME")  "/tmp"))
(unless (file-exists-p temporary-file-directory)
  (make-directory temporary-file-directory))

(require 'slime)
(slime-setup '(slime-fancy))
(slime-setup '(slime-repl))

;;set slime coding
(setq slime-net-coding-system 'utf-8-unix)
;;my slime-mode setting
(defun my-slime-mode-hook ()
    (setq tab-width 4 indent-tabs-mode nil)
    ;;(define-key slime-mode-map [(tab)] 'slime-complete-symbol)
    (define-key slime-mode-map [(tab)] 'slime-indent-and-complete-symbol))
(add-hook 'slime-mode-hook 'my-slime-mode-hook)

;;hpperspec.el
(require 'hyperspec)

(provide 'lisp-setting)

;; lisp-setting.el end here
