;;Lisp programme setting

(zz-load-path "site-lisp/slime")
;;Common Lisp indentation
(autoload 'common-lisp-indent-function "cl-indent")
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq lisp-indent-function 'common-lisp-indent-function)))

;;Emacs proper cl-flet indentation
(when-emacs24-3
 (eval-after-load "cl-indent"
   '(progn
     (put 'cl-flet 'common-lisp-indent-function 
      (get 'flet 'common-lisp-indent-function))
     )))

;;add common lisp configure file mode alias
(setq auto-mode-alist
   (append
    (list (cons "\\.sbclrc$"    'lisp-mode))  ;;sbcl configure file
    (list (cons "\\.eclrc$"     'lisp-mode))  ;;ecl configure file
    (list (cons "\\.stumpwmrc$" 'lisp-mode))  ;;stumpwm configure file
    auto-mode-alist))

(setq slime-lisp-implementations
      '(
        (sbcl  ("sbcl" "--noinform") :coding-system utf-8-unix)
        (clisp ("clisp"))
        (ecl   ("ecl"))
        ))

;;(setq inferior-lisp-program "sbcl --noinform") ; your Lisp system
;;(setq inferior-lisp-program "sbcl.exe --noinform") ; your Lisp system

;;reset slime temp directory
(setq temporary-file-directory (concat (getenv "HOME")  "/tmp"))
(unless (file-exists-p temporary-file-directory)
  (make-directory temporary-file-directory))

(require 'slime)
(slime-setup '(
               slime-fancy
               slime-repl
               ))
(fset 'run-lisp 'slime)

;;set slime coding
(setq slime-net-coding-system 'utf-8-unix)
;;my slime-mode setting
(defun my-slime-mode-hook ()
  (setq tab-width 4 indent-tabs-mode nil)
  ;;(define-key slime-mode-map [(tab)] 'slime-complete-symbol)
  (define-key slime-mode-map (kbd "C-c 0") 'slime-selector)
  (define-key slime-mode-map [(tab)] 'slime-indent-and-complete-symbol))

(add-hook 'slime-mode-hook 'my-slime-mode-hook)

;;my slime-repl-mode setting
(defun my-slime-repl-mode-hook ()
  (define-key slime-repl-mode-map (kbd "C-c 0") 'slime-selector))

(add-hook 'slime-repl-mode-hook 'my-slime-repl-mode-hook)

;;hpperspec.el
(require 'hyperspec)
(setq common-lisp-hyperspec-root (concat zzemacs-path "doc/hyperspec/"))

;;eldoc
(defun turn-on-eldoc-mode () (eldoc-mode t))
(require 'eldoc-extension)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-inteeraction-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")

;;adjust parens
;(require 'adjust-parens)
;(add-hook 'lisp-mode-hook
;          (lambda ()
;            (define-key help-map (kbd "TAB") 'lisp-indent-adjust-parens)
;            (define-key help-map (kbd "<backtab>") 'lisp-dedent-adjust-parens)))

;;slime with auto-complete
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(progn
    (add-to-list 'ac-modes 'slime-repl-mode)
    (add-to-list 'ac-modes 'slime-mode)
  ))

;;connect stumpwm slime swank
(defun slime-connect-stumpwm ()
  (interactive)
  (slime-connect "127.0.0.1" 4405))

(provide 'lisp-setting)

;; lisp-setting.el end here
