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

;;slime start entry
(defslime-start sbcl  "sbcl --noinform")
(defslime-start clisp "clisp")
(defslime-start ecl   "ecl")

;;(setq inferior-lisp-program "sbcl --noinform") ; your Lisp system
;;(setq inferior-lisp-program "sbcl.exe --noinform") ; your Lisp system

;;reset slime temp directory
(setq temporary-file-directory (concat (getenv "HOME")  "/tmp"))
(unless (file-exists-p temporary-file-directory)
  (make-directory temporary-file-directory))

(require 'slime)
(slime-setup '(
               slime-scratch
               slime-editing-commands
               slime-fancy
               slime-repl
               slime-autodoc
               ))
(fset 'run-lisp 'slime)

;;set slime coding
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-startup-animation nil)

;;my slime-mode setting
(defun my-slime-mode-hook ()
  (setq tab-width 4 indent-tabs-mode nil)
  (define-key slime-mode-map (kbd "C-c <tab>") 'slime-complete-symbol)
  (define-key slime-mode-map [(tab)] 'slime-indent-and-complete-symbol))

(add-hook 'slime-mode-hook 'my-slime-mode-hook)

;;my slime-repl-mode setting
(defun my-slime-repl-mode-hook ()
  (define-key slime-repl-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments))

(add-hook 'slime-repl-mode-hook 'my-slime-repl-mode-hook)

;;hpperspec.el
(require 'hyperspec)
(setq common-lisp-hyperspec-root (concat zzemacs-path "/doc/hyperspec/"))

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

;; sawfish mode settings
;; load the first sawfish.el or sawfish.elc file found in the load-path
(require 'sawfish)
;; this tells emacs to automatically activate the sawfish-mode whenever open
;; file with "sawfishrc" or "jl" (John Lisp) suffix
(add-to-list 'auto-mode-alist '(".*sawfishrc\\'" . sawfish-mode ))
(add-to-list 'auto-mode-alist '(".*\\.jl\\'"     . sawfish-mode ))

;;connect stumpwm slime swank
(defun slime-connect-stumpwm ()
  (interactive)
  (slime-connect "127.0.0.1" 4405))

;;require paredit
(require 'paredit)
(dolist (hook
          (list
           'emacs-lisp-mode-hook
           'eval-expression-minibuffer-setup-hook
           'ielm-mode-hook
           'lisp-mode-hook
           'lisp-interaction-mode-hook
           'scheme-mode-hook
           ))
        (add-hook hook 'enable-paredit-mode))

;;Advanced highlighting of matching parentheses
(require 'mic-paren)
(paren-activate)
(setf paren-priarity 'close)

;;Provide a face for parens in lisp modes
(require 'parenface)

;;Bridge process filter
(require 'bridge)
(autoload 'install-bridge "bridge" "Install a process bridge." t)
(setq bridge-hook 
      '(lambda ()
         ;; Example options
         (setq bridge-source-insert nil) ;Don't insert in source buffer
         (setq bridge-destination-insert nil) ;Don't insert in dest buffer
         ;; Handle copy-it messages yourself
         (setq bridge-handlers
               '(("copy-it" . my-copy-handler)))))


(provide 'lisp-setting)

;; lisp-setting.el end here
