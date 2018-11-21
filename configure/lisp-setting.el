;;Lisp programme setting

(zz-load-path "site-lisp/slime")

;;Common Lisp indentation
(autoload 'common-lisp-indent-function "cl-indent")
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq lisp-indent-function 'common-lisp-indent-function)
            (put 'if 'lisp-indent-function nil)
            (put 'when 'lisp-indent-function 1)
            (put 'unless 'lisp-indent-function 1)
            (put 'do 'lisp-indent-function 2)
            (put 'do* 'lisp-indent-function 2)
            ))

;;add common lisp configure file mode alias
(setq auto-mode-alist
   (append
    (list (cons "\\.sbclrc$"    'lisp-mode))  ;;sbcl configure file
    (list (cons "\\.clisprc$"   'lisp-mode))  ;;clisp configure file
    (list (cons "\\.eclrc$"     'lisp-mode))  ;;ecl configure file
    (list (cons "\\.stumpwmrc$" 'lisp-mode))  ;;stumpwm configure file
    (list (cons "\\.clfswmrc$"  'lisp-mode))  ;;clfswm configure file
    auto-mode-alist))

;;(setq inferior-lisp-program "sbcl --noinform") ;; your Lisp system
(setq slime-lisp-implementations
      (if-ms-windows
       '(
         (clisp ("clisp") :coding-system utf-8-unix)
         (sbcl  ("sbcl" "--noinform") :coding-system utf-8-unix) 
         (ecl   ("ecl"))
         )
       '(
         (sbcl  ("sbcl" "--noinform") :coding-system utf-8-unix)
         (clisp ("clisp") :coding-system utf-8-unix)
         (ecl   ("ecl"))
         )
       ))

;;slime start entry
(defslime-start sbcl  'sbcl)
(defslime-start clisp 'clisp)
(defslime-start ecl   'ecl)

;;reset slime temp directory
;(setq temporary-file-directory (concat (getenv "HOME")  "/tmp"))
;(unless (file-exists-p temporary-file-directory)
;  (make-directory temporary-file-directory))

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

;;my slime-repl-mode setting
(defun my:slime-repl-mode-hook ()
  (defkeys-map slime-repl-mode-map
    ((kbd "C-c ;") 'slime-insert-balanced-comments)
    ))

(add-hook 'slime-repl-mode-hook 'my:slime-repl-mode-hook)

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
(add-hook 'lisp-mode-hook
          (lambda ()
            (defkeys help-map
              ((kbd "TAB")       'lisp-indent-adjust-parens)
              ((kbd "<backtab>") 'lisp-dedent-adjust-parens))))

;;slime with auto-complete
(defvar slime-ac-flag t "flag for slime with auto complete cowork")
(when slime-ac-flag
  (require 'ac-slime)
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (eval-after-load "auto-complete"
                   '(progn
                     (add-to-list 'ac-modes 'slime-repl-mode)
                     (add-to-list 'ac-modes 'slime-mode)
                     )))

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
(zz-load-path "site-lisp/paredit")
(require 'paredit)
(eval-after-load 'paredit
                 '(progn
                   (defkeys-map paredit-mode-map
                     ((kbd "M-s")      nil)
                     ((kbd "<M-up>")   nil)
                     ((kbd "<M-down>") nil))))

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
         (setq bridge-handlers '(("copy-it" . my:copy-handler)))))

;;elisp function help
(require 'find-func)
(find-function-setup-keys)


(provide 'lisp-setting)

;; lisp-setting.el end here
