;;Lisp programme setting

(zz/load-path "site-lisp/slime")

;;common lisp indentation
(autoload 'common-lisp-indent-function "cl-indent")
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq lisp-indent-function 'common-lisp-indent-function)
            (zz/indent if nil)
            (zz/indent when 1)
            (zz/indent unless 1)
            (zz/indent do 2)
            (zz/indent do* 2)
            (zz/indent defcommand 3)
            ))

;;emacs mode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (zz/indent if nil)
            ))

;;add common lisp configure file mode alias
(setq auto-mode-alist
   (append
    (list (cons "\\.sbclrc$"    'lisp-mode))  ; sbcl configure file
    (list (cons "\\.clisprc$"   'lisp-mode))  ; clisp configure file
    (list (cons "\\.eclrc$"     'lisp-mode))  ; ecl configure file
    (list (cons "\\.stumpwmrc$" 'lisp-mode))  ; stumpwm configure file
    (list (cons "\\.clfswmrc$"  'lisp-mode))  ; clfswm configure file
    auto-mode-alist))

;;(setq inferior-lisp-program "sbcl --noinform")
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
(zz/slime-start sbcl  'sbcl)
(zz/slime-start clisp 'clisp)
(zz/slime-start ecl   'ecl)

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
               slime-company
               ))
(fset 'run-lisp 'slime)

;;set slime coding
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-startup-animation nil)

;;my slime-repl-mode setting
(defun zz/slime-repl-mode-hook ()
  (defkeys-map slime-repl-mode-map
    ((kbd "C-c ;") 'slime-insert-balanced-comments)
    ))

(add-hook 'slime-repl-mode-hook 'zz/slime-repl-mode-hook)

;;hpperspec.el
(require 'hyperspec)
(setq common-lisp-hyperspec-root (concat zzemacs-path "/doc/hyperspec/"))

;;eldoc
(defun zz/turn-on-eldoc-mode ()
  (eldoc-mode t))
(require 'eldoc-extension)
(add-hook 'emacs-lisp-mode-hook 'zz/turn-on-eldoc-mode)
(add-hook 'lisp-inteeraction-mode-hook 'zz/turn-on-eldoc-mode)
;;allow long ElDoc messages to resize echo area display
(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")

;;adjust parens
;(require 'adjust-parens)
;(add-hook 'lisp-mode-hook
;          (lambda ()
;            (defkeys-map help-map
;              ((kbd "TAB")       'lisp-indent-adjust-parens)
;              ((kbd "<backtab>") 'lisp-dedent-adjust-parens))))

;;sawfish mode settings
(require 'sawfish)
(add-to-list 'auto-mode-alist '(".*sawfishrc\\'" . sawfish-mode ))
(add-to-list 'auto-mode-alist '(".*\\.jl\\'"     . sawfish-mode ))

;;connect stumpwm slime swank
(defun zz/slime-connect-stumpwm ()
  (interactive)
  (slime-connect "127.0.0.1" 4405))

;;require paredit
(zz/load-path "site-lisp/paredit")
(require 'paredit)
(eval-after-load 'paredit
                 '(progn
                   (defkeys-map paredit-mode-map
                     ((kbd "M-s")      nil)
                     ((kbd "<M-up>")   nil)
                     ((kbd "<M-down>") nil))))

;;provide a face for parens in lisp modes
(require 'parenface)

;;bridge process filter
(require 'bridge)
(autoload 'install-bridge "bridge" "Install a process bridge." t)
(setq bridge-hook 
      (lambda ()
         (setq bridge-source-insert nil)
         (setq bridge-destination-insert nil)))

;;elisp function help
(require 'find-func)
(find-function-setup-keys)


(provide 'lisp-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; lisp-setting.el end here
