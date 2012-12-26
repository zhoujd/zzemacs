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
(slime-setup '(slime-fancy))
;;set slime coding
(setq slime-net-coding-system 'utf-8-unix)
;;my slime-mode setting
(defun my-slime-mode-hook ()
    (setq tab-width 4 indent-tabs-mode nil)
    ;;(define-key slime-mode-map [(tab)] 'slime-complete-symbol)
    (define-key slime-mode-map [(tab)] 'slime-indent-and-complete-symbol))
(add-hook 'slime-mode-hook 'my-slime-mode-hook)


;;;;scheme setting here
;;PLT scheme
(setq scheme-program-name "mzscheme")
;;MIT scheme
;(setq scheme-program-name "mit-scheme")

(setq auto-mode-alist
 (append
  (list
   ;; insert entries for other modes here if needed.
   (cons "\\.rkt$" 'scheme-mode))
  auto-mode-alist))

;; eldoc
(require 'eldoc-extension)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-inteeraction-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")

;;auto-complete setting
(defvar ac-source-scheme
  '((candidates
     . (lambda ()
         (require 'scheme-complete)
         (all-completions ac-target (car (scheme-current-env))))))
  "Source for scheme keywords.")

;;Auto-complete-mode config
(add-hook 'scheme-mode-hook
          '(lambda ()
             (make-local-variable 'ac-sources)
             (setq ac-sources (append ac-sources '(ac-source-scheme)))))

;;scheme complete
(autoload 'scheme-smart-complete "scheme-complete" nil t)
(eval-after-load 'scheme
  '(progn (define-key scheme-mode-map "\t"   'scheme-complete-or-indent)
          (define-key scheme-mode-map "\e\t" 'scheme-smart-complete)))

(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
            (eldoc-mode t)
            (setq lisp-indent-function 'scheme-smart-indent-function)))

;;quack
(require 'quack)

;;geiser for scheme
(zz-load-path "site-lisp/geiser/elisp")
(require 'geiser)

(provide 'lisp-setting)

;; lisp-setting.el end here
