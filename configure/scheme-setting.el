;;scheme programme setting

;;PLT scheme
;(setq scheme-program-name "mzscheme")
;;MIT scheme
;(setq scheme-program-name "mit-scheme")

;;Support racket file
(setq auto-mode-alist
 (append
  (list
   ;; insert entries for other modes here if needed.
   (cons "\\.rkt$" 'scheme-mode))
  auto-mode-alist))

;;auto-complete setting
(defvar ac-source-scheme
  '((candidates
     . #'(lambda ()
           (require 'scheme-complete)
           (all-completions ac-target (car (scheme-current-env))))))
  "Source for scheme keywords.")

;;Auto-complete-mode config
(add-hook 'scheme-mode-hook
          #'(lambda ()
              (make-local-variable 'ac-sources)
              (setq ac-sources (append ac-sources '(ac-source-scheme)))))

;;scheme complete
(require 'scheme-complete)
(eval-after-load 'scheme
  '(progn
    (define-key scheme-mode-map "\C-c=" 'geiser-mode)
    (define-key scheme-mode-map "\t"    'scheme-complete-or-indent)
    (define-key scheme-mode-map "\e\t"  'scheme-smart-complete)))

(add-hook 'scheme-mode-hook
          #'(lambda ()
              (make-local-variable 'eldoc-documentation-function)
              (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
              (eldoc-mode t)
              (setq lisp-indent-function 'scheme-smart-indent-function)))

;;geiser for scheme
(zz-load-path "site-lisp/geiser/elisp")
(require 'geiser)
(setq geiser-active-implementations '(racket))

;;Gambit-C
(autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
(autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
(add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
(add-hook 'scheme-mode-hook (function gambit-mode))
(setq gambit-highlight-color "gray")
(setq scheme-program-name "gsi -:d-")
(require 'gambit)

;;quack
(require 'quack)
(setq quack-remap-find-file-bindings-p nil)
(setq quack-global-menu-p nil)
(setq quack-remember-new-programs-p nil)
(setq quack-default-program "gsi -:d-")

(provide 'scheme-setting)

;; scheme-setting.el end here
