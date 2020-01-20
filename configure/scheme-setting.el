;;;; scheme programme setting
;;;

;;Support racket file
(setq auto-mode-alist
 (append
  (list
   ;; insert entries for other modes here if needed.
   (cons "\\.rkt$" 'scheme-mode))
  auto-mode-alist))

;;scheme complete
(require 'scheme-complete)
(eval-after-load 'scheme
  '(progn
    (defkeys-map scheme-mode-map
      ("\C-c=" 'geiser-mode)
      ("\t"    'scheme-complete-or-indent)
      ("\e\t"  'scheme-smart-complete))))

;;IUScheme Setup
(require 'iuscheme)
(add-hook 'inferior-scheme-mode-hook
          (lambda ()
            (defkeys-map inferior-scheme-mode-map
              ("\t"    'scheme-complete-or-indent)
              ("\e\t"  'scheme-smart-complete)
              )))

;;Balanced Setup
;(require 'balanced)
;(add-hook 'scheme-mode-hook 'balanced-on)

(add-hook 'scheme-mode-hook
          (lambda ()
            (setq lisp-indent-function 'scheme-smart-indent-function)))

;;geiser for scheme
(zz-load-path "site-lisp/geiser")
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
(add-hook 'gambit-mode-hook
          (lambda ()
            (setq lisp-indent-function 'gambit-indent-function)))

;;quack
(setq quack-remap-find-file-bindings-p nil)
(setq quack-global-menu-p nil)
(setq quack-remember-new-programs-p nil)
(setq quack-default-program "gsi -:d-")
(require 'quack)

;;racket mode
;;http://docs.racket-lang.org/guide/Emacs.html
(zz-load-path "site-lisp/racket-mode")
(require 'racket-mode)
(add-hook 'racket-mode-hook
          (lambda ()
            (defkeys-map racket-mode-map
              ((kbd "C-c r") 'racket-run))))


(provide 'scheme-setting)

;; scheme-setting.el end here
