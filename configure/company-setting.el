;;; company-setting.el

;;company-mode <ctrl+tab> to open complete menu
(zz-load-path "site-lisp/company-mode")
(require 'company)
(global-company-mode t)
(setq company-idle-delay 0) ;; nil for not auto popup
(setq company-show-numbers t)
(setq company-minimum-prefix-length 1)

(define-key company-active-map [return]    nil)
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map [tab]       'company-complete-selection)
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(global-set-key [(control tab)] 'company-complete)
(global-set-key [tab] 'company-complete-common)

(provide 'company-setting)
