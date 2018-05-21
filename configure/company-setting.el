;;;; company-setting.el --- company mode file
;;https://www.emacswiki.org/emacs/CompanyMode#toc1

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


(require 'color)
(let ((bg (face-attribute 'default :background)))
 (custom-set-faces
  `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
  `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
  `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
  `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
  `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))


(provide 'company-setting)

;;;; company-setting.el --- end here
