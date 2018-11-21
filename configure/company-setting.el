;;;; company-setting.el --- company mode file
;;https://www.emacswiki.org/emacs/CompanyMode#toc1

;;company-mode <ctrl+tab> to open complete menu
(zz-load-path "site-lisp/company-mode")
(require 'company)
(global-company-mode t)
(setq company-idle-delay 0) ;; nil for not auto popup
(setq company-show-numbers t)
(setq company-minimum-prefix-length 1)

(defkeys-map company-active-map
  ([return]    nil)
  ((kbd "RET") nil)
  ([tab]       'company-complete-selection)
  ((kbd "TAB") 'company-complete-selection))

(defkeys-map global-map
  ([(control tab)] 'company-complete)
  ([tab]           'company-complete-common))


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
