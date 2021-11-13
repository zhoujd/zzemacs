;;;; company-setting.el --- company mode file
;;https://www.emacswiki.org/emacs/CompanyMode#toc1
;;https://melpa.org/#/company

(zz:load-path "site-lisp/company")
(require 'company)
(global-company-mode t)
(setq company-idle-delay 0) ;; nil for not auto popup
(setq company-show-numbers t)
(setq company-minimum-prefix-length 2)

;;keys
(defkeys-map company-active-map
  ([return]    nil)
  ((kbd "RET") nil)
  ((kbd "C-h") 'delete-backward-char)
  ([tab]       'company-complete-selection)
  ((kbd "TAB") 'company-complete-selection))

;;company-mode <ctrl+tab> to open complete menu
(defkeys-map global-map
  ([(control tab)] 'company-complete))

(defun zz:company-hook ()
  (require 'color)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))
(add-hook 'company-mode-hook 'zz:company-hook)


(provide 'company-setting)

;;;; company-setting.el --- end here
