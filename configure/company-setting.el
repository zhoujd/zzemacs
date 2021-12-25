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
  (custom-set-faces
   `(company-preview ((t (:foreground "#c5c8c6" ::underline t ))))
   `(company-preview-common ((t (:inherit company-preview))))
   `(company-scrollbar-bg ((t (:background "#1d1f21" :foreground "#c5c8c6"))))
   `(company-scrollbar-fg ((t (:background "#c5c8c6" :foreground "#1d1f21"))))
   `(company-tooltip ((t (:background "#1d1f21" :foreground "#c5c8c6" ))))
   `(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
   `(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
   `(company-tooltip-selection ((t (:background "#4F4F4F" :foreground "#1d1f21" ))))
   `(company-tooltip-annotation ((t (:foreground "gray31" :weight bold))))))

(add-hook 'company-mode-hook 'zz:company-hook)


(provide 'company-setting)

;;;; company-setting.el --- end here
