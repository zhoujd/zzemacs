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
  (let ((darker-fg        "#e4e4ef")
        (darker-fg+1      "#f4f4ff")
        (darker-fg+2      "#f5f5f5")
        (darker-white     "#ffffff")
        (darker-black     "#000000")
        (darker-bg-1      "#101010")
        (darker-bg        "#181818")
        (darker-bg+1      "#282828")
        (darker-bg+2      "#453d41")
        (darker-bg+3      "#484848")
        (darker-bg+4      "#52494e")
        (darker-red-1     "#c73c3f")
        (darker-red       "#f43841")
        (darker-red+1     "#ff4f58")
        (darker-green     "#73c936")
        (darker-yellow    "#ffdd33")
        (darker-brown     "#cc8c3c")
        (darker-quartz    "#95a99f")
        (darker-niagara-1 "#5f627f")
        (darker-niagara   "#96a6c8")
        (darker-wisteria  "#9e95c7"))
    (custom-set-faces
     `(company-tooltip ((t (:foreground ,darker-fg :background ,darker-bg+1))))
     `(company-tooltip-annotation ((t (:foreground ,darker-brown :background ,darker-bg+1))))
     `(company-tooltip-annotation-selection ((t (:foreground ,darker-brown :background ,darker-bg-1))))
     `(company-tooltip-selection ((t (:foreground ,darker-fg :background ,darker-bg-1))))
     `(company-tooltip-mouse ((t (:background ,darker-bg-1))))
     `(company-tooltip-common ((t (:foreground ,darker-green))))
     `(company-tooltip-common-selection ((t (:foreground ,darker-green))))
     `(company-scrollbar-fg ((t (:background ,darker-bg-1))))
     `(company-scrollbar-bg ((t (:background ,darker-bg+2))))
     `(company-preview ((t (:background ,darker-green))))
     `(company-preview-common ((t (:foreground ,darker-green :background ,darker-bg-1))))
     )))

(add-hook 'company-mode-hook 'zz:company-hook)


(provide 'company-setting)

;;;; company-setting.el --- end here
