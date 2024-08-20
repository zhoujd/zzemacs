;;;; company-setting.el --- company mode file
;;https://www.emacswiki.org/emacs/CompanyMode
;;https://melpa.org/#/company

(zz:load-path "site-lisp/company")
(require 'company)
(global-company-mode t)
(setq company-idle-delay 0.1 ; menu show delay (nil for never)
      company-show-numbers t
      company-complete-number t
      company-dabbrev-ignore-case t
      company-dabbrev-downcase nil
      company-dabbrev-other-buffers t
      company-dabbrev-code-other-buffers t
      company-text-icons-mapping nil        ; don't show icon
      company-vscode-icons-mapping nil      ; don't show icon
      company-selection-wrap-around t
      company-minimum-prefix-length 2)

;;set default `company-backends'
(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         company-yasnippet
         )))

(setq company--disabled-backends
      '(company-etags))

(setq company-global-modes
      '(not org-mode))

;;code completion and snippets
;;https://robert.kra.hn/posts/rust-emacs-setup/
(defun zz:check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun zz:do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun zz:tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (zz:do-yas-expand)))
        (if (zz:check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(defun zz:company-yasnippet-or-completion ()
  (interactive)
  (or (zz:do-yas-expand)
      (company-complete-common)))

;;company keys
(defkeys-map company-active-map
  ([return]    nil)
  ((kbd "RET") nil)
  ((kbd "C-h") 'delete-backward-char)
  ([tab]       'company-complete-selection)
  ((kbd "TAB") 'company-complete-selection))

;;global keys
(defkeys-map global-map
  ((kbd "TAB") 'zz:tab-indent-or-complete))

(defun zz:company-hook ()
  (require 'color)
  (let ((darker-fg    "#e4e4ef")
        (darker-fg+1  "#f4f4ff")
        (darker-fg+2  "#f5f5f5")
        (darker-bg-1  "#101010")
        (darker-bg    "#181818")
        (darker-bg+1  "#282828")
        (darker-bg+2  "#453d41")
        (darker-green "#73c936")
        (darker-brown "#cc8c3c"))
    (custom-set-faces
     `(company-tooltip ((t (:foreground ,darker-fg :background ,darker-bg+1))))
     `(company-tooltip-annotation ((t (:foreground ,darker-brown :background ,darker-bg+1))))
     `(company-tooltip-annotation-selection ((t (:foreground ,darker-brown :background ,darker-bg-1))))
     `(company-tooltip-selection ((t (:foreground ,darker-brown :background ,darker-bg-1))))
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
