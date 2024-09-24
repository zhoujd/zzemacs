;;;; company-setting.el --- company mode file
;;https://elpa.gnu.org/packages/doc/company.html
;;https://www.emacswiki.org/emacs/CompanyMode
;;https://melpa.org/#/company

(zz/load-path "site-lisp/company-mode")
(require 'company)
(global-company-mode t)
(setq company-idle-delay nil ; menu show delay (nil for never)
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

;;company-ctags
(require 'company-ctags)
(defun zz/company-ctags ()
  "Input code from company backend using fuzzy matching."
  (interactive)
  (company-abort)
  (let* ((company-backends '(company-ctags))
         (company-ctags-fuzzy-match-p t))
    (company-complete-common)))

;;company-shell
(require 'company-shell)
(defun zz/company-shell ()
  "Input code from company backend using fuzzy matching."
  (interactive)
  (company-abort)
  (let* ((company-backends '(company-shell)))
    (company-complete-common)))

;;default `company-backends'
(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         company-yasnippet
         )))

(setq company--disabled-backends
      '(company-etags
        company-ctags
        company-shell))

(setq company-global-modes
      '(not org-mode))

;;company keys
(defkeys-map company-active-map
  ([return]    nil)
  ((kbd "RET") nil)
  ([backtab]   'delete-backward-char)
  ((kbd "C-h") 'delete-backward-char)
  ([tab]       'company-complete-selection)
  ((kbd "TAB") 'company-complete-selection))

;;global keys
(defkeys-map global-map
  ((kbd "C-c C-/") 'company-other-backend)
  ((kbd "TAB")     'company-indent-or-complete-common))

(defun zz/company-hook ()
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
(add-hook 'company-mode-hook 'zz/company-hook)


(provide 'company-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; company-setting.el --- end here
