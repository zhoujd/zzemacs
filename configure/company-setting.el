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


(provide 'company-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; company-setting.el --- end here
