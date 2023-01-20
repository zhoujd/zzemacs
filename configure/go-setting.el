;;;;go programe setting
;; get the following packages ("M-x package-list-packages"):
;;     go-mode
;;     go-eldoc
;;     company-mode
;;     company-go
;; get the following go programs (run each line in your shell):
;;     go install golang.org/x/tools/cmd/godoc@latest
;;     go install golang.org/x/tools/cmd/goimports@latest
;;     go install github.com/rogpeppe/godef@latest
;;     go install github.com/nsf/gocode@latest

;; go-mode
(zz:load-path "site-lisp/go-mode")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; go-guru
(require 'go-guru)
(add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)

;; go-eldoc
(zz:load-path "site-lisp/go-eldoc")
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; godoc path
(defvar zz:go-path
  (list
   (format "%s/go/bin" (getenv "HOME"))
   ))
(mapc #'zz:add-os-path zz:go-path)

;; company-go
;(require 'company-go)
;(add-hook 'go-mode-hook 'company-mode)
;(defun zz:company-go-hook ()
;  (set (make-local-variable 'company-backends) '(company-go)))
;(add-hook 'go-mode-hook 'zz:company-go-hook)

(defun zz:go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "C-c m") 'gofmt)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark))
(add-hook 'go-mode-hook 'zz:go-mode-hook)

(defun zz:go-indent4 ()
  (setq tab-width 4)
  (setq indent-tabs-mode 1))

(defun zz:go-indent2 ()
  (setq tab-width 2)
  (setq standard-indent 2)
  (setq indent-tabs-mode nil))

(add-hook 'go-mode-hook 'zz:go-indent4)

;; enable lsp-mode
(require 'lsp-go)
(add-hook 'go-mode-hook 'lsp-deferred)


(provide 'go-setting)

;;;; go-setting.el end here
