;;;;go programe setting
;;;

;; get the following packages ("M-x package-list-packages"):
;;     go-mode
;;     go-eldoc
;;     company-mode
;;     company-go
;; get the following go programs (run each line in your shell):
;;     go get golang.org/x/tools/cmd/godoc
;;     go get golang.org/x/tools/cmd/goimports
;;     go get github.com/rogpeppe/godef
;;     go get github.com/nsf/gocode

;; go-mode
(zz-load-path "site-lisp/go-mode")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; go-eldoc
(zz-load-path "site-lisp/go-eldoc")
(require 'go-eldoc)

;; godoc path
(defvar zz:go-path
  (list
   (format "%s/go/bin" (getenv "HOME"))
   ))
(mapc #'zz:add-os-path zz:go-path)

;; company-go
(require 'company-go)

(defun zz:go-mode-hook ()
  ;; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;(local-set-key (kbd "C-c m") 'gofmt)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (set (make-local-variable 'company-backends) '(company-go)))

(add-hook 'go-mode-hook 'zz:go-mode-hook)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'company-mode)

(provide 'go-setting)

;;;; go-setting.el end here
