;;;; complete-setting.el --- complete common file


(zz-load-path "site-lisp")
(zz-load-path "elisp")

;; cedet setting
(require 'cedet)
(require 'semantic/sb)

;; select which submodes we want to activate
(mapc (lambda (MODE) (add-to-list 'semantic-default-submodes MODE))
      '(global-semantic-idle-scheduler-mode
        global-semanticdb-minor-mode
        global-semantic-idle-summary-mode
        global-semantic-mru-bookmark-mode))

;; smart complitions
(require 'semantic/ia)
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))

(global-ede-mode t)
(semantic-mode t)

;; 1:list methold in current buffer
;; 2:switch buffer in h & cpp file
(require 'eassist)
(define-key eassist-mode-map (kbd "TAB") 'eassist-jump-to-method)
(define-key eassist-mode-map (kbd "C-b") 'eassist-backspace-pressed)
(define-key eassist-mode-map (kbd "C-q") 'eassist-escape)

;; Semantic DataBase
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.d/semanticdb"))

;; semantic-ia-fast-jump fixed
(defadvice push-mark (around semantic-mru-bookmark activate)
  "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
If `semantic-mru-bookmark-mode' is active, also push a tag onto
the mru bookmark stack."
  (semantic-mrub-push semantic-mru-bookmark-ring
                      (point)
                      'mark)
  ad-do-it)

;; hippie-try-expand settings
(setq hippie-expand-try-functions-list
      '(
        yas/hippie-try-expand
        semantic-ia-complete-symbol
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs))


(defun my-indent-or-complete ()
   (interactive)
   (if (looking-at "\\>")
      (hippie-expand nil)
      (indent-for-tab-command)))

;;YASNIPPET
;;https://github.com/capitaomorte/yasnippet
(zz-load-path "site-lisp/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory (concat zzemacs-path "/site-lisp/yasnippet/snippets"))

;;company mode
(require 'company-setup)

;;auto complete
(require 'ac-setup)


(provide 'complete-setting)

;;; complete-setting.el ends here
