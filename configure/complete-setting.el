;;;; complete-setting.el --- complete common file


(zz-load-path "site-lisp")

;; cedet
(mapc (lambda (mode) (add-to-list 'semantic-default-submodes mode))
      '(global-semanticdb-minor-mode
        global-semantic-idle-summary-mode
        global-semantic-mru-bookmark-mode))

(global-ede-mode t)
(semantic-mode t)

(defun my:add-semantic-to-autocomplete() 
  (add-to-list 'ac-sources 'ac-source-semantic))

(mapc
 (lambda (mode)
   (add-hook mode 'my:add-semantic-to-autocomplete))
 '(
   c-mode-common-hook
   ))

;; list methold in current buffer
;; switch buffer in h & cpp file
(require 'eassist)
(define-key eassist-mode-map (kbd "TAB") 'eassist-jump-to-method)
(define-key eassist-mode-map (kbd "C-b") 'eassist-backspace-pressed)
(define-key eassist-mode-map (kbd "C-q") 'eassist-escape)

;; Semantic DataBase
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.d/semanticdb"))

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

(defun indent-or-complete ()
   "complete if point is at end of a word, otherwise indent line"
   (interactive)
   (if (looking-at "\\>")
       (hippie-expand nil)
       (indent-for-tab-command)))

;;YASNIPPET
;;https://github.com/capitaomorte/yasnippet
(zz-load-path "site-lisp/yasnippet")
(zz-load-path "site-lisp/yasnippet-snippets")
(require 'yasnippet-snippets)
(yas-global-mode t)


(provide 'complete-setting)

;;; complete-setting.el ends here
