;;;; fn-setting.el --- key function config file
;;;

(zz:load-path "elisp")
(require 'apply-keys)

;;fn-key-table
(defvar fn-key-table
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "f1"  "1" hash)
    (puthash "f2"  "2" hash)
    (puthash "f3"  "3" hash)
    (puthash "f4"  "4" hash)
    (puthash "f5"  "5" hash)
    (puthash "f6"  "6" hash)
    (puthash "f7"  "7" hash)
    (puthash "f8"  "8" hash)
    (puthash "f9"  "9" hash)
    (puthash "f10" "0" hash)
    (puthash "f11" "-" hash)
    (puthash "f12" "=" hash)
    hash)
  "fn-key-table")

(defun zz:def-fn-keys (fn-sym fn)
  (when (and fn-sym fn)
    (define-key global-map    fn-sym    fn)
    (define-key help-fn-map   fn-sym    fn)
    (define-key fn-map        fn-name   fn)))

(defun zz:def-s-fn-keys (s-fn-sym s-fn)
  (when (and s-fn-sym s-fn)
    (define-key global-map    s-fn-sym  s-fn)
    (define-key help-fn-map   s-fn-sym  s-fn)
    (define-key shift-fn-map  fn-name   s-fn)))

(defun zz:def-c-fn-keys (c-fn-sym c-fn)
  (when (and c-fn-sym c-fn)
    (define-key global-map    c-fn-sym  c-fn)
    (define-key help-fn-map   c-fn-sym  c-fn)
    (define-key ctrl-fn-map   fn-name   c-fn)))

(defun zz:def-m-fn-keys (m-fn-sym m-fn)
  (when (and m-fn-sym m-fn)
    (define-key global-map    m-fn-sym  m-fn)
    (define-key help-fn-map   m-fn-sym  m-fn)
    (define-key meta-fn-map   fn-name   m-fn)))

(defun zz:def-cx-fn-keys (ctrl-x-fn-sym ctrl-x-fn)
  (when (and ctrl-x-fn-sym ctrl-x-fn)
    (define-key global-map    ctrl-x-fn-sym  ctrl-x-fn)
    (define-key help-fn-map   ctrl-x-fn-sym  ctrl-x-fn)
    (define-key ctrl-x-fn-map fn-name        ctrl-x-fn)))

(defun zz:def-cc-fn-keys (ctrl-c-fn-sym ctrl-c-fn)
  (when (and ctrl-c-fn-sym ctrl-c-fn)
    (define-key global-map    ctrl-c-fn-sym  ctrl-c-fn)
    (define-key help-fn-map   ctrl-c-fn-sym  ctrl-c-fn)
    (define-key ctrl-c-fn-map fn-name        ctrl-c-fn)))

;;group define fn key
(defun define-fn-key (fn-name fn-sym fn s-fn-sym s-fn
                              c-fn-sym c-fn m-fn-sym m-fn
                              ctrl-x-fn-sym ctrl-x-fn
                              ctrl-c-fn-sym ctrl-c-fn &optional doc)
  (zz:def-fn-keys fn-sym fn)
  (zz:def-s-fn-keys s-fn-sym s-fn)
  (zz:def-c-fn-keys c-fn-sym c-fn)
  (zz:def-m-fn-keys m-fn-sym m-fn)
  (zz:def-cx-fn-keys ctrl-x-fn-sym ctrl-x-fn)
  (zz:def-cc-fn-keys ctrl-c-fn-sym ctrl-c-fn))

;;define fn key setting
(define-fn-key (gethash "f1" fn-key-table)
  [f1]              help-map
  [S-f1]            'zz:evil-toggle
  [C-f1]            'magit-status
  [M-f1]            'monky-status
  (kbd "C-x <f1>")  nil
  (kbd "C-c <f1>")  nil
  "f1 key binding")

(define-fn-key (gethash "f2" fn-key-table)
  [f2]              'bm-toggle
  [S-f2]            'bm-show-all
  [C-f2]            'bm-next
  [M-f2]            'bm-previous
  (kbd "C-x <f2>")  'bm-remove-all-all-buffers
  (kbd "C-c <f2>")  'bm-remove-all-current-buffer
  "f2 key binding")

(define-fn-key (gethash "f3" fn-key-table)
  [f3]              'zz:last-buffer-go
  [S-f3]            'bookmark-bmenu-list
  [C-f3]            'helm-projectile-find-file
  [M-f3]            'helm-projectile-grep
  (kbd "C-x <f3>")  'register-list
  (kbd "C-c <f3>")  'repeat-complex-command
  "f3 key binding")

(define-fn-key (gethash "f4" fn-key-table)
  [f4]              f4-map
  [S-f4]            'zz:undo-kill-buffer
  [C-f4]            'zz:helm-find
  [M-f4]            'zz:helm-grep-ag
  (kbd "C-x <f4>")  'helm-recentf
  (kbd "C-c <f4>")  'zz:recentf-open-files-compl
  "f4 key binding")

(define-fn-key (gethash "f5" fn-key-table)
  [f5]              'highlight-symbol
  [S-f5]            'highlight-symbol-remove-all
  [C-f5]            'highlight-symbol-next
  [M-f5]            'highlight-symbol-prev
  (kbd "C-x <f5>")  'highlight-symbol-query-replace
  (kbd "C-c <f5>")  'highlight-symbol-list-all
  "f5 key binding")

(define-fn-key (gethash "f6" fn-key-table)
  [f6]              (if-ms-windows 'zz:get-shell 'zz:get-term)
  [S-f6]            (if-ms-windows 'zz:cd-shell 'multi-term-dedicated-toggle)
  [C-f6]            (if-ms-windows 'multi-shell-next 'multi-term-next)
  [M-f6]            (if-ms-windows 'multi-shell-prev 'multi-term-prev)
  (kbd "C-x <f6>")  (if-ms-windows 'zz:switch-to-shell 'zz:switch-to-term)
  (kbd "C-c <f6>")  (if-ms-windows 'zz:get-shell 'ansi-term)
  "f6 key binding")

(define-fn-key (gethash "f7" fn-key-table)
  [f7]              'compile
  [S-f7]            'zz:switch-to-compilation
  [C-f7]            'next-error
  [M-f7]            'previous-error
  (kbd "C-x <f7>")  'toggle-case-fold-search
  (kbd "C-c <f7>")  'toggle-text-mode-auto-fill
  "f7 key binding")

(define-fn-key (gethash "f8" fn-key-table)
  [f8]              'gdb
  [S-f8]            'gud-stop-subjob
  [C-f8]            'gdb-restore-windows
  [M-f8]            'gud-refresh
  (kbd "C-x <f8>")  'gdb-many-windows
  (kbd "C-c <f8>")  'gud-tooltip-mode
  "f8 key binding")

(define-fn-key (gethash "f9" fn-key-table)
  [f9]              (zz:quick-shell zz:shell "*shell*")
  [S-f9]            (if-ms-windows 'zz:get-linux-shell 'zz:get-shell)
  [C-f9]            'zz:switch-to-scratch
  [M-f9]            'zz:popup-term
  (kbd "C-x <f9>")  'zz:switch-to-shell
  (kbd "C-c <f9>")  (if-ms-windows 'eshell 'zz:remote-shell)
  "f9 key binding")

(define-fn-key (gethash "f10" fn-key-table)
  [f10]             'helm-occur
  [S-f10]           'helm-multi-swoop
  [C-f10]           'whitespace-cleanup
  [M-f10]           'whitespace-cleanup-region
  (kbd "C-x <f10>") 'menu-bar-open
  (kbd "C-c <f10>") 'hide-mode-line
  "f10 key binding")

(define-fn-key (gethash "f11" fn-key-table)
  [f11]             'linum-mode
  [S-f11]           'blank-mode
  [C-f11]           'hl-line-mode
  [M-f11]           'fci-mode
  (kbd "C-x <f11>") 'tool-bar-mode
  (kbd "C-c <f11>") 'scroll-bar-mode
  "f11 key binding")

(define-fn-key (gethash "f12" fn-key-table)
  [f12]             'find-name-dired
  [S-f12]           'helm-locate
  [C-f12]           'rgrep
  [M-f12]           'lgrep
  (kbd "C-x <f12>") 'zz:untabify-buffer
  (kbd "C-c <f12>") 'zz:tabify-buffer
  "f12 key binding")

(keys-unset-to-map
 help-fn-map
 (list
  [f1]
  [f4]
  ))


(provide 'fn-setting)

;;; fn-setting.el ends here
