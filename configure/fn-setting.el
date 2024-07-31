;;;; fn-setting.el --- key function config file
;;;

(zz:load-path "elisp")
(require 'apply-keys)

(define-fn-key (gethash "f1" fn-key-table)
  [f1]              help-map
  [S-f1]            'helm-describe-modes
  [C-f1]            'helm-descbinds
  [M-f1]            'zz:ctrl-z-help
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
  [M-f3]            'helm-projectile-ag
  (kbd "C-x <f3>")  'register-list
  (kbd "C-c <f3>")  'repeat-complex-command
  "f3 key binding")

(define-fn-key (gethash "f4" fn-key-table)
  [f4]              f4-map
  [S-f4]            'helm-recentb
  [C-f4]            'zz:helm-find
  [M-f4]            'zz:helm-grep-ag
  (kbd "C-x <f4>")  'helm-recentf
  (kbd "C-c <f4>")  'zz:recentf-files
  "f4 key binding")

(define-fn-key (gethash "f5" fn-key-table)
  [f5]              'symbol-overlay-put
  [S-f5]            'symbol-overlay-remove-all
  [C-f5]            'symbol-overlay-jump-next
  [M-f5]            'symbol-overlay-jump-prev
  (kbd "C-x <f5>")  'symbol-overlay-query-replace
  (kbd "C-c <f5>")  'symbol-overlay-mode
  "f5 key binding")

(define-fn-key (gethash "f6" fn-key-table)
  [f6]              (if-ms-windows 'zz:get-shell 'zz:get-term)
  [S-f6]            (if-ms-windows 'zz:cd-shell 'zz:cd-term)
  [C-f6]            (if-ms-windows 'multi-shell-next 'multi-term-next)
  [M-f6]            (if-ms-windows 'multi-shell-prev 'multi-term-prev)
  (kbd "C-x <f6>")  (if-ms-windows 'zz:switch-to-shell 'zz:switch-to-term)
  (kbd "C-c <f6>")  (if-ms-windows 'zz:get-shell 'zz:get-term)
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
  [f8]              'gud-gdb
  [S-f8]            'gdb
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
  (kbd "C-c <f9>")  'zz:remote-shell
  "f9 key binding")

(define-fn-key (gethash "f10" fn-key-table)
  [f10]             'helm-occur
  [S-f10]           'helm-swoop
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
  (kbd "C-x <f11>") 'which-key-mode
  (kbd "C-c <f11>") 'flymake-mode
  "f11 key binding")

(define-fn-key (gethash "f12" fn-key-table)
  [f12]             'neotree-toggle
  [S-f12]           'neotree-dir
  [C-f12]           'deadgrep
  [M-f12]           'helm-ag
  (kbd "C-x <f12>") 'zz:untabify-buffer
  (kbd "C-c <f12>") 'zz:tabify-buffer
  "f12 key binding")


(provide 'fn-setting)

;;; fn-setting.el ends here
