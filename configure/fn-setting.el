;;;; fn-setting.el --- key function config file
;;;

;;define fn key setting
(define-fn-key (gethash "f1" fn-key-table)
    [f1]              nil
    [S-f1]            'planner-create-task-from-buffer
    [C-f1]            'session-save
    [M-f1]            'session-restore
    (kbd "C-x <f1>")  nil
    (kbd "C-x <f1>")  nil
    )

(define-fn-key (gethash "f2" fn-key-table)
    [f2]              'bc-next
    [S-f2]            'bc-previous
    [C-f2]            'bc-set
    [M-f2]            'bc-list
    (kbd "C-x <f2>")  nil
    (kbd "C-c <f2>")  nil
    )

(define-fn-key (gethash "f3" fn-key-table)
    [f3]              'my-last-buffer-go
    [S-f3]            'list-bookmarks
    [C-f3]            'bc-local-next
    [M-f3]            'bc-local-previous
    (kbd "C-x <f3>")  'register-list
    (kbd "C-c <f3>")  'repeat-complex-command
    )

(define-fn-key (gethash "f4" fn-key-table)
    [f4]              'kill-this-buffer
    [S-f4]            'undo-kill-buffer
    [C-f4]            'highlight-symbol-next
    [M-f4]            'highlight-symbol-prev
    (kbd "C-x <f4>")  'recentf-open-files
    (kbd "C-c <f4>")  'recentf-open-files-compl
    )

(define-fn-key (gethash "f5" fn-key-table)
    [f5]              'speedbar-get-focus
    [S-f5]            'sr-speedbar-toggle
    [C-f5]            'line-to-top-of-window
    [M-f5]            'etags-stack-show
    (kbd "C-x <f5>")  nil
    (kbd "C-c <f5>")  nil
    )

(define-fn-key (gethash "f6" fn-key-table)
    [f6]              (if-ms-windows 'get-local-shell 'get-term)
    [S-f6]            (if-ms-windows 'get-local-curr-shell 'multi-term-dedicated-toggle)
    [C-f6]            (if-ms-windows 'multi-shell-next 'multi-term-next)
    [M-f6]            (if-ms-windows 'multi-shell-prev 'multi-term-prev)
    
    (kbd "C-x <f6>")  (if-ms-windows 'switch-to-shell 'switch-to-term)
    (kbd "C-c <f6>")  (unless-ms-windows 'switch-term-and-text)
    )

(define-fn-key (gethash "f7" fn-key-table)
    [f7]              'compile
    [S-f7]            'switch-to-compilation
    [C-f7]            'next-error
    [M-f7]            'previous-error
    (kbd "C-x <f7>")  nil
    (kbd "C-c <f7>")  nil
    )

(define-fn-key (gethash "f8" fn-key-table)
    [f8]              'gdb
    [S-f8]            'gud-kill
    [C-f8]            'gdb-restore-windows
    [M-f8]            'gdb-many-windows
    (kbd "C-x <f8>")  'gdb-use-separate-io
    (kbd "C-c <f8>")  'gud-tooltip-mode
    )

(define-fn-key (gethash "f9" fn-key-table)
    [f9]              (lambda () (interactive)  (start-shell "*shell*"))
    [S-f9]            'multi-shell-new
    [C-f9]            'switch-to-scratch
    [M-f9]            'popup-term
    (kbd "C-x <f9>")  'switch-to-shell
    (kbd "C-c <f9>")  'eshell
    )

(define-fn-key (gethash "f10" fn-key-table)
    [f10]             nil
    [S-f10]           'tool-bar-mode
    [C-f10]           'my-toggle-maxframe
    [M-f10]           'my-toggle-fullscreen
    (kbd "C-x <f10>") 'scroll-bar-mode
    (kbd "C-c <f10>") 'tabbar-mode
    )

(define-fn-key (gethash "f11" fn-key-table)
    [f11]             'linum-mode
    [S-f11]           'fci-mode
    [C-f11]           'hl-line-mode
    [M-f11]           'blank-mode
    (kbd "C-x <f11>") 'my-unicad-switch
    (kbd "C-c <f11>") 'my-os-file-switch
    )

(define-fn-key (gethash "f12" fn-key-table)
    [f12]             'find-grep
    [S-f12]           'rgrep
    [C-f12]           'find-name-dired
    [M-f12]           'my-c-rgrep
    (kbd "C-x <f12>") 'my-occur
    (kbd "C-c <f12>") 'my-woman-at-point
    )

(provide 'fn-setting)

;;; fn-setting.el ends here
