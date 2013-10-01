;;;; fn-setting.el --- key function config file
;;;

;;;;function key setting on console
;;;keymap:zz/esc-fn-map        f1-f2  + 1/= <===> esc + f1/f12
;;;keymap:zz/f1-fn-map         f1-f3  + 1/= <===> f1  + f1/f12
;;;keymap:zz/f4-fn-map         f1-f4  + 1/= <===> f4  + f1/f12

;;;keymap:zz/ctr-x-fn-map      f1-f5  + 1/= <===> ctrl-x + f1/f12
;;;keymap:zz/ctr-c-fn-map      f1-f6  + 1/= <===> ctrl-c + f1/f12

;;;keymap:zz/fn-map            f1-f9  + 1/= <===> f1/f12
;;;keymap:zz/shift-fn-map      f1-f10 + 1/= <===> shift   + f1/f12
;;;keymap:zz/ctrl-fn-map       f1-f11 + 1/= <===> control + f1/f12
;;;keymap:zz/alt-fn-map        f1-f12 + 1/= <===> alt     + f1/f12

;;f1-f9 => f1/f12
(defvar zz/fn-map (make-sparse-keymap) "f1-f9 => f1/f12")
(define-key help-map [f9]  zz/fn-map)

;;f1-f10 => S-f1/f12
(defvar zz/shift-fn-map (make-sparse-keymap) "f1-f10 => S-f1/f12")
(define-key help-map [f10] zz/shift-fn-map)

;;f1-f11 => C-f1/f12
(defvar zz/ctrl-fn-map (make-sparse-keymap) "f1-f11 => C-f1/f12")
(define-key help-map [f11] zz/ctrl-fn-map)

;;f1-f12 => M-f1/f12
(defvar zz/alt-fn-map (make-sparse-keymap) "f1-f12 => M-f1/f12")
(define-key help-map [f12] zz/alt-fn-map)

;;f1-f5 => ctrl-x f1/f12
(defvar zz/ctrl-x-fn-map (make-sparse-keymap) "f1-f5 => ctrl-x f1/f12")
(define-key help-map (kbd "<f5>") zz/ctrl-x-fn-map)

;;f1-f5 => ctrl-c f1/f12
(defvar zz/ctrl-c-fn-map (make-sparse-keymap) "f1-f5 => ctrl-c f1/f12")
(define-key help-map (kbd "<f6>") zz/ctrl-c-fn-map)

;;define fn key setting
(define-fn-key "1"
    [f1]              nil
    [S-f1]            'planner-create-task-from-buffer
    [C-f1]            'session-save
    [M-f1]            'session-restore
    (kbd "C-x <f1>")  nil
    (kbd "C-x <f1>")  nil
    )

(define-fn-key "2"
    [f2]              'bc-next
    [S-f2]            'bc-previous
    [C-f2]            'bc-set
    [M-f2]            'bc-list
    (kbd "C-x <f2>")  nil
    (kbd "C-c <f2>")  nil
    )

(define-fn-key "3"
    [f3]              'my-last-buffer-go
    [S-f3]            'list-bookmarks
    [C-f3]            'bc-local-next
    [M-f3]            'bc-local-previous
    (kbd "C-x <f3>")  'my-occur
    (kbd "C-c <f3>")  'my-woman-at-point
    )

(define-fn-key "4"
    [f4]              'kill-this-buffer
    [S-f4]            'undo-kill-buffer
    [C-f4]            'highlight-symbol-next
    [M-f4]            'highlight-symbol-prev
    (kbd "C-x <f4>")  'recentf-open-files
    (kbd "C-c <f4>")  'recentf-open-files-compl
    )

(define-fn-key "5" 
    [f5]              'speedbar-get-focus
    [S-f5]            'sr-speedbar-toggle
    [C-f5]            'line-to-top-of-window
    [M-f5]            'etags-stack-show
    (kbd "C-x <f5>")  nil
    (kbd "C-c <f5>")  nil
    )

(define-fn-key "6"
    [f6]              (if-ms-windows 'multi-shell-new 'get-term)
    [S-f6]            (if-ms-windows 'multi-shell-current-directory 'multi-term-dedicated-toggle)
    [C-f6]            (if-ms-windows 'multi-shell-next 'multi-term-next)
    [M-f6]            (if-ms-windows 'multi-shell-prev 'multi-term-prev)
    (kbd "C-x <f6>")  (if-ms-windows 'switch-to-shell 'switch-to-term)
    (kbd "C-c <f6>")  (unless-ms-windows 'switch-term-and-text)
    )

(define-fn-key "7"
    [f7]              'compile
    [S-f7]            'switch-to-compilation
    [C-f7]            'next-error
    [M-f7]            'previous-error
    (kbd "C-x <f7>")  nil
    (kbd "C-c <f7>")  nil
    )

(define-fn-key "8"
    [f8]              'gdb
    [S-f8]            'gud-kill
    [C-f8]            'gdb-restore-windows
    [M-f8]            'gdb-many-windows
    (kbd "C-x <f8>")  'gdb-use-separate-io
    (kbd "C-c <f8>")  'gud-tooltip-mode
    )

(define-fn-key "9"
    [f9]              (lambda () (interactive)  (start-shell "*shell*"))
    [S-f9]            'multi-shell-new
    [C-f9]            'switch-to-scratch
    [M-f9]            'popup-term
    (kbd "C-x <f9>")  'switch-to-shell
    (kbd "C-c <f9>")  'eshell
    )

(define-fn-key "0"
    [f10]              nil
    [S-f10]            'tool-bar-mode
    [C-f10]            'my-toggle-maxframe
    [M-f10]            'my-toggle-fullscreen
    (kbd "C-x <f10>")  'scroll-bar-mode
    (kbd "C-c <f10>")  'tabbar-mode
    )

(define-fn-key "-"
    [f11]              'linum-mode
    [S-f11]            'fci-mode
    [C-f11]            'hl-line-mode
    [M-f11]            'blank-mode
    (kbd "C-x <f11>")  nil
    (kbd "C-c <f11>")  nil
    )

(define-fn-key "="
    [f12]              'find-grep
    [S-f12]            'rgrep
    [C-f12]            'find-name-dired
    [M-f12]            'my-c-rgrep
    (kbd "C-x <f12>")  'my-unicad-switch
    (kbd "C-c <f12>")  'my-os-file-switch
    )

(provide 'fn-setting)

;;; fn-setting.el ends here
