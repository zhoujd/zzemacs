;;;; fn-setting.el --- key function config file
;;;

;;;;function key setting on console
;;;keymap:zz/esc-fn-map        f1-f2  + 1/= <===> esc + f1/f12
;;;keymap:zz/f1-fn-map         f1-f3  + 1/= <===> f1  + f1/f12
;;;keymap:zz/f4-fn-map         f1-f4  + 1/= <===> f4  + f1/f12

;;;keymap:zz/ctr-x-fn-map      f1-f7  + 1/= <===> ctrl-x + f1/f12
;;;keymap:zz/ctr-c-fn-map      f1-f8  + 1/= <===> ctrl-c + f1/f12

;;;keymap:zz/fn-map            f1-f9  + 1/= <===> f1/f12
;;;keymap:zz/shift-fn-map      f1-f10 + 1/= <===> shift   + f1/f12
;;;keymap:zz/ctrl-fn-map       f1-f11 + 1/= <===> control + f1/f12
;;;keymap:zz/meta-fn-map       f1-f12 + 1/= <===> alt     + f1/f12

;;f1-f7 => ctrl-x f1/f12
(defvar zz/ctrl-x-fn-map (make-sparse-keymap) "f1-f7 => ctrl-x f1/f12")
;;f1-f8 => ctrl-c f1/f12
(defvar zz/ctrl-c-fn-map (make-sparse-keymap) "f1-f8 => ctrl-c f1/f12")
;;f1-f9 => f1/f12
(defvar zz/fn-map (make-sparse-keymap) "f1-f9 => f1/f12")
;;f1-f10 => S-f1/f12
(defvar zz/shift-fn-map (make-sparse-keymap) "f1-f10 => S-f1/f12")
;;f1-f11 => C-f1/f12
(defvar zz/ctrl-fn-map (make-sparse-keymap) "f1-f11 => C-f1/f12")
;;f1-f12 => M-f1/f12
(defvar zz/meta-fn-map (make-sparse-keymap) "f1-f12 => M-f1/f12")

;;fn-key entry access
(apply-keys-to-map
 help-map
 (list
  [f7]  zz/ctrl-x-fn-map
  [f8]  zz/ctrl-c-fn-map
  
  [f9]  zz/fn-map
  [f10] zz/shift-fn-map
  [f11] zz/ctrl-fn-map
  [f12] zz/meta-fn-map 
  ))

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

;;group define fn key
(defun define-fn-key
    (fn-name fn-sym fn s-fn-sym s-fn c-fn-sym c-fn m-fn-sym m-fn
     ctrl-x-fn-sym ctrl-x-fn ctrl-c-fn-sym ctrl-c-fn &optional doc)
  (when (and fn-sym fn) 
    (define-key global-map       fn-sym         fn)
    (define-key zz/fn-map        fn-name        fn))
  (when (and s-fn-sym s-fn)
    (define-key global-map       s-fn-sym       s-fn)
    (define-key zz/shift-fn-map  fn-name        s-fn))
  (when (and c-fn-sym c-fn)
    (define-key global-map       c-fn-sym       c-fn)
    (define-key zz/ctrl-fn-map   fn-name        c-fn))
  (when (and m-fn-sym m-fn)
    (define-key global-map       m-fn-sym       m-fn)
    (define-key zz/meta-fn-map   fn-name        m-fn))
  (when (and ctrl-x-fn-sym ctrl-x-fn)
    (define-key global-map       ctrl-x-fn-sym  ctrl-x-fn)
    (define-key zz/ctrl-x-fn-map fn-name        ctrl-x-fn))
  (when (and ctrl-c-fn-sym ctrl-c-fn)
    (define-key global-map       ctrl-c-fn-sym  ctrl-c-fn)
    (define-key zz/ctrl-c-fn-map fn-name        ctrl-c-fn)))

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
    (kbd "C-x <f3>")  'my-occur
    (kbd "C-c <f3>")  'my-woman-at-point
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
    [f6]              (if-ms-windows
                       (lambda () (interactive) (with-chinese-env (multi-shell-new)))
                       'get-term)
    [S-f6]            (if-ms-windows
                       (lambda () (interactive) (with-chinese-env (multi-shell-current-directory)))
                       'multi-term-dedicated-toggle)
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
    (kbd "C-x <f11>") nil
    (kbd "C-c <f11>") nil
    )

(define-fn-key (gethash "f12" fn-key-table)
    [f12]             'find-grep
    [S-f12]           'rgrep
    [C-f12]           'find-name-dired
    [M-f12]           'my-c-rgrep
    (kbd "C-x <f12>") 'my-unicad-switch
    (kbd "C-c <f12>") 'my-os-file-switch
    )

(provide 'fn-setting)

;;; fn-setting.el ends here
