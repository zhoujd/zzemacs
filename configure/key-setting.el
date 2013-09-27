;;;; key-setting.el --- key config file
;;;

;;;;function key setting on console
;;;keymap:zz/esc-fn-map        f1-f2  + 1/= <===> esc + f1/f12
;;;keymap:zz/f1-fn-map         f1-f3  + 1/= <===> f1  + f1/f12
;;;keymap:zz/f4-fn-map         f1-f4  + 1/= <===> f4  + f1/f12

;;;keymap:zz/ctr-x-fn-map      f1-f5  + 1/= <===> ctrl-x + f1/f12
;;;keymap:zz/ctr-c-fn-map      f1-f6  + 1/= <===> ctrl-x + f1/f12

;;;keymap:zz/ctrl-map          f1-f7  + 1/= <===> control + 1/=
;;;keymap:zz/alt-map           f1-f8  + 1/= <===> alt     + 1/=

;;;keymap:zz/fn-map            f1-f9  + 1/= <===> f1/f12
;;;keymap:zz/shift-fn-map      f1-f10 + 1/= <===> shift   + f1/f12
;;;keymap:zz/ctrl-fn-map       f1-f11 + 1/= <===> control + f1/f12
;;;keymap:zz/alt-fn-map        f1-f12 + 1/= <===> alt     + f1/f12

;;f4/esc-f4 key map 
(defvar f4-map (make-sparse-keymap) "f4 map for self functions.")
(define-key global-map [f4]      f4-map)
(define-key help-map   [escape]  f4-map)

;;f4-esc key map 
(defvar f4-esc-map (make-sparse-keymap) "f4-escape for extend functions.")
(define-key f4-map [escape] f4-esc-map)

;;f4-quote key map for help using
(defvar f4-backquote-map (make-sparse-keymap) "f4-backquote for self help function.")
(define-key f4-map (kbd "`") f4-backquote-map)

;;f4-e key map 
(defvar f4-e-map (make-sparse-keymap) "f4-e for execute functions.")
(define-key f4-map "e" f4-e-map)

;;f4-p key map 
(defvar f4-p-map (make-sparse-keymap) "f4-p for execute functions, can define in temp-setting.el.")
(define-key f4-map "p" f4-p-map)

;;f1-quote key map for help using
(defvar f1-backquote-map (make-sparse-keymap) "f1-backquote for self help function.")
(define-key help-map (kbd "`") f1-backquote-map)

;;f1-f11 => C-1
(define-key esc-map [C-1] [C-f1])
(define-key esc-map [C-2] [C-f2])
(define-key esc-map [C-3] [C-f3])
(define-key esc-map [C-4] [C-f4])
(define-key esc-map [C-5] [C-f5])
(define-key esc-map [C-6] [C-f6])
(define-key esc-map [C-7] [C-f7])
(define-key esc-map [C-8] [C-f8])
(define-key esc-map [C-9] [C-f9])
(define-key esc-map [C-0] [C-f10])
(define-key esc-map [C--] [C-f11])
(define-key esc-map [C-=] [C-f12])

;;f1-f9 => f1/f12
(define-key help-map "1" [f1])
(define-key help-map "2" [f2])
(define-key help-map "3" [f3])
(define-key help-map "4" [f4])
(define-key help-map "5" [f5])
(define-key help-map "6" [f6])
(define-key help-map "7" [f7])
(define-key help-map "8" [f8])
(define-key help-map "9" [f9])
(define-key help-map "0" [f10])
(define-key help-map "-" [f11])
(define-key help-map "=" [f12])

;;f1-f10 => S-f1/f12
(define-key help-map "!" [S-f1])
(define-key help-map "@" [S-f2])
(define-key help-map "#" [S-f3])
(define-key help-map "$" [S-f4])
(define-key help-map "%" [S-f5])
(define-key help-map "^" [S-f6])
(define-key help-map "&" [S-f7])
(define-key help-map "*" [S-f8])
(define-key help-map "(" [S-f9])
(define-key help-map ")" [S-f10])
(define-key help-map "_" [S-f11])
(define-key help-map "+" [S-f12])

;;f1-f11 => C-f1/f12
(define-key help-map [C-1] [C-f1])
(define-key help-map [C-2] [C-f2])
(define-key help-map [C-3] [C-f3])
(define-key help-map [C-4] [C-f4])
(define-key help-map [C-5] [C-f5])
(define-key help-map [C-6] [C-f6])
(define-key help-map [C-7] [C-f7])
(define-key help-map [C-8] [C-f8])
(define-key help-map [C-9] [C-f9])
(define-key help-map [C-0] [C-f10])
(define-key help-map [C--] [C-f11])
(define-key help-map [C-=] [C-f12])

;;f1-f11 => M-f1/f12
(define-key help-map [M-1] [M-f1])
(define-key help-map [M-2] [M-f2])
(define-key help-map [M-3] [M-f3])
(define-key help-map [M-4] [M-f4])
(define-key help-map [M-5] [M-f5])
(define-key help-map [M-6] [M-f6])
(define-key help-map [M-7] [M-f7])
(define-key help-map [M-8] [M-f8])
(define-key help-map [M-9] [M-f9])
(define-key help-map [M-0] [M-f10])
(define-key help-map [M--] [M-f11])
(define-key help-map [M-=] [M-f12])

(when-ms-windows
 (setq w32-pass-rwindow-to-system nil)
 (setq w32-pass-lwindow-to-system nil)
 (setq w32-rwindow-modifier 'hyper)
 (setq w32-lwindow-modifier 'super))

(define-key f1-backquote-map (kbd "h") 'common-lisp-hyperspec)
(define-key f1-backquote-map (kbd "i") 'zz-info-open-file)

;;execute start-process key
(when-ms-windows
 (execute-set-key f4-e-map "1" "vs-x86-prompt" '("cmd" "/c" "start" "vcvarsall" "x86"))
 (execute-set-key f4-e-map "2" "vs-x64-prompt" '("cmd" "/c" "start" "vcvarsall" "x64")))

(execute-set-key f4-e-map "f" "firefox"       '("firefox" "http://www.baidu.com"))
(execute-set-key f4-e-map "b" "bcompare"      '("bcompare"))

;;switch to shells
(define-key f4-map (kbd "<f9>")  (lambda () (interactive) (start-shell "*shell-f9*")))
(define-key f4-map (kbd "<f10>") (lambda () (interactive) (start-shell "*shell-f10*")))
(define-key f4-map (kbd "<f11>") (lambda () (interactive) (start-shell "*shell-f11*")))
(define-key f4-map (kbd "<f12>") (lambda () (interactive) (start-shell "*shell-f12*")))

(define-key f4-map (kbd "=") 'er/expand-region)
(define-key f4-map (kbd "-") 'smartparens-mode)

;;f4-map setting
(define-key f4-map (kbd "C-1") 'my-utf-8)
(define-key f4-map (kbd "C-h") 'sourcepair-jump-to-headerfile)
(define-key f4-map (kbd "C-l") 'command-history)

(if-ms-windows
 (progn
   (execute-set-key f4-map "C-d" "explorer" (list "explorer" (my-trans-path-sep default-directory "/" "\\"))))
 (progn
   (define-key f4-map (kbd "C-d") 'open-with-nautilus)
   (define-key f4-map (kbd "C-t") 'open-with-terminal)))
 
;;for info
(global-set-key [C-f1]    'session-save)
(global-set-key [M-f1]    'session-restore)

(global-set-key   [f2]    'bc-next)
(global-set-key [S-f2]    'bc-previous)
(global-set-key [C-f2]    'bc-set)
(global-set-key [M-f2]    'bc-list)

(global-set-key   [f3]    'my-last-buffer-go)
(global-set-key [S-f3]    'list-bookmarks)
(global-set-key [C-f3]    'bc-local-next)
(global-set-key [M-f3]    'bc-local-previous)
    
(global-set-key (kbd "C-x <f3>") 'my-occur)
(global-set-key (kbd "C-c <f3>") 'my-woman-at-point)


(global-set-key [S-f4]    'undo-kill-buffer)
(global-set-key [C-f4]    'highlight-symbol-next)
(global-set-key [M-f4]    'highlight-symbol-prev)
     
(global-set-key (kbd "<f4> <f4>")  'kill-this-buffer)
(global-set-key (kbd "C-x  <f4>")  'recentf-open-files)
(global-set-key (kbd "C-c  <f4>")  'recentf-open-files-compl)


(global-set-key   [f5]    'speedbar-get-focus)
(global-set-key [S-f5]    'sr-speedbar-toggle)
(global-set-key [C-f5]    'line-to-top-of-window)
(global-set-key [M-f5]    'etags-stack-show)


(if-ms-windows          
 (progn ;; For Windows
   (global-set-key   [f6]   'multi-shell-new)
   (global-set-key [S-f6]   'multi-shell-current-directory)
   (global-set-key [C-f6]   'multi-shell-next)
   (global-set-key [M-f6]   'multi-shell-prev)
   (global-set-key (kbd "C-x <f6>") 'switch-to-shell)
   )   
 (progn ;; For Linux
   (global-set-key   [f6]   'get-term)
   (global-set-key [S-f6]   'multi-term-dedicated-toggle)
   (global-set-key [C-f6]   'multi-term-next)
   (global-set-key [M-f6]   'multi-term-prev)
   (global-set-key (kbd "C-x  <f6>")  'switch-to-term)
   (global-set-key (kbd "C-c  <f6>")  'switch-term-and-text)
   ))


(global-set-key   [f7]    'compile)
(global-set-key [S-f7]    'switch-to-compilation)
(global-set-key [C-f7]    'next-error)
(global-set-key [M-f7]    'previous-error)

(global-set-key   [f8]    'gdb)
(global-set-key [S-f8]    'gud-kill)
(global-set-key [C-f8]    'gdb-restore-windows)
(global-set-key [M-f8]    'gdb-many-windows)

(global-set-key (kbd "C-x <f8>")  'gdb-use-separate-io)
(global-set-key (kbd "C-c <f8>")  'gud-tooltip-mode)


(global-set-key   [f9]    (lambda () (interactive) (start-shell "*shell*")))
(global-set-key [S-f9]    'multi-shell-new)
(global-set-key [C-f9]    'switch-to-scratch)
(global-set-key [M-f9]    'popup-term)
 
(global-set-key (kbd "C-x <f9>")  'switch-to-shell)
(global-set-key (kbd "C-c <f9>")  'eshell)

(global-set-key [S-f10]   'tool-bar-mode)
(global-set-key [C-f10]   'my-toggle-maxframe)
(global-set-key [M-f10]   'my-toggle-fullscreen)

(global-set-key (kbd "C-x <f10>")  'scroll-bar-mode)
(global-set-key (kbd "C-c <f10>")  'tabbar-mode)


(global-set-key   [f11]   'linum-mode)
(global-set-key [S-f11]   'fci-mode)
(global-set-key [C-f11]   'hl-line-mode)
(global-set-key [M-f11]   'blank-mode)

(global-set-key [f12]     'find-grep)
(global-set-key [S-f12]   'rgrep)
(global-set-key [C-f12]   'find-name-dired)
(global-set-key [M-f12]   'my-c-rgrep)

(global-set-key (kbd "C-x <f12>") 'my-unicad-switch)
(global-set-key (kbd "C-c <f12>") 'my-os-file-switch)

;;number 0-1/-/=
(global-set-key (kbd "M-1") (lookup-key help-map [M-1]))
(global-set-key (kbd "M-2") (lookup-key help-map [M-2]))
(global-set-key (kbd "M-3") (lookup-key help-map [M-3]))
(global-set-key (kbd "M-4") (lookup-key help-map [M-4]))
;;gud control setting                            
(global-set-key (kbd "M-5") (lookup-key help-map [M-5]))
(global-set-key (kbd "M-6") (lookup-key help-map [M-6]))
(global-set-key (kbd "M-7") (lookup-key help-map [M-7]))
(global-set-key (kbd "M-8") (lookup-key help-map [M-8]))
(global-set-key (kbd "M-9") (lookup-key help-map [M-9]))
(global-set-key (kbd "M-0") (lookup-key help-map [M-0]))

(global-set-key (kbd "C-`") (lookup-key esc-map (kbd "C-`")))
(global-set-key (kbd "C-1") (lookup-key esc-map [C-1]))
(global-set-key (kbd "C-2") (lookup-key esc-map [C-2]))
(global-set-key (kbd "C-3") (lookup-key esc-map [C-3]))
(global-set-key (kbd "C-4") (lookup-key esc-map [C-4]))
;;gud control setting                           
(global-set-key (kbd "C-5") (lookup-key esc-map [C-5]))
(global-set-key (kbd "C-6") (lookup-key esc-map [C-6]))
(global-set-key (kbd "C-7") (lookup-key esc-map [C-7]))
(global-set-key (kbd "C-8") (lookup-key esc-map [C-8]))
(global-set-key (kbd "C-9") (lookup-key esc-map [C-9]))
(global-set-key (kbd "C-0") (lookup-key esc-map [C-0]))

;;f1 1,2,3,4 for highlight-symbol
(global-set-key (kbd "<f4> 1") 'highlight-symbol-at-point)
(global-set-key (kbd "<f4> 2") 'highlight-symbol-remove-all)
(global-set-key (kbd "<f4> 3") 'highlight-symbol-query-replace)

;;gdb frame show setting
(global-set-key (kbd "<f4> 5") 'gdb-frame-stack-buffer)
(global-set-key (kbd "<f4> 6") 'gdb-frame-breakpoints-buffer)
(global-set-key (kbd "<f4> 7") 'gdb-frame-assembler-buffer)
(global-set-key (kbd "<f4> 8") 'gdb-frame-memory-buffer)
(global-set-key (kbd "<f4> 9") 'gdb-frame-locals-buffer)
(global-set-key (kbd "<f4> 0") 'gdb-frame-gdb-buffer)
(global-set-key (kbd "<f4> -") 'gud-up)
(global-set-key (kbd "<f4> =") 'gud-down)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-c w")   'compare-windows)
(global-set-key (kbd "M-#")     'query-replace-regexp)

(global-set-key (kbd "%")       'match-paren)

(global-unset-key [backspace])
(global-set-key   [backspace] 'delete-backward-char)
(global-unset-key [delete])
(global-set-key   [delete]    'delete-char)

(if-ms-windows    
 (progn ;; For Windows
   (global-set-key [C-wheel-up]   'text-scale-increase)
   (global-set-key [C-wheel-down] 'text-scale-decrease))
 (progn ;; For Linux
   (global-set-key [C-mouse-4] 'text-scale-increase)
   (global-set-key [C-mouse-5] 'text-scale-decrease)))

;;Control tab quotes a tab => "\C-q\t"
(global-set-key [(control tab)]  (lookup-key zz/ctrl-map [(tab)]))

;;tabbar switch group
(global-set-key (kbd "M-]")  (lookup-key zz/alt-map "]"))
(global-set-key (kbd "M-[")  (lookup-key zz/alt-map "["))

(global-set-key (kbd "C-c h") 'helm-mini)

;;undo/redo
(define-key global-map (kbd "C--")  (lookup-key zz/ctrl-map "-"))
(define-key global-map (kbd "C-=")  (lookup-key zz/ctrl-map "="))

;;winner restore
(define-key global-map (kbd "C-,")  (lookup-key zz/ctrl-map ","))
(define-key global-map (kbd "C-.")  (lookup-key zz/ctrl-map "."))

;;quick move other windows
(define-key global-map [M-up]    (lookup-key zz/alt-map "k"))
(define-key global-map [M-down]  (lookup-key zz/alt-map "j"))
(define-key global-map [M-right] (lookup-key zz/alt-map "l"))
(define-key global-map [M-left]  (lookup-key zz/alt-map "h"))

;;window size change
(define-key global-map [S-up]    'enlarge-window)
(define-key global-map [S-down]  'shrink-window)
(define-key global-map [S-right] 'enlarge-window-horizontally)
(define-key global-map [S-left]  'shrink-window-horizontally)

;;set apps do M+x
(define-key global-map [apps]    'execute-extended-command)


(provide 'key-setting)

;;; key-setting.el ends here
