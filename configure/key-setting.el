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
(define-key global-map [f4] f4-map)
(define-key esc-map    [f4] f4-map)

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

;;f1-esc key map for help using
(defvar f1-esc-map (make-sparse-keymap) "f1-escape for self help functions.")
(define-key help-map [escape] f1-esc-map)

;;f1-quote key map for help using
(defvar f1-backquote-map (make-sparse-keymap) "f1-backquote for self help function.")
(define-key help-map (kbd "`") f1-backquote-map)


;;f1-f7 => C-1/+
(defvar zz/ctrl-map
  (let ((map (make-sparse-keymap)))
    (define-key map "`" 'imenu)
    (define-key map "1" 'delete-window)
    (define-key map "2" 'delete-frame)
    (define-key map "3" 'tabbar-backward-group)
    (define-key map "4" 'delete-frame)
    (define-key map "5" 'gud-until)
    (define-key map "6" 'gud-remove)
    (define-key map "7" 'gud-finish)
    (define-key map "8" 'gud-jump)
    (define-key map "9" 'gud-pstar)
    (define-key map "0" 'other-frame)
    (define-key map "-" 'undo)
    (define-key map "=" 'redo)
    (define-key map "," 'winner-undo)
    (define-key map "." 'winner-redo)
    
    (define-key map [(tab)] "\C-q\t")
    map)
  "f7 <=> control")
 
;;f1-f8 => M-1/+
(defvar zz/alt-map
  (let ((map (make-sparse-keymap)))
    (define-key  map "1"  'delete-other-windows)
    (define-key  map "2"  'delete-other-frames)
    (define-key  map "3"  'tabbar-forward-group)
    (define-key  map "4"  'kill-this-buffer)
    (if-ms-windows
     (define-key map "5"  'gud-cont)
     (define-key map "5"  'gud-go))
    
    (define-key  map "6"  'gud-break)
    (define-key  map "7"  'gud-next)
    (define-key  map "8"  'gud-step)
    (define-key  map "9"  'gud-print)
    (define-key  map "0"  'other-window)
    (define-key  map "-"  nil)
    (define-key  map "="  nil)

    (define-key  map "]"  'tabbar-forward-tab)
    (define-key  map "["  'tabbar-backward-tab)
    
    (define-key  map "h"  'windmove-left)
    (define-key  map "j"  'windmove-down)
    (define-key  map "k"  'windmove-up)
    (define-key  map "l"  'windmove-right)
        
    map)
  "f8 <=> alt")

;;f1-f9 => f1/f12
(defvar zz/fn-map
  (let ((map (make-sparse-keymap)))
    (define-key map "1" [f1])
    (define-key map "2" [f2])
    (define-key map "3" [f3])
    (define-key map "4" (lookup-key zz/alt-map "4"))
    (define-key map "5" [f5])
    (define-key map "6" [f6])
    (define-key map "7" [f7])
    (define-key map "8" [f8])
    (define-key map "9" [f9])
    (define-key map "0" [f10])
    (define-key map "-" [f11])
    (define-key map "=" [f12])
    map)
  "f9 <=> f1/f12")

;;f1-f10 => S-f1/f12
(defvar zz/shift-fn-map
  (let ((map (make-sparse-keymap)))
    (define-key map "1" [S-f1])
    (define-key map "2" [S-f2])
    (define-key map "3" [S-f3])
    (define-key map "4" [S-f4])
    (define-key map "5" [S-f5])
    (define-key map "6" [S-f6])
    (define-key map "7" [S-f7])
    (define-key map "8" [S-f8])
    (define-key map "9" [S-f9])
    (define-key map "0" [S-f10])
    (define-key map "-" [S-f11])
    (define-key map "=" [S-f12])

    (define-key map "h" [S-left])
    (define-key map "j" [S-down])
    (define-key map "k" [S-up])
    (define-key map "l" [S-right])
    
    map)
  "f10 <=> S-f1/f12")

;;f1-f11 => C-f1/f12
(defvar zz/ctrl-fn-map
  (let ((map (make-sparse-keymap)))
    (define-key map "1" [C-f1])
    (define-key map "2" [C-f2])
    (define-key map "3" [C-f3])
    (define-key map "4" [C-f4])
    (define-key map "5" [C-f5])
    (define-key map "6" [C-f6])
    (define-key map "7" [C-f7])
    (define-key map "8" [C-f8])
    (define-key map "9" [C-f9])
    (define-key map "0" [C-f10])
    (define-key map "-" [C-f11])
    (define-key map "=" [C-f12])
    map)
  "f11 <=> C-f1/f12")

;;esc-f12 => M-f1/f12
(defvar zz/alt-fn-map
  (let ((map (make-sparse-keymap)))
    (define-key map "1" [M-f1])
    (define-key map "2" [M-f2])
    (define-key map "3" [M-f3])
    (define-key map "4" [M-f4])
    (define-key map "5" [M-f5])
    (define-key map "6" [M-f6])
    (define-key map "7" [M-f7])
    (define-key map "8" [M-f8])
    (define-key map "9" [M-f9])
    (define-key map "0" [M-f10])
    (define-key map "-" [M-f11])
    (define-key map "=" [M-f12])
    map)
  "f12 <=> M-f1/f12")

(define-key global-map (kbd "<f1> <f7>") zz/ctrl-map)
(define-key global-map (kbd "<f1> <f8>") zz/alt-map)

(define-key global-map [(f1) (f9)]  zz/fn-map)
(define-key global-map [(f1) (f10)] zz/shift-fn-map)
(define-key global-map [(f1) (f11)] zz/ctrl-fn-map)
(define-key global-map [(f1) (f12)] zz/alt-fn-map)

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
(global-set-key (kbd "<f4> <f9>")    
                (lambda () (interactive) (start-shell "*shell-f9*")))
(global-set-key (kbd "<f4> <f10>")    
                (lambda () (interactive) (start-shell "*shell-f10*")))
(global-set-key (kbd "<f4> <f11>")  
                (lambda () (interactive) (start-shell "*shell-f11*")))
(global-set-key (kbd "<f4> <f12>")  
                (lambda () (interactive) (start-shell "*shell-f12*")))

(global-set-key (kbd "<f4> =") 'er/expand-region)
(when-emacs24-3
 (global-set-key (kbd "<f4> -") 'smartparens-mode))

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
(global-set-key (kbd "M-1") (lookup-key zz/alt-map "1"))
(global-set-key (kbd "M-2") (lookup-key zz/alt-map "2"))
(global-set-key (kbd "M-3") (lookup-key zz/alt-map "3"))
(global-set-key (kbd "M-4") (lookup-key zz/alt-map "4"))
;;gud control setting
(global-set-key (kbd "M-5") (lookup-key zz/alt-map "5"))
(global-set-key (kbd "M-6") (lookup-key zz/alt-map "6"))
(global-set-key (kbd "M-7") (lookup-key zz/alt-map "7"))
(global-set-key (kbd "M-8") (lookup-key zz/alt-map "8"))
(global-set-key (kbd "M-9") (lookup-key zz/alt-map "9"))
(global-set-key (kbd "M-0") (lookup-key zz/alt-map "0"))

(global-set-key (kbd "C-`") (lookup-key zz/ctrl-map "`"))
(global-set-key (kbd "C-1") (lookup-key zz/ctrl-map "1"))
(global-set-key (kbd "C-2") (lookup-key zz/ctrl-map "2"))
(global-set-key (kbd "C-3") (lookup-key zz/ctrl-map "3"))
(global-set-key (kbd "C-4") (lookup-key zz/ctrl-map "4"))
;;gud control setting
(global-set-key (kbd "C-5") (lookup-key zz/ctrl-map "5"))
(global-set-key (kbd "C-6") (lookup-key zz/ctrl-map "6"))
(global-set-key (kbd "C-7") (lookup-key zz/ctrl-map "7"))
(global-set-key (kbd "C-8") (lookup-key zz/ctrl-map "8"))
(global-set-key (kbd "C-9") (lookup-key zz/ctrl-map "9"))
(global-set-key (kbd "C-0") (lookup-key zz/ctrl-map "0"))

;;f1 1,2,3,4 for highlight-symbol
(global-set-key (kbd "<f1> 1") 'highlight-symbol-at-point)
(global-set-key (kbd "<f1> 2") 'highlight-symbol-remove-all)
(global-set-key (kbd "<f1> 3") 'highlight-symbol-query-replace)

;;gdb frame show setting
(global-set-key (kbd "<f1> 5") 'gdb-frame-stack-buffer)
(global-set-key (kbd "<f1> 6") 'gdb-frame-breakpoints-buffer)
(global-set-key (kbd "<f1> 7") 'gdb-frame-assembler-buffer)
(global-set-key (kbd "<f1> 8") 'gdb-frame-memory-buffer)
(global-set-key (kbd "<f1> 9") 'gdb-frame-locals-buffer)
(global-set-key (kbd "<f1> 0") 'gdb-frame-gdb-buffer)
(global-set-key (kbd "<f1> -") 'gud-up)
(global-set-key (kbd "<f1> =") 'gud-down)

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

;;f2-map setting
(define-key f4-map (kbd "C-1") 'my-utf-8)
(define-key f4-map (kbd "C-h") 'sourcepair-jump-to-headerfile)
(define-key f4-map (kbd "C-l") 'command-history)

(unless-ms-windows  
 (define-key f4-map (kbd "C-d") 'open-with-nautilus)
 (define-key f4-map (kbd "C-t") 'open-with-terminal))

(when-ms-windows
 (execute-set-key f4-map "C-d" "explorer" (list "explorer" (my-trans-path-sep default-directory "/" "\\"))))

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
