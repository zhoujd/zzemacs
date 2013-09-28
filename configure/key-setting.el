;;;; key-setting.el --- key config file
;;;

;;;function key map setting
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

;;ctrl/alt key proxy setting
(defvar zz/ctrl-proxy     esc-map "zz/ctrl-proxy")
(defvar zz/alt-proxy      esc-map "zz/alt-proxy")
;;fn-ctrl/alt key proxy setting
(defvar zz/ctrl-fn-proxy  help-map "zz/ctrl-fn-proxy")
(defvar zz/alt-fn-proxy   help-map "zz/alt-fn-proxy")
(defvar zz/shift-fn-proxy help-map "zz/shift-fn-proxy")
(defvar zz/fn-proxy       help-map "zz/fn-proxy")

;;C-1/+
(defvar zz/ctrl-map
  (let ((map zz/ctrl-proxy))
    (define-key map (kbd "C-`") 'imenu)
    (define-key map (kbd "C-1") 'delete-window)
    (define-key map (kbd "C-2") 'delete-frame)
    (define-key map (kbd "C-3") 'tabbar-backward-group)
    (define-key map (kbd "C-4") 'delete-frame)
    (define-key map (kbd "C-5") 'gud-until)
    (define-key map (kbd "C-6") 'gud-remove)
    (define-key map (kbd "C-7") 'gud-finish)
    (define-key map (kbd "C-8") 'gud-jump)
    (define-key map (kbd "C-9") 'gud-pstar)
    (define-key map (kbd "C-0") 'other-frame)
    (define-key map (kbd "C--") 'undo)
    (define-key map (kbd "C-=") 'redo)
    (define-key map (kbd "C-,") 'winner-undo)
    (define-key map (kbd "C-.") 'winner-redo)
    
    (define-key map [(control) (tab)] "\C-q\t")
    map)
  "control")
 
;;M-1/+
(defvar zz/alt-map
  (let ((map zz/alt-proxy))
    (define-key  map (kbd "M-1")  'delete-other-windows)
    (define-key  map (kbd "M-2")  'delete-other-frames)
    (define-key  map (kbd "M-3")  'tabbar-forward-group)
    (define-key  map (kbd "M-4")  'kill-this-buffer)
    (if-ms-windows
     (define-key map (kbd "M-5")  'gud-cont)
     (define-key map (kbd "M-5")  'gud-go))
    
    (define-key  map (kbd "M-6")  'gud-break)
    (define-key  map (kbd "M-7")  'gud-next)
    (define-key  map (kbd "M-8")  'gud-step)
    (define-key  map (kbd "M-9")  'gud-print)
    (define-key  map (kbd "M-0")  'other-window)
    (define-key  map (kbd "M--")  nil)
    (define-key  map (kbd "M-=")  nil)

    (define-key  map (kbd "M-]")  'tabbar-forward-tab)
    (define-key  map (kbd "M-[")  'tabbar-backward-tab)
            
    map)
  "alt")

;;f1/f12
(defvar zz/fn-map
  (let ((map zz/fn-proxy))
    (define-key map (kbd "1") [f1])
    (define-key map (kbd "2") [f2])
    (define-key map (kbd "3") [f3])
    (define-key map (kbd "4") (lookup-key zz/alt-map "4"))
    (define-key map (kbd "5") [f5])
    (define-key map (kbd "6") [f6])
    (define-key map (kbd "7") [f7])
    (define-key map (kbd "8") [f8])
    (define-key map (kbd "9") [f9])
    (define-key map (kbd "0") [f10])
    (define-key map (kbd "-") [f11])
    (define-key map (kbd "=") [f12])
    map)
  "f1/f12")

;;S-f1/f12
(defvar zz/shift-fn-map
  (let ((map zz/shift-fn-proxy))
    (define-key map (kbd "!") [S-f1])   ;;shift + 1
    (define-key map (kbd "@") [S-f2])   ;;shift + 2
    (define-key map (kbd "#") [S-f3])   ;;shift + 3
    (define-key map (kbd "$") [S-f4])   ;;shift + 4
    (define-key map (kbd "%") [S-f5])   ;;shift + 5
    (define-key map (kbd "^") [S-f6])   ;;shift + 6
    (define-key map (kbd "&") [S-f7])   ;;shift + 7
    (define-key map (kbd "*") [S-f8])   ;;shift + 8
    (define-key map (kbd "(") [S-f9])   ;;shift + 9
    (define-key map (kbd ")") [S-f10])  ;;shift + 0
    (define-key map (kbd "_") [S-f11])  ;;shift + -
    (define-key map (kbd "+") [S-f12])  ;;shift + =
    map)
  "S-f1/f12")

;;C-f1/f12
(defvar zz/ctrl-fn-map
  (let ((map zz/ctrl-fn-proxy))
    (define-key map (kbd "C-1") [C-f1])
    (define-key map (kbd "C-2") [C-f2])
    (define-key map (kbd "C-3") [C-f3])
    (define-key map (kbd "C-4") [C-f4])
    (define-key map (kbd "C-5") [C-f5])
    (define-key map (kbd "C-6") [C-f6])
    (define-key map (kbd "C-7") [C-f7])
    (define-key map (kbd "C-8") [C-f8])
    (define-key map (kbd "C-9") [C-f9])
    (define-key map (kbd "C-0") [C-f10])
    (define-key map (kbd "C--") [C-f11])
    (define-key map (kbd "C-=") [C-f12])
    map)
  "C-f1/f12")

;;esc-f12 => M-f1/f12
(defvar zz/alt-fn-map
  (let ((map zz/alt-fn-proxy))
    (define-key map (kbd "M-1") [M-f1])
    (define-key map (kbd "M-2") [M-f2])
    (define-key map (kbd "M-3") [M-f3])
    (define-key map (kbd "M-4") [M-f4])
    (define-key map (kbd "M-5") [M-f5])
    (define-key map (kbd "M-6") [M-f6])
    (define-key map (kbd "M-7") [M-f7])
    (define-key map (kbd "M-8") [M-f8])
    (define-key map (kbd "M-9") [M-f9])
    (define-key map (kbd "M-0") [M-f10])
    (define-key map (kbd "M--") [M-f11])
    (define-key map (kbd "M-=") [M-f12])
    map)
  "M-f1/f12")

;;shift -> up/down/left/right
(define-key help-map [left]  [S-left])
(define-key help-map [down]  [S-down])
(define-key help-map [up]    [S-up])
(define-key help-map [right] [S-right])
;;alt -> up/down/left/right
(define-key esc-map [left]   'windmove-left)
(define-key esc-map [down]   'windmove-down)
(define-key esc-map [up]     'windmove-up)
(define-key esc-map [right]  'windmove-right)

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
(global-set-key (kbd "M-1") (lookup-key zz/alt-map (kbd "M-1")))
(global-set-key (kbd "M-2") (lookup-key zz/alt-map (kbd "M-2")))
(global-set-key (kbd "M-3") (lookup-key zz/alt-map (kbd "M-3")))
(global-set-key (kbd "M-4") (lookup-key zz/alt-map (kbd "M-4")))
;;gud control setting
(global-set-key (kbd "M-5") (lookup-key zz/alt-map (kbd "M-5")))
(global-set-key (kbd "M-6") (lookup-key zz/alt-map (kbd "M-6")))
(global-set-key (kbd "M-7") (lookup-key zz/alt-map (kbd "M-7")))
(global-set-key (kbd "M-8") (lookup-key zz/alt-map (kbd "M-8")))
(global-set-key (kbd "M-9") (lookup-key zz/alt-map (kbd "M-9")))
(global-set-key (kbd "M-0") (lookup-key zz/alt-map (kbd "M-0")))

(global-set-key (kbd "C-`") (lookup-key zz/ctrl-map (kbd "C-`")))
(global-set-key (kbd "C-1") (lookup-key zz/ctrl-map (kbd "C-1")))
(global-set-key (kbd "C-2") (lookup-key zz/ctrl-map (kbd "C-2")))
(global-set-key (kbd "C-3") (lookup-key zz/ctrl-map (kbd "C-3")))
(global-set-key (kbd "C-4") (lookup-key zz/ctrl-map (kbd "C-4")))
;;gud control setting
(global-set-key (kbd "C-5") (lookup-key zz/ctrl-map (kbd "C-5")))
(global-set-key (kbd "C-6") (lookup-key zz/ctrl-map (kbd "C-6")))
(global-set-key (kbd "C-7") (lookup-key zz/ctrl-map (kbd "C-7")))
(global-set-key (kbd "C-8") (lookup-key zz/ctrl-map (kbd "C-8")))
(global-set-key (kbd "C-9") (lookup-key zz/ctrl-map (kbd "C-9")))
(global-set-key (kbd "C-0") (lookup-key zz/ctrl-map (kbd "C-0")))

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
(global-set-key [(control tab)]  (lookup-key zz/ctrl-map [(control) (tab)]))

;;tabbar switch group
(global-set-key (kbd "M-]")  (lookup-key zz/alt-map (kbd "M-]")))
(global-set-key (kbd "M-[")  (lookup-key zz/alt-map (kbd "M-[")))

(global-set-key (kbd "C-c h") 'helm-mini)

;;undo/redo
(define-key global-map (kbd "C--")  (lookup-key zz/ctrl-map (kbd "C--")))
(define-key global-map (kbd "C-=")  (lookup-key zz/ctrl-map (kbd "C-=")))

;;winner restore
(define-key global-map (kbd "C-,")  (lookup-key zz/ctrl-map (kbd "C-,")))
(define-key global-map (kbd "C-.")  (lookup-key zz/ctrl-map (kbd "C-.")))

;;quick move other windows
(define-key global-map [M-up]    (lookup-key zz/alt-map [up]))
(define-key global-map [M-down]  (lookup-key zz/alt-map [down]))
(define-key global-map [M-right] (lookup-key zz/alt-map [right]))
(define-key global-map [M-left]  (lookup-key zz/alt-map [left]))

;;window size change
(define-key global-map [S-up]    'enlarge-window)
(define-key global-map [S-down]  'shrink-window)
(define-key global-map [S-right] 'enlarge-window-horizontally)
(define-key global-map [S-left]  'shrink-window-horizontally)

;;set apps do M+x
(define-key global-map [apps]    'execute-extended-command)


(provide 'key-setting)

;;; key-setting.el ends here
