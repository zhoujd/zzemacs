;;;; key-setting.el --- key config file
;;;

;;;;function key setting on console
;;;keymap:zz/ctrl-map          f1-f7  + 1/= <===> control + 1/=
;;;keymap:zz/alt-map           f1-f8  + 1/= <===> alt     + 1/=

;;;;emacs default setting
;;C-x @ S        event-apply-shift-modifier
;;C-x @ a        event-apply-alt-modifier
;;C-x @ c        event-apply-control-modifier
;;C-x @ h        event-apply-hyper-modifier
;;C-x @ m        event-apply-meta-modifier
;;C-x @ s        event-apply-super-modifier
(apply-keys-to-map
 key-translation-map
 (list 
  (kbd "C-c -") 'event-apply-control-modifier
  (kbd "C-c =") 'event-apply-meta-modifier
  ))

;;f4/esc-f4 key map 
(defvar f4-map (make-sparse-keymap) "f4 map for self functions.")
(define-key global-map [f4] f4-map)
(define-key help-map   "4"  f4-map)

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
(defvar f4-p-map (make-sparse-keymap) "f4-p for execute functions, in temp-setting.el.")
(define-key f4-map "p" f4-p-map)

;;f1-esc key map for help using
(defvar f1-esc-map (make-sparse-keymap) "f1-escape for self help functions.")
(define-key help-map [escape] f1-esc-map)

;;f1-quote key map for help using
(defvar f1-backquote-map (make-sparse-keymap) "f1-backquote for self help function.")
(define-key help-map (kbd "`") f1-backquote-map)

;;f1-f7 => C-1/+
(defvar zz/ctrl-map  (make-sparse-keymap))
(define-key help-map (kbd "<f7>") zz/ctrl-map)

;;f1-f8 => M-1/+
(defvar zz/alt-map (make-sparse-keymap))
(define-key help-map (kbd "<f8>") zz/alt-map)

(apply-keys-to-map
 zz/ctrl-map
 (list 
  "`" 'imenu
  "1" 'delete-window
  "2" 'delete-frame
  "3" 'tabbar-backward-group
  "4" 'delete-frame
  "5" 'gud-until
  "6" 'gud-remove
  "7" 'gud-finish
  "8" 'gud-jump
  "9" 'gud-pstar
  "0" 'other-frame
  "-" 'undo
  "=" 'redo
  ;;undo/redo
  "," 'winner-undo
  "." 'winner-redo

  [(tab)] "\C-q\t"
  ))
 
(apply-keys-to-map
 zz/alt-map
 (list
  "1" 'delete-other-windows
  "2" 'delete-other-frames
  "3" 'tabbar-forward-group
  "4" 'kill-this-buffer
  "5" (if-ms-windows 'gud-cont 'gud-go)
  
  "6" 'gud-break
  "7" 'gud-next
  "8" 'gud-step
  "9" 'gud-print
  "0" 'other-window
  
  "#" 'query-replace-regexp
  "]" 'tabbar-forward-tab
  "[" 'tabbar-backward-tab
  ))

;;number 0-1/-/=
(apply-keys-to-map
 global-map
 (list
  (kbd "M-1")     (lookup-key zz/alt-map "1")
  (kbd "M-2")     (lookup-key zz/alt-map "2")
  (kbd "M-3")     (lookup-key zz/alt-map "3")
  (kbd "M-4")     (lookup-key zz/alt-map "4")
  ;;gud control setting
  (kbd "M-5")     (lookup-key zz/alt-map "5")
  (kbd "M-6")     (lookup-key zz/alt-map "6")
  (kbd "M-7")     (lookup-key zz/alt-map "7")
  (kbd "M-8")     (lookup-key zz/alt-map "8")
  (kbd "M-9")     (lookup-key zz/alt-map "9")
  (kbd "M-0")     (lookup-key zz/alt-map "0")

  (kbd "M-#")     (lookup-key zz/alt-map "#")
  ;;tabbar switch group
  (kbd "M-]")     (lookup-key zz/alt-map "]")
  (kbd "M-[")     (lookup-key zz/alt-map "[")

  ;;;ctrl number setting
  (kbd "C-`")     (lookup-key zz/ctrl-map "`")
  (kbd "C-1")     (lookup-key zz/ctrl-map "1")
  (kbd "C-2")     (lookup-key zz/ctrl-map "2")
  (kbd "C-3")     (lookup-key zz/ctrl-map "3")
  (kbd "C-4")     (lookup-key zz/ctrl-map "4")
  ;;gud control setting
  (kbd "C-5")     (lookup-key zz/ctrl-map "5")
  (kbd "C-6")     (lookup-key zz/ctrl-map "6")
  (kbd "C-7")     (lookup-key zz/ctrl-map "7")
  (kbd "C-8")     (lookup-key zz/ctrl-map "8")
  (kbd "C-9")     (lookup-key zz/ctrl-map "9")
  (kbd "C-0")     (lookup-key zz/ctrl-map "0")
  
  ;;undo/redo
  (kbd "C--")     (lookup-key zz/ctrl-map "-")
  (kbd "C-=")     (lookup-key zz/ctrl-map "=")

  ;;winner restore
  (kbd "C-,")     (lookup-key zz/ctrl-map ",")
  (kbd "C-.")     (lookup-key zz/ctrl-map ".")

  ;;Control tab quotes a tab => "\C-q\t"
  [(control tab)] (lookup-key zz/ctrl-map [(tab)])
  ))

;;key for f1-backquote-map
(apply-keys-to-map
 f1-backquote-map
 (list
  (kbd "h") 'common-lisp-hyperspec
  (kbd "i") 'zz-info-open-file
  ))

;;execute start-process key
(apply-keys-to-map
 f4-e-map
 (list
  "1" (when-ms-windows (execute-set-key
                        "vs-x86-prompt"
                        '("cmd" "/c" "start" "vcvarsall" "x86")))
  "2" (when-ms-windows (execute-set-key
                        "vs-x64-prompt"
                        '("cmd" "/c" "start" "vcvarsall" "x64")))
  "f" (execute-set-key "firefox"  '("firefox" "http://www.baidu.com"))
  "b" (execute-set-key "bcompare" '("bcompare"))
  ))

;;switch to shells
(apply-keys-to-map
 f4-map
 (list  
  (kbd "<f4>")  'kill-this-buffer
  
  (kbd "<f9>")  (lambda () (interactive) (start-shell "*shell-f9*"))
  (kbd "<f10>") (lambda () (interactive) (start-shell "*shell-f10*"))
  (kbd "<f11>") (lambda () (interactive) (start-shell "*shell-f11*"))
  (kbd "<f12>") (lambda () (interactive) (start-shell "*shell-f12*"))
  
  (kbd "=")     'er/expand-region
  (kbd "-")     (if-emacs24-3 'smartparens-mode)
  
  (kbd "C-1")   'my-utf-8
  (kbd "C-h")   'sourcepair-jump-to-headerfile
  (kbd "C-l")   'command-history

  (kbd "C-d")   (if-ms-windows
                 (execute-set-key
                  "explorer"
                  (list "explorer"
                        (my-trans-path-sep
                         default-directory "/" "\\")))
                 'open-with-nautilus)  
  (kbd "C-t")   (unless-ms-windows 'open-with-terminal)
  ))

;;f1 1,2,3,4 for highlight-symbol
(apply-keys-to-map
 help-map
 (list
  (kbd "1") 'highlight-symbol-at-point
  (kbd "2") 'highlight-symbol-remove-all
  (kbd "3") 'highlight-symbol-query-replace
  ;;"4" -> f4-map
  
  ;;gdb frame show setting
  (kbd "5") 'gdb-frame-stack-buffer
  (kbd "6") 'gdb-frame-breakpoints-buffer
  (kbd "7") 'gdb-frame-assembler-buffer
  (kbd "8") 'gdb-frame-memory-buffer
  (kbd "9") 'gdb-frame-locals-buffer
  (kbd "0") 'gdb-frame-gdb-buffer
  (kbd "-") 'gud-up
  (kbd "=") 'gud-down
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-x C-b") 'ibuffer
  (kbd "C-x C-j") 'dired-jump
  (kbd "C-c w")   'compare-windows

  (kbd "%")       'match-paren

  [backspace]     'delete-backward-char
  [delete]        'delete-char

  [C-wheel-up]    (when-ms-windows 'text-scale-increase)
  [C-wheel-down]  (when-ms-windows 'text-scale-decrease)
  [C-mouse-4]     (unless-ms-windows 'text-scale-increase)
  [C-mouse-5]     (unless-ms-windows 'text-scale-decrease)

  (kbd "C-c h")   'helm-mini

  ;;quick move other windows
  [M-up]          'windmove-up
  [M-down]        'windmove-down
  [M-right]       'windmove-right
  [M-left]        'windmove-left
  
  ;;window size change
  [S-up]          'enlarge-window
  [S-down]        'shrink-window
  [S-right]       'enlarge-window-horizontally
  [S-left]        'shrink-window-horizontally

  ;;set apps do M+x
  [apps]          'execute-extended-command
  ))

(provide 'key-setting)

;;; key-setting.el ends here
