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
(defvar zz/ctrl-map     esc-map "zz/ctrl-map")
(defvar zz/alt-map      esc-map "zz/alt-map")
;;fn-ctrl/alt key proxy setting
(defvar zz/fn-map       esc-map "zz/fn-map")

;;multi key setting
(defun apply-keys-to-map (map key-pairs)
  "apply multi key defines"
  (let ((i 0))
    (while (< i (length key-pairs))
      (let ((key (nth i key-pairs))
            (fn (nth (1+ i) key-pairs)))
        (when fn
          (define-key map key fn)))
      (setq i (+ i 2)))))

;;;esc-map C-1/=
(apply-keys-to-map
 zz/ctrl-map
 (list
  (kbd "C-`") 'imenu
  (kbd "C-1") 'delete-window
  (kbd "C-2") 'delete-frame
  (kbd "C-3") 'tabbar-backward-group
  (kbd "C-4") 'delete-frame
  (kbd "C-5") 'gud-until
  (kbd "C-6") 'gud-remove
  (kbd "C-7") 'gud-finish
  (kbd "C-8") 'gud-jump
  (kbd "C-9") 'gud-pstar
  (kbd "C-0") 'other-frame
  (kbd "C--") 'undo
  (kbd "C-=") 'redo
  (kbd "C-,") 'winner-undo
  (kbd "C-.") 'winner-redo
  
  [(control) (tab)] "\C-q\t"
  ))

;;;esc-map M-1/=
(apply-keys-to-map
 zz/alt-map
 (list
  (kbd "M-1")  'delete-other-windows
  (kbd "M-2")  'delete-other-frames
  (kbd "M-3")  'tabbar-forward-group
  (kbd "M-4")  'kill-this-buffer
  (kbd "M-5")  (if-ms-windows 'gud-cont 'gud-go)
  (kbd "M-6")  'gud-break
  (kbd "M-7")  'gud-next
  (kbd "M-8")  'gud-step
  (kbd "M-9")  'gud-print
  (kbd "M-0")  'other-window
  (kbd "M--")  nil
  (kbd "M-=")  nil
  
  (kbd "M-]")  'tabbar-forward-tab
  (kbd "M-[")  'tabbar-backward-tab
  
  ;;alt -> up/down/left/right
  [left]       'windmove-left
  [down]       'windmove-down
  [up]         'windmove-up
  [right]      'windmove-right
  ))

;;shift -> up/down/left/right
(define-key help-map [left]  [S-left])
(define-key help-map [down]  [S-down])
(define-key help-map [up]    [S-up])
(define-key help-map [right] [S-right])

(when-ms-windows
 (setq w32-pass-rwindow-to-system nil)
 (setq w32-pass-lwindow-to-system nil)
 (setq w32-rwindow-modifier 'hyper)
 (setq w32-lwindow-modifier 'super))

(define-key f1-backquote-map (kbd "h") 'common-lisp-hyperspec)
(define-key f1-backquote-map (kbd "i") 'zz-info-open-file)

;;execute start-process key
(apply-keys-to-map
 f4-e-map
 (list
  "1" (when-ms-windows (execute-set-key "vs-x86-prompt" '("cmd" "/c" "start" "vcvarsall" "x86")))
  "2" (when-ms-windows (execute-set-key "vs-x64-prompt" '("cmd" "/c" "start" "vcvarsall" "x64")))
  "f" (execute-set-key "firefox"  '("firefox" "http://www.baidu.com"))
  "b" (execute-set-key "bcompare" '("bcompare"))
  ))

;;switch to shells
(apply-keys-to-map
 f4-map
 (list
  (kbd "<f9>")  (lambda () (interactive) (start-shell "*shell-f9*"))
  (kbd "<f10>") (lambda () (interactive) (start-shell "*shell-f10*"))
  (kbd "<f11>") (lambda () (interactive) (start-shell "*shell-f11*"))
  (kbd "<f12>") (lambda () (interactive) (start-shell "*shell-f12*"))
  
  (kbd "=")     'er/expand-region
  (kbd "-")     'smartparens-mode
  
  (kbd "C-1")   'my-utf-8
  (kbd "C-h")   'sourcepair-jump-to-headerfile
  (kbd "C-l")   'command-history

  (kbd "C-d")   (if-ms-windows (execute-set-key "explorer" (list "explorer" (my-trans-path-sep default-directory "/" "\\")))
                               'open-with-nautilus)  
  (kbd "C-t")   (unless-ms-windows 'open-with-terminal)
  ))

;;apply fn-key setting
(apply-keys-to-map
 zz/fn-map
 (list
  ;;for info
  [S-f1]    'planner-create-task-from-buffer
  [C-f1]    'session-save
  [M-f1]    'session-restore
  
  [f2]      'bc-next
  [S-f2]    'bc-previous
  [C-f2]    'bc-set
  [M-f2]    'bc-list
  
  [f3]      'my-last-buffer-go
  [S-f3]    'list-bookmarks
  [C-f3]    'bc-local-next
  [M-f3]    'bc-local-previous

  [S-f4]    'undo-kill-buffer
  [C-f4]    'highlight-symbol-next
  [M-f4]    'highlight-symbol-prev

  [f5]      'speedbar-get-focus
  [S-f5]    'sr-speedbar-toggle
  [C-f5]    'line-to-top-of-window
  [M-f5]    'etags-stack-show

  [f6]      (if-ms-windows 'multi-shell-new  'get-term)
  [S-f6]    (if-ms-windows 'multi-shell-current-directory 'multi-term-dedicated-toggle)
  [C-f6]    (if-ms-windows 'multi-shell-next 'multi-term-next)
  [M-f6]    (if-ms-windows 'multi-shell-prev 'multi-term-prev)
    
  [f7]      'compile
  [S-f7]    'switch-to-compilation
  [C-f7]    'next-error
  [M-f7]    'previous-error
  
  [f8]      'gdb
  [S-f8]    'gud-kill
  [C-f8]    'gdb-restore-windows
  [M-f8]    'gdb-many-windows

  [f9]      (lambda () (interactive) (start-shell "*shell*"))
  [S-f9]    'multi-shell-new
  [C-f9]    'switch-to-scratch
  [M-f9]    'popup-term

  [S-f10]   'tool-bar-mode
  [C-f10]   'my-toggle-maxframe
  [M-f10]   'my-toggle-fullscreen

  [f11]     'linum-mode
  [S-f11]   'fci-mode
  [C-f11]   'hl-line-mode
  [M-f11]   'blank-mode
  
  [f12]     'find-grep
  [S-f12]   'rgrep
  [C-f12]   'find-name-dired
  [M-f12]   'my-c-rgrep
  ))

;;global-map f1-f12
(apply-keys-to-map
 global-map
 (list
  ;;->f1 for help-map
  [f2]   (lookup-key zz/fn-map [f2])
  [f3]   (lookup-key zz/fn-map [f3])
  ;;->f4 for f4-map
  [f5]   (lookup-key zz/fn-map [f5])
  [f6]   (lookup-key zz/fn-map [f6])
  [f7]   (lookup-key zz/fn-map [f7])
  [f8]   (lookup-key zz/fn-map [f8])
  [f9]   (lookup-key zz/fn-map [f9])
  [f10]  (lookup-key zz/fn-map [f10])
  [f11]  (lookup-key zz/fn-map [f11])
  [f12]  (lookup-key zz/fn-map [f12])  
  ))

;;global-map shift + f1-f12
(apply-keys-to-map
 global-map
 (list
  [S-f1]   (lookup-key zz/fn-map [S-f1])
  [S-f2]   (lookup-key zz/fn-map [S-f2])
  [S-f3]   (lookup-key zz/fn-map [S-f3])
  [S-f4]   (lookup-key zz/fn-map [S-f4])
  [S-f5]   (lookup-key zz/fn-map [S-f5])
  [S-f6]   (lookup-key zz/fn-map [S-f6])
  [S-f7]   (lookup-key zz/fn-map [S-f7])
  [S-f8]   (lookup-key zz/fn-map [S-f8])
  [S-f9]   (lookup-key zz/fn-map [S-f9])
  [S-f10]  (lookup-key zz/fn-map [S-f10])
  [S-f11]  (lookup-key zz/fn-map [S-f11])
  [S-f12]  (lookup-key zz/fn-map [S-f12])  
  ))

;;global-map control + f1-f12
(apply-keys-to-map
 global-map
 (list
  [C-f1]   (lookup-key zz/fn-map [C-f1])
  [C-f2]   (lookup-key zz/fn-map [C-f2])
  [C-f3]   (lookup-key zz/fn-map [C-f3])
  [C-f4]   (lookup-key zz/fn-map [C-f4])
  [C-f5]   (lookup-key zz/fn-map [C-f5])
  [C-f6]   (lookup-key zz/fn-map [C-f6])
  [C-f7]   (lookup-key zz/fn-map [C-f7])
  [C-f8]   (lookup-key zz/fn-map [C-f8])
  [C-f9]   (lookup-key zz/fn-map [C-f9])
  [C-f10]  (lookup-key zz/fn-map [C-f10])
  [C-f11]  (lookup-key zz/fn-map [C-f11])
  [C-f12]  (lookup-key zz/fn-map [C-f12])  
  ))

;;global-map alt + f1-f12
(apply-keys-to-map
 global-map
 (list
  [M-f1]   (lookup-key zz/fn-map [M-f1])
  [M-f2]   (lookup-key zz/fn-map [M-f2])
  [M-f3]   (lookup-key zz/fn-map [M-f3])
  [M-f4]   (lookup-key zz/fn-map [M-f4])
  [M-f5]   (lookup-key zz/fn-map [M-f5])
  [M-f6]   (lookup-key zz/fn-map [M-f6])
  [M-f7]   (lookup-key zz/fn-map [M-f7])
  [M-f8]   (lookup-key zz/fn-map [M-f8])
  [M-f9]   (lookup-key zz/fn-map [M-f9])
  [M-f10]  (lookup-key zz/fn-map [M-f10])
  [M-f11]  (lookup-key zz/fn-map [M-f11])
  [M-f12]  (lookup-key zz/fn-map [M-f12])  
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-x <f3>") 'my-occur
  (kbd "C-c <f3>") 'my-woman-at-point

  (kbd "<f4> <f4>")  'kill-this-buffer
  (kbd "C-x  <f4>")  'recentf-open-files
  (kbd "C-c  <f4>")  'recentf-open-files-compl
  
  (kbd "C-x <f6>") (if-ms-windows 'switch-to-shell 'switch-to-term)
  (kbd "C-c  <f6>")  (unless-ms-windows 'switch-term-and-text)
  
  (kbd "C-x <f8>")  'gdb-use-separate-io
  (kbd "C-c <f8>")  'gud-tooltip-mode
  
  (kbd "C-x <f9>")  'switch-to-shell
  (kbd "C-c <f9>")  'eshell
  
  (kbd "C-x <f10>")  'scroll-bar-mode
  (kbd "C-c <f10>")  'tabbar-mode
  
  (kbd "C-x <f12>") 'my-unicad-switch
  (kbd "C-c <f12>") 'my-os-file-switch
  ))

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
(apply-keys-to-map
 help-map
 (list
  (kbd "1") 'highlight-symbol-at-point
  (kbd "2") 'highlight-symbol-remove-all
  (kbd "3") 'highlight-symbol-query-replace
  
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
