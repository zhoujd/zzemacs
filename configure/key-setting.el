;;;; key-setting.el --- key config file
;;;

;;;;function key map setting
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
  (kbd "C-c `") 'event-apply-shift-modifier
  (kbd "C-c -") 'event-apply-control-modifier
  (kbd "C-c =") 'event-apply-meta-modifier
  ))

;;f4/esc-f4 key map 
(defvar f4-map (make-sparse-keymap)
  "f4 map for self functions.")
(define-key global-map [f4]      f4-map)
(define-key help-map   "4"       f4-map)

;;f4-esc key map 
(defvar f4-esc-map (make-sparse-keymap)
  "f4-escape for extend functions.")
(define-key f4-map [escape] f4-esc-map)

;;f4-quote key map for help using
(defvar f4-backquote-map (make-sparse-keymap)
  "f4-backquote for self help function.")
(define-key f4-map (kbd "`") f4-backquote-map)

;;f4-e key map 
(defvar f4-e-map (make-sparse-keymap)
  "f4-e for execute functions.")
(define-key f4-map "e" f4-e-map)

;;f4-p key map 
(defvar f4-p-map (make-sparse-keymap)
  "f4-p for execute functions, can define in temp-setting.el.")
(define-key f4-map "p" f4-p-map)

;;f1-quote key map for help using
(defvar f1-backquote-map (make-sparse-keymap)
  "f1-backquote for self help function.")
(define-key help-map (kbd "`") f1-backquote-map)

;;;global-map C-1/=
(apply-keys-to-map
 global-map
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
  
  [control tab]   "\C-q\t"
  [C-wheel-up]    (when-ms-windows   'text-scale-increas)
  [C-wheel-down]  (when-ms-windows   'text-scale-decrease)
  [C-mouse-4]     (unless-ms-windows 'text-scale-increase)
  [C-mouse-5]     (unless-ms-windows 'text-scale-decrease)
  ))

;;;global-map M-1/=
(apply-keys-to-map
 global-map
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
  
  (kbd "M-]")  'tabbar-forward-tab
  (kbd "M-[")  'tabbar-backward-tab

  (kbd "M-#")  'query-replace-regexp
  
  ;;alt -> up/down/left/right
  [M-left]     'windmove-left
  [M-down]     'windmove-down
  [M-up]       'windmove-up
  [M-right]    'windmove-right
  ))

;;f1-backquote-map
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

;;apply fn-key setting
(apply-keys-to-map
 global-map
 (list
  ;;for info
  [S-f1]            'planner-create-task-from-buffer
  [C-f1]            'session-save
  [M-f1]            'session-restore
  
  [f2]              'bc-next
  [S-f2]            'bc-previous
  [C-f2]            'bc-set
  [M-f2]            'bc-list
  
  [f3]              'my-last-buffer-go
  [S-f3]            'list-bookmarks
  [C-f3]            'bc-local-next
  [M-f3]            'bc-local-previous
  (kbd "C-x <f3>")  'my-occur
  (kbd "C-c <f3>")  'my-woman-at-point

  [S-f4]            'undo-kill-buffer
  [C-f4]            'highlight-symbol-next
  [M-f4]            'highlight-symbol-prev
  (kbd "C-x <f4>")  'recentf-open-files
  (kbd "C-c <f4>")  'recentf-open-files-compl
  
  [f5]              'speedbar-get-focus
  [S-f5]            'sr-speedbar-toggle
  [C-f5]            'line-to-top-of-window
  [M-f5]            'etags-stack-show

  [f6]              (if-ms-windows 'multi-shell-new  'get-term)
  [S-f6]            (if-ms-windows
                     'multi-shell-current-directory
                     'multi-term-dedicated-toggle)
  [C-f6]            (if-ms-windows 'multi-shell-next 'multi-term-next)
  [M-f6]            (if-ms-windows 'multi-shell-prev 'multi-term-prev)
  (kbd "C-x <f6>")  (if-ms-windows 'switch-to-shell 'switch-to-term)
  (kbd "C-c <f6>")  (unless-ms-windows 'switch-term-and-text)
  
  [f7]              'compile
  [S-f7]            'switch-to-compilation
  [C-f7]            'next-error
  [M-f7]            'previous-error
  
  [f8]              'gdb
  [S-f8]            'gud-kill
  [C-f8]            'gdb-restore-windows
  [M-f8]            'gdb-many-windows
  (kbd "C-x <f8>")  'gdb-use-separate-io
  (kbd "C-c <f8>")  'gud-tooltip-mode
  
  [f9]              (lambda () (interactive) (start-shell "*shell*"))
  [S-f9]            'multi-shell-new
  [C-f9]            'switch-to-scratch
  [M-f9]            'popup-term
  (kbd "C-x <f9>")  'switch-to-shell
  (kbd "C-c <f9>")  'eshell
  
  [S-f10]           'tool-bar-mode
  [C-f10]           'my-toggle-maxframe
  [M-f10]           'my-toggle-fullscreen
  (kbd "C-x <f10>") 'scroll-bar-mode
  (kbd "C-c <f10>") 'tabbar-mode
  
  [f11]             'linum-mode
  [S-f11]           'fci-mode
  [C-f11]           'hl-line-mode
  [M-f11]           'blank-mode
  
  [f12]             'find-grep
  [S-f12]           'rgrep
  [C-f12]           'find-name-dired
  [M-f12]           'my-c-rgrep
  (kbd "C-x <f12>") 'my-unicad-switch
  (kbd "C-c <f12>") 'my-os-file-switch
  ))

;;;for console f1/f12
(apply-keys-to-map
 help-map
 (list
  [f2]  (lookup-key global-map [f2])
  [f3]  (lookup-key global-map [f3])
  [f4]  (lookup-key global-map [f4])
  [f5]  (lookup-key global-map [f5])
  [f6]  (lookup-key global-map [f6])
  [f7]  (lookup-key global-map [f7])
  [f8]  (lookup-key global-map [f8])
  [f9]  (lookup-key global-map [f9])
  [f10] (lookup-key global-map [f10])
  [f11] (lookup-key global-map [f11])
  [f12] (lookup-key global-map [f12])
  ))

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

(apply-keys-to-map
 global-map
 (list
  (kbd "C-x C-b") 'ibuffer
  (kbd "C-x C-j") 'dired-jump
  (kbd "C-c w")   'compare-windows
  
  (kbd "%")       'match-paren  
  [backspace]     'delete-backward-char
  [delete]        'delete-char

  ;;Control tab quotes a tab => "\C-q\t"
  (kbd "C-c h")   'helm-mini
  
  ;;window size change
  [S-up]          'enlarge-wind
  [S-down]        'shrink-window
  [S-right]       'enlarge-window-horizontally
  [S-left]        'shrink-window-horizontally
  
  ;;set apps do M+x
  [apps]          (when-ms-windows 'execute-extended-command)
  ))

(provide 'key-setting)

;;; key-setting.el ends here
