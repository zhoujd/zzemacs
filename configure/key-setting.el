;;;; key-setting.el --- key config file
;;;

;;  x11        console
;;---------------------
;;f1-f12   -- esc-f1/f12
;;S-f1/f12 -- f1 -f1/f12
;;C-f1/f12 -- f2 -f1/f12
;;M-f1/f12 -- f3 -f1/f12

;;use keymaps control flag
(defvar use-graph-keymap-p window-system "use console keymap setting")

(defun show-keymap-select ()
  (if use-graph-keymap-p
      (message "switch to graph keymap")
      (message "switch to console keymap")))

(defun switch-to-keymap ()
  (interactive)
  (setq use-graph-keymap-p (not use-graph-keymap-p))
  (zz-load-configure "key-setting.el")
  (show-keymap-select))

;;define new keymap for terminal
(unless use-graph-keymap-p
  ;;f2 key map
  (defvar f2-map (make-sparse-keymap) "f2 <=> control.")
  (define-key global-map [f2] f2-map)  
  ;;f3 key map
  (defvar f3-map (make-sparse-keymap) "f3 <=> alt.")
  (define-key global-map [f3] f3-map)
  )

(when-ms-windows
 (setq w32-pass-rwindow-to-system nil)
 (setq w32-pass-lwindow-to-system nil)
 (setq w32-rwindow-modifier 'hyper)
 (setq w32-lwindow-modifier 'super))

;;f4/esc-f4 key map 
(defvar f4-map (make-sparse-keymap) "f4 map for self functions.")
(define-key global-map [f4] f4-map)
(define-key esc-map    [f4] f4-map)

;;f4-esc key map 
(defvar f4-esc-map (make-sparse-keymap) "f4-escape for extend functions.")
(define-key f4-map [escape] f4-esc-map)

;;f4-e key map 
(defvar f4-e-map (make-sparse-keymap) "f4-e for execute functions.")
(define-key f4-map "e" f4-e-map)

;;f4-p key map 
(defvar f4-p-map (make-sparse-keymap) "f4-p for execute functions, can define in temp-setting.el.")
(define-key f4-map "p" f4-p-map)

;;f1-esc key map for help using
(defvar f1-esc-map (make-sparse-keymap) "f1-escape for self help functions.")
(define-key help-map [escape] f1-esc-map)

;;f1-esc key map for help using
(defvar f1-backquote-map (make-sparse-keymap) "f1-backquote for self help function.")
(define-key help-map (kbd "`") f1-backquote-map)

;;marcro for start-process
(defmacro execute-set-key (key-map key name args)
  `(define-key ,key-map (kbd ,key)
     (lambda ()
       (interactive)
       (apply 'start-process ,name nil ,args))))

(define-key f1-backquote-map (kbd "h") 'common-lisp-hyperspec)
(define-key f1-backquote-map (kbd "i") 'zz-info-open-file)

;;execute start-process key
(when-ms-windows
 (execute-set-key f4-e-map "1" "vs-x86-prompt" '("cmd" "/c" "start" "vcvarsall" "x86"))
 (execute-set-key f4-e-map "2" "vs-x64-prompt" '("cmd" "/c" "start" "vcvarsall" "x64")))

(execute-set-key f4-e-map "f" "firefox"       '("firefox" "http://www.baidu.com"))
(execute-set-key f4-e-map "b" "bcompare"      '("bcompare"))

;;for keymap switch
(when window-system
  (global-set-key (kbd "<f4> `") 'switch-to-keymap))

;;switch to shells
(global-set-key (kbd "<f4> <f9>")    
                (lambda () (interactive) (start-shell "*shell-f9*")))
(global-set-key (kbd "<f4> <f10>")    
                (lambda () (interactive) (start-shell "*shell-f10*")))
(global-set-key (kbd "<f4> <f11>")  
                (lambda () (interactive) (start-shell "*shell-f11*")))
(global-set-key (kbd "<f4> <f12>")  
                (lambda () (interactive) (start-shell "*shell-f12*")))

(global-set-key (kbd "<f4> <tab>")   'company-complete-common)

;;for info
(if use-graph-keymap-p
    (progn
      (global-set-key [C-f1]    'session-save)
      (global-set-key [M-f1]    'session-restore)
      )
    (progn
      (global-set-key (kbd "<f2> <f1>")   'session-save)
      (global-set-key (kbd "<f3> <f1>")   'session-restore)
      ))

(if use-graph-keymap-p
    (progn
      (global-set-key   [f2]    'bc-next)
      (global-set-key [S-f2]    'bc-previous)
      (global-set-key [C-f2]    'bc-set)
      (global-set-key [M-f2]    'bc-list))
    (progn
      (define-key esc-map   [f2]          'bc-next)
      (global-set-key (kbd "<f1> <f2>")   'bc-previous)
      (global-set-key (kbd "<f2> <f2>")   'bc-set)
      (global-set-key (kbd "<f3> <f2>")   'bc-list)))

(if use-graph-keymap-p
    (progn
      (global-set-key   [f3]    'my-last-buffer-go)
      (global-set-key [S-f3]    'list-bookmarks)
      (global-set-key [C-f3]    'bc-local-next)
      (global-set-key [M-f3]    'bc-local-previous)
      )
    (progn
      (define-key esc-map   [f3]          'my-last-buffer-go)
      (global-set-key (kbd "<f1> <f3>")   'list-bookmarks)
      (global-set-key (kbd "<f2> <f3>")   'bc-local-next)
      (global-set-key (kbd "<f3> <f3>")   'bc-local-previous)
      ))

(global-set-key (kbd "C-x <f3>") 'my-occur)
(global-set-key (kbd "C-c <f3>") 'my-woman-at-point)

(if use-graph-keymap-p
    (progn
      (global-set-key [S-f4]    'undo-kill-buffer)
      (global-set-key [C-f4]    'highlight-symbol-next)
      (global-set-key [M-f4]    'highlight-symbol-prev)
      )
    (progn
      (global-set-key (kbd "<f1> <f4>")   'undo-kill-buffer)
      (global-set-key (kbd "<f2> <f4>")   'highlight-symbol-next)
      (global-set-key (kbd "<f3> <f4>")   'highlight-symbol-prev)
      ))

(global-set-key (kbd "<f4> <f4>")  'kill-this-buffer)
(global-set-key (kbd "C-x  <f4>")  'recentf-open-files)
(global-set-key (kbd "C-c  <f4>")  'recentf-open-files-compl)

(if use-graph-keymap-p
    (progn
      (global-set-key   [f5]    'speedbar-get-focus)
      (global-set-key [S-f5]    'sr-speedbar-toggle)
      (global-set-key [C-f5]    'line-to-top-of-window)
      (global-set-key [M-f5]    'etags-stack-show))
    (progn
      (define-key esc-map   [f5]          'speedbar-get-focus)
      (global-set-key (kbd "<f1> <f5>")   'sr-speedbar-toggle)
      (global-set-key (kbd "<f2> <f5>")   'line-to-top-of-window)
      (global-set-key (kbd "<f3> <f5>")   'etags-stack-show)))

(if use-graph-keymap-p
    (progn
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
         )))
    (progn   
      (define-key esc-map   [f6]         'get-term)
      (global-set-key (kbd "<f1> <f6>")  'multi-term-dedicated-toggle)
      (global-set-key (kbd "<f2> <f6>")  'multi-term-next)
      (global-set-key (kbd "<f3> <f6>")  'multi-term-prev)
      (global-set-key (kbd "C-x  <f6>")  'switch-to-term)
      (global-set-key (kbd "C-c  <f6>")  'switch-term-and-text)
      ))

(if use-graph-keymap-p
    (progn
      (global-set-key   [f7]    'compile)
      (global-set-key [S-f7]    'switch-to-compilation)
      (global-set-key [C-f7]    'next-error)
      (global-set-key [M-f7]    'previous-error))
    (progn   
      (define-key esc-map   [f7]          'compile)
      (global-set-key (kbd "<f1> <f7>")   'switch-to-compilation)
      (global-set-key (kbd "<f2> <f7>")   'next-error)
      (global-set-key (kbd "<f3> <f7>")   'previous-error)))

(if use-graph-keymap-p
    (progn
      (global-set-key   [f8]    'gdb)
      (global-set-key [S-f8]    'gud-kill)
      (global-set-key [C-f8]    'gdb-restore-windows)
      (global-set-key [M-f8]    'gdb-many-windows))
    (progn    
      (define-key esc-map   [f8]          'gdb)
      (global-set-key (kbd "<f1> <f8>")   'gud-kill)
      (global-set-key (kbd "<f2> <f8>")   'gdb-restore-windows)
      (global-set-key (kbd "<f3> <f8>")   'gdb-many-windows)))

(global-set-key (kbd "C-x <f8>")  'gdb-use-separate-io)
(global-set-key (kbd "C-c <f8>")  'gud-tooltip-mode)

(if use-graph-keymap-p
    (progn
      (global-set-key   [f9]    (lambda () (interactive) (start-shell "*shell*")))
      (global-set-key [S-f9]    'multi-shell-new)
      (global-set-key [C-f9]    'switch-to-scratch)
      (global-set-key [M-f9]    'popup-term)
      )
    (progn   
      (define-key esc-map   [f9]          (lambda () (interactive) (start-shell "*shell*")))
      (global-set-key (kbd "<f1> <f9>")   'multi-shell-new)
      (global-set-key (kbd "<f2> <f9>")   'switch-to-scratch)
      (global-set-key (kbd "<f3> <f9>")   'popup-term)
      ))

(global-set-key (kbd "C-x <f9>")  'switch-to-shell)
(global-set-key (kbd "C-c <f9>")  'eshell)

(if use-graph-keymap-p
    (progn
      (global-set-key [S-f10]   'tool-bar-mode)
      (global-set-key [C-f10]   'my-toggle-maxframe)
      (global-set-key [M-f10]   'my-toggle-fullscreen))
    (progn    
      (global-set-key (kbd "<f1> <f10>")  'tool-bar-mode)
      (global-set-key (kbd "<f2> <f10>")  'my-toggle-maxframe)
      (global-set-key (kbd "<f3> <f10>")  'my-toggle-fullscreen)))

(global-set-key (kbd "C-x <f10>")  'scroll-bar-mode)
(global-set-key (kbd "C-c <f10>")  'tabbar-mode)

(if use-graph-keymap-p
    (progn
      (global-set-key   [f11]   'linum-mode)
      (global-set-key [S-f11]   'fci-mode)
      (global-set-key [C-f11]   'hl-line-mode)
      (global-set-key [M-f11]   'blank-mode))
    (progn  
      (define-key esc-map   [f11]          'linum-mode)
      (global-set-key (kbd "<f1> <f11>")   'fci-mode)
      (global-set-key (kbd "<f2> <f11>")   'hl-line-mode)
      (global-set-key (kbd "<f3> <f11>")   'blank-mode)))

(if use-graph-keymap-p
    (progn
      (global-set-key [f12]     'find-grep)
      (global-set-key [S-f12]   'rgrep)
      (global-set-key [C-f12]   'find-name-dired)
      (global-set-key [M-f12]   'my-c-rgrep))
    (progn
      (define-key esc-map   [f12]        'find-grep)
      (global-set-key (kbd "<f1> <f12>") 'rgrep)
      (global-set-key (kbd "<f2> <f12>") 'find-name-dired)
      (global-set-key (kbd "<f3> <f12>") 'my-c-rgrep)
      ))


(global-set-key (kbd "C-x <f12>") 'my-unicad-switch)
(global-set-key (kbd "C-c <f12>") 'my-os-file-switch)

;;number 0-1/-/=
(global-set-key (kbd "M-1")    'delete-other-windows)
(global-set-key (kbd "M-2")    'delete-other-frames)
(global-set-key (kbd "M-3")    'tabbar-forward-group)
(global-set-key (kbd "M-4")    'kill-this-buffer)
;;gud control setting
(if-ms-windows
 (global-set-key (kbd "M-5")   'gud-cont)
 (global-set-key (kbd "M-5")   'gud-go))

(global-set-key (kbd "M-6")    'gud-break-remove)
(global-set-key (kbd "M-7")    'gud-next)
(global-set-key (kbd "M-8")    'gud-step)
(global-set-key (kbd "M-9")    'gud-print)
(global-set-key (kbd "M-0")    'other-window)

(if use-graph-keymap-p
    (progn
      (global-set-key (kbd "C-`") 'imenu)
      (global-set-key (kbd "C-1") 'delete-window)
      (global-set-key (kbd "C-2") 'delete-frame)
      (global-set-key (kbd "C-3") 'tabbar-backward-group)
      (global-set-key (kbd "C-4") 'delete-frame)
      ;;gud control setting
      (global-set-key (kbd "C-5") 'gud-until)
      (global-set-key (kbd "C-6") 'gud-watch)
      (global-set-key (kbd "C-7") 'gud-finish)
      (global-set-key (kbd "C-8") 'gud-jump)
      (global-set-key (kbd "C-9") 'gud-pstar)
      (global-set-key (kbd "C-0") 'other-frame))
    (progn
      (global-set-key (kbd "<f2> `") 'imenu)
      (global-set-key (kbd "<f2> 1") 'delete-window)
      (global-set-key (kbd "<f2> 2") 'delete-frame)
      (global-set-key (kbd "<f2> 3") 'tabbar-backward-group)
      (global-set-key (kbd "<f2> 4") 'delete-frame)
      ;;gud control setting
      (global-set-key (kbd "<f2> 5") 'gud-until)
      (global-set-key (kbd "<f2> 6") 'gud-watch)
      (global-set-key (kbd "<f2> 7") 'gud-finish)
      (global-set-key (kbd "<f2> 8") 'gud-jump)
      (global-set-key (kbd "<f2> 9") 'gud-pstar)
      (global-set-key (kbd "<f2> 0") 'other-frame))
    )

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

(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-c w")   'compare-windows)
(global-set-key (kbd "M-#")     'query-replace-regexp)

(global-set-key (kbd "C-x M-s") 'sudo-unset-ro-or-save)
(global-set-key (kbd "C-x M-f") 'sudo-find-file)

(global-set-key (kbd "%") 'match-paren)

(global-unset-key [backspace])
(global-set-key   [backspace] 'delete-backward-char)
(global-unset-key [delete])
(global-set-key   [delete]    'delete-char)

(global-set-key (kbd "M-]") 'tabbar-forward)
(global-set-key (kbd "M-[") 'tabbar-backward)

(if-ms-windows    
 (progn ;; For Windows
   (global-set-key [C-wheel-up]   'text-scale-increase)
   (global-set-key [C-wheel-down] 'text-scale-decrease))
 (progn ;; For Linux
   (global-set-key [C-mouse-4] 'text-scale-increase)
   (global-set-key [C-mouse-5] 'text-scale-decrease)))

;;Control tab quotes a tab.
(global-set-key [C-tab] "\C-q\t")
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
(if use-graph-keymap-p
    (progn
      (define-key global-map (kbd "C--") 'undo)
      (define-key global-map (kbd "C-=") 'redo))
    (progn
      (define-key f2-map (kbd "-") 'undo)
      (define-key f2-map (kbd "=") 'redo))
    )

;;winner restore
(if use-graph-keymap-p
    (progn
      (define-key global-map (kbd "C-,") 'winner-undo)
      (define-key global-map (kbd "C-.") 'winner-redo))
    (progn
      (define-key f3-map (kbd ",") 'winner-undo)
      (define-key f3-map (kbd ".") 'winner-redo))
    )

;;quick move other windows
(if use-graph-keymap-p
    (progn
      (define-key global-map [M-up]    'windmove-up)
      (define-key global-map [M-down]  'windmove-down)
      (define-key global-map [M-right] 'windmove-right)
      (define-key global-map [M-left]  'windmove-left))
    (progn
      (define-key f3-map [up]    'windmove-up)
      (define-key f3-map [down]  'windmove-down)
      (define-key f3-map [right] 'windmove-right)
      (define-key f3-map [left]  'windmove-left))
    )

;;for mark
(unless use-graph-keymap-p
  (define-key f2-map (kbd "<SPC>") 'set-mark-command)
  )

;;;f3-map setting
;;window size change
(if use-graph-keymap-p
    (progn
      (define-key global-map [S-up]    'enlarge-window)
      (define-key global-map [S-down]  'shrink-window)
      (define-key global-map [S-right] 'enlarge-window-horizontally)
      (define-key global-map [S-left]  'shrink-window-horizontally))
    (progn
      (define-key f2-map [up]    'enlarge-window)
      (define-key f2-map [down]  'shrink-window)
      (define-key f2-map [right] 'enlarge-window-horizontally)
      (define-key f2-map [left]  'shrink-window-horizontally))
    )

;;set apps do M+x
(define-key global-map  [apps]  'execute-extended-command)

(provide 'key-setting)

;;; key-setting.el ends here
