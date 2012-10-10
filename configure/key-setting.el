;;;; key-setting.el --- key config file
;;;

;;f2 key map 
(defvar f2-map (make-sparse-keymap) "Keymap for self related commands.")
(define-key global-map [f2] f2-map)

;;f3 key map 
(defvar f3-map (make-sparse-keymap) "Keymap for self related commands.")
(define-key global-map [f3] f3-map)

;;f4 key map 
(defvar f4-map (make-sparse-keymap) "Keymap for self related commands.")
(define-key global-map [f4] f4-map)

;;switch to shells
(define-key ctl-x-map [f9]
  (lambda () (interactive) (switch-to-shell "*shell-f9*")))
(define-key ctl-x-map [f10]
  (lambda () (interactive) (switch-to-shell "*shell-f10*")))
(define-key ctl-x-map [f11]
  (lambda () (interactive) (switch-to-shell "*shell-f11*")))

;;for info
(global-set-key (kbd "<f1> <f1>")   'session-save)
(global-set-key (kbd "<f2> <f1>")   'session-restore)
(global-set-key (kbd "<f3> <f1>")   'recentf-open-files)
(global-set-key (kbd "<f4> <f1>")   'recentf-open-files-compl)

(global-set-key (kbd "<f1> <f2>")   'bc-set)
(global-set-key (kbd "<f2> <f2>")   'bc-clear)
(global-set-key (kbd "<f3> <f2>")   'bc-next)
(global-set-key (kbd "<f4> <f2>")   'bc-previous)
(define-key esc-map [f2] 'bc-list)

(global-set-key (kbd "<f1> <f3>")   'my-last-buffer-go)
(global-set-key (kbd "<f2> <f3>")   'list-bookmarks)
(global-set-key (kbd "<f4> <f3>")   'my-occur)

(global-set-key (kbd "<f1> <f4>")   'bc-local-previous)
(global-set-key (kbd "<f2> <f4>")   'bc-local-next)
(global-set-key (kbd "<f3> <f4>")   'undo-kill-buffer)
(global-set-key (kbd "<f4> <f4>")   'kill-this-buffer)

(global-set-key (kbd "<f1> <f5>")   'speedbar-get-focus)
(global-set-key (kbd "<f2> <f5>")   'sr-speedbar-toggle)
(global-set-key (kbd "<f3> <f5>")   'line-to-top-of-window)
(global-set-key (kbd "<f4> <f5>")   'etags-stack-show)

(if (or (eq window-system 'w32)
        (eq window-system 'win32))
 (progn ;; For Windows
   (global-set-key (kbd "<f1> <f6>")  'multi-shell-new)
   (global-set-key (kbd "<f2> <f6>")  'multi-shell-current-directory)
   (global-set-key (kbd "<f3> <f6>")  'multi-shell-next)
   (global-set-key (kbd "<f4> <f6>")  'multi-shell-prev)
   (define-key esc-map [f6] 'switch-to-shell)
   )   
 (progn ;; For Linux
   (global-set-key (kbd "<f1> <f6>")  'get-term)
   (global-set-key (kbd "<f2> <f6>")  'multi-term-dedicated-toggle)
   (global-set-key (kbd "<f3> <f6>")  'multi-term-next)
   (global-set-key (kbd "<f4> <f6>")  'multi-term-prev)
   (define-key esc-map [f6] 'switch-to-term)
   ))

(global-set-key (kbd "<f1> <f7>")   'compile)
(global-set-key (kbd "<f2> <f7>")   'switch-to-compilation)
(global-set-key (kbd "<f3> <f7>")   'next-error)
(global-set-key (kbd "<f4> <f7>")   'previous-error)

(global-set-key (kbd "<f1> <f8>")   'gdb)
(global-set-key (kbd "<f2> <f8>")   'gud-tooltip-mode)
(global-set-key (kbd "<f3> <f8>")   'gdb-many-windows)
(global-set-key (kbd "<f4> <f8>")   'gdb-restore-windows)
(define-key esc-map [f8] 'gud-kill)

(global-set-key (kbd "<f1> <f9>")   'shell)
(global-set-key (kbd "<f2> <f9>")   'multi-shell-new)
(global-set-key (kbd "<f3> <f9>")   'switch-to-scratch)
(global-set-key (kbd "<f4> <f9>")   'popup-term)
(define-key esc-map [f9]  'switch-to-shell)

(global-set-key (kbd "<f1> <f10>")  'tool-bar-mode)
(global-set-key (kbd "<f2> <f10>")  'scroll-bar-mode)
(global-set-key (kbd "<f3> <f10>")  'my-toggle-maxframe)
(global-set-key (kbd "<f4> <f10>")  'my-toggle-fullscreen)

(global-set-key (kbd "<f1> <f11>")   'linum-mode)
(global-set-key (kbd "<f2> <f11>")   'hl-line-mode)
(global-set-key (kbd "<f3> <f11>")   'blank-mode)
(global-set-key (kbd "<f4> <f11>")   'fci-mode)

(global-set-key (kbd "<f1> <f12>") 'find-name-dired)
(global-set-key (kbd "<f2> <f12>") 'my-c-rgrep)
(global-set-key (kbd "<f3> <f12>") 'rgrep)
(global-set-key (kbd "<f4> <f12>") 'find-grep)

(global-set-key (kbd "C-x <f12>") 'my-unicad-switch)
(global-set-key (kbd "C-c <f12>") 'my-os-file-switch)

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "\e1") 'delete-window)

(global-set-key (kbd "M-2") 'delete-other-frames)
(global-set-key (kbd "\e2") 'delete-frames)

(global-set-key (kbd "M-3") 'tabbar-forward-group)
(global-set-key (kbd "\e3") 'tabbar-backward-group)

(global-set-key (kbd "M-4") 'kill-this-buffer)
(global-set-key (kbd "\e4") 'delete-frame)

;;f1 1,2,3,4 for highlight-symbol
(global-set-key (kbd "<f1> 1") 'highlight-symbol-at-point)
(global-set-key (kbd "<f1> 2") 'highlight-symbol-remove-all)
(global-set-key (kbd "<f1> 3") 'highlight-symbol-next)
(global-set-key (kbd "<f1> 4") 'highlight-symbol-prev)

;;gud control setting
(global-set-key (kbd "M-5") 'gud-go)
(global-set-key (kbd "\e5") 'gud-until)
(global-set-key (kbd "M-6") 'gud-break-remove)
(global-set-key (kbd "\e6") 'gud-watch)
(global-set-key (kbd "M-7") 'gud-next)
(global-set-key (kbd "\e7") 'gud-finish)
(global-set-key (kbd "M-8") 'gud-step)
(global-set-key (kbd "\e8") 'gud-jump)
(global-set-key (kbd "M-9") 'gud-print)
(global-set-key (kbd "\e9") 'gud-pstar)
;;gdb frame show setting
(global-set-key (kbd "<f1> 5") 'gdb-frame-stack-buffer)
(global-set-key (kbd "<f1> 6") 'gdb-frame-breakpoints-buffer)
(global-set-key (kbd "<f1> 7") 'gdb-frame-assembler-buffer)
(global-set-key (kbd "<f1> 8") 'gdb-frame-memory-buffer)
(global-set-key (kbd "<f1> 9") 'gdb-frame-locals-buffer)
(global-set-key (kbd "<f1> 0") 'gdb-use-separate-io-buffer)
(global-set-key (kbd "<f1> -") 'gud-up)
(global-set-key (kbd "<f1> =") 'gud-down)

(global-set-key (kbd "M-0") 'other-window)
(global-set-key (kbd "\e0") 'other-frame)

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
(global-set-key   [delete] 'delete-char)

(global-set-key (kbd "M-]") 'tabbar-forward)
(global-set-key (kbd "M-[") 'tabbar-backward)

(if (or (eq window-system 'w32)
        (eq window-system 'win32))
 (progn ;; For Windows
  (global-set-key [C-wheel-up]   'text-scale-increase)
  (global-set-key [C-wheel-down] 'text-scale-decrease))
 (progn ;; For Linux
  (global-set-key [C-mouse-4] 'text-scale-increase)
  (global-set-key [C-mouse-5] 'text-scale-decrease)))


;;Control tab quotes a tab.
(global-set-key [C-tab] "\C-q\t")

;;f2-map setting
(define-key f2-map (kbd "C-1") 'my-utf-8)
(define-key f2-map (kbd "C-h") 'sourcepair-jump-to-headerfile)
(unless (or (eq window-system 'w32)
            (eq window-system 'win32))
  (define-key f2-map (kbd "C-d") 'open-with-nautilus)
  (define-key f2-map (kbd "C-t") 'open-with-terminal))

;;undo/redo
(define-key f2-map (kbd "-") 'undo)
(define-key f2-map (kbd "=") 'redo)

;;winner restore
(define-key f2-map (kbd ",") 'winner-undo)
(define-key f2-map (kbd ".") 'winner-redo)

;;quick move other windows
(define-key f2-map [up]    'windmove-up)
(define-key f2-map [down]  'windmove-down)
(define-key f2-map [right] 'windmove-right)
(define-key f2-map [left]  'windmove-left)

;;for mark
(define-key f2-map (kbd "<SPC>") 'set-mark-command)

;;;f3-map setting
;;window size change
(define-key f3-map [up]    'enlarge-window)
(define-key f3-map [down]  'shrink-window)
(define-key f3-map [right] 'enlarge-window-horizontally)
(define-key f3-map [left]  'shrink-window-horizontally)


(provide 'key-setting)

;;; key-setting.el ends here
