;;;; key-setting.el --- key config file
;;;

;;myself key map
(defvar esc-f2-map (make-sparse-keymap) "Keymap for self related commands.")
(define-key esc-map [f2] esc-f2-map)

(define-key esc-f2-map "1"  'my-utf-8)

;;f12 key map 
(defvar f12-map (make-sparse-keymap) "Keymap for self related commands.")
(define-key global-map [f12] f12-map)

(define-key f12-map "1" 'my-utf-8)
(define-key f12-map "h" 'sourcepair-jump-to-headerfile)

;;switch to shells
(define-key f12-map [f9]
  (lambda () (interactive) (shell-create-by-name "*shell-f9*")))
(define-key f12-map [f10]
  (lambda () (interactive) (shell-create-by-name "*shell-f10*")))
(define-key f12-map [f11]
  (lambda () (interactive) (shell-create-by-name "*shell-f11*")))

;;evil mode
;(global-set-key (kbd "<f12> <f12>") 'evil-mode)

;;for info
(global-set-key [M-f1] 'info)

(global-set-key [(control f2)] 'bc-set)
(global-set-key [(f2)]         'bc-next)
(global-set-key [(shift f2)]   'bc-previous)
(global-set-key [C-S-f2]       'bc-list)
(global-set-key [(meta f2)]    'bc-list)

(global-set-key [f3]   'my-last-buffer-go)
(global-set-key [C-f3] 'list-bookmarks)

(global-set-key [f4]   'kill-this-buffer)
(global-set-key [C-f4] 'vm)

(global-set-key [f5]   'speedbar-get-focus)
(global-set-key [S-f5] 'sr-speedbar-toggle)
(global-set-key [C-f5] 'line-to-top-of-window)

(if (or (eq window-system 'w32)
        (eq window-system 'win32))
 (progn ;; For Windows
   (global-set-key [f6] 'edit-with-gvim)
   )
 (progn ;; For Linux
   (global-set-key [f6]   'get-term)
   (global-set-key [S-f6] 'multi-term-dedicated-toggle)
   (global-set-key [C-f6] 'multi-term-prev)
   (global-set-key [M-f6] 'multi-term-next))
 )

(global-set-key [f7]     'compile)
(global-set-key [S-f7]   'to-compilation)
(global-set-key [C-f7]   'next-error)
(global-set-key [M-f7]   'previous-error)
(global-set-key [C-S-f7] 'previous-error)

(global-set-key [f8]     'gdb)
(global-set-key [M-f8]   'gud-tooltip-mode)
(global-set-key [C-S-f8] 'gud-tooltip-mode)
(global-set-key [S-f8]   'gdb-many-windows)
(global-set-key [C-f8]   'gdb-restore-windows)

;;eshell,shell,terminal-emulator
(global-set-key [f9]   'shell)
(global-set-key [C-f9] 'to-scratch)
(global-set-key [M-f9] 'popup-term)
(global-set-key [S-f9] 'popup-term)

(global-set-key [C-f10] 'tool-bar-mode)
(global-set-key [S-f10] 'menu-bar-mode)
(global-set-key [M-f10] 'my-toggle-fullscreen)

(global-set-key [f11]   'linum-mode)
(global-set-key [C-f11] 'hl-line-mode)
(global-set-key [M-f11] 'blank-mode)
(global-set-key [S-f11] 'fci-mode)

(global-set-key [C-f12] 'find-name-dired)
(global-set-key [M-f12] 'c-rgrep)
(global-set-key [S-f12] 'find-dired)
(global-set-key (kbd "C-x <f12>") 'my-unicad-switch)
(global-set-key (kbd "C-c <f12>") 'my-os-file-switch)

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "C-1") 'delete-window)

(global-set-key (kbd "M-2") 'delete-other-frames)
(global-set-key (kbd "C-2") 'delete-frames)

(global-set-key (kbd "M-3") 'tabbar-forward-group)
(global-set-key (kbd "C-3") 'tabbar-backward-group)

(global-set-key (kbd "M-4") 'kill-this-buffer)
(global-set-key (kbd "C-4") 'delete-frame)

(global-set-key (kbd "M-5") 'gud-go)
(global-set-key (kbd "C-5") 'gud-until)

(global-set-key (kbd "M-6") 'gud-break-remove)
(global-set-key (kbd "C-6") 'gud-watch)

(global-set-key (kbd "M-7") 'gud-next)
(global-set-key (kbd "C-7") 'gud-finish)

(global-set-key (kbd "M-8") 'gud-step)
(global-set-key (kbd "C-8") 'gud-jump)

(global-set-key (kbd "M-9") 'gud-print)
(global-set-key (kbd "C-9") 'gud-pstar)

(global-set-key (kbd "M-0") 'other-window)
(global-set-key (kbd "C-0") 'other-frame)

(global-set-key (kbd "<f1> 5") 'gdb-frame-stack-buffer)
(global-set-key (kbd "<f1> 6") 'gdb-frame-breakpoints-buffer)
(global-set-key (kbd "<f1> 7") 'gdb-frame-assembler-buffer)
(global-set-key (kbd "<f1> 8") 'gdb-frame-memory-buffer)
(global-set-key (kbd "<f1> 9") 'gdb-frame-locals-buffer)
(global-set-key (kbd "<f1> 0") 'gdb-use-separate-io-buffer)
(global-set-key (kbd "<f1> -") 'gud-up)
(global-set-key (kbd "<f1> =") 'gud-down)

(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c w")   'compare-windows)
(global-set-key (kbd "M-#")     'query-replace-regexp)

;;quick move other windows
(global-set-key [M-up]    'windmove-up)
(global-set-key [M-down]  'windmove-down)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-left]  'windmove-left)
;;window size change
(global-set-key [C-S-up]    'enlarge-window)
(global-set-key [C-S-down]  'shrink-window)
(global-set-key [C-S-right] 'enlarge-window-horizontally)
(global-set-key [C-S-left]  'shrink-window-horizontally)

;;winner restore
(global-set-key [(control ,)] 'winner-undo)
(global-set-key [(control .)] 'winner-redo)

(global-set-key "%" 'match-paren)

(global-unset-key [backspace])
(global-set-key   [backspace] 'delete-backward-char)
(global-unset-key [delete])
(global-set-key   [delete] 'delete-char)

(global-set-key (kbd "M-]") 'tabbar-forward)
(global-set-key (kbd "M-[") 'tabbar-backward)

; C-- is undo (removes the shift), C-= is redo
(global-set-key [(control -)] 'undo)
(global-set-key [(control =)] 'redo)
(global-set-key [(meta =)]    'redo)

(if (or (eq window-system 'w32)
        (eq window-system 'win32))
 (progn ;; For Windows
  (global-set-key [C-wheel-up]   'text-scale-increase)
  (global-set-key [C-wheel-down] 'text-scale-decrease))
 (progn ;; For Linux
  (global-set-key [C-mouse-4] 'text-scale-increase)
  (global-set-key [C-mouse-5] 'text-scale-decrease)))

;;shift+space for mark
(global-set-key (kbd "S-<SPC>") 'set-mark-command)
;;Control tab quotes a tab.
(global-set-key [C-tab] "\C-q\t")

(provide 'key-setting)

;;; key-setting.el ends here
