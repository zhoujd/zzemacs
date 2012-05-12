;;;; key-setting.el --- key config file
;;;

;;myself key map
(defvar esc-f2-map (make-sparse-keymap)
  "Keymap for self related commands.")
(define-key esc-map [f2] esc-f2-map)

(defvar ctl/x-f2-map (make-sparse-keymap)
  "Keymap for self related commands.")
(define-key ctl-x-map [f2] ctl/x-f2-map)

(define-key esc-f2-map "1"  'my-utf-8)
(define-key ctl/x-f2-map "1"  'my-utf-8)

(defvar f12-map (make-sparse-keymap)
  "Keymap for self related commands.")
(define-key global-map [f12] f12-map)

(define-key f12-map "1" 'my-utf-8)
(define-key f12-map "9" 'goto-gnome-terminal)
;;switch head file
(define-key f12-map "h" 'sourcepair-jump-to-headerfile)

;;switch to shells
(define-key f12-map [f9]
  (lambda () (interactive) (shell-create-by-name "*shell-f9*")))
(define-key f12-map [f10]
  (lambda () (interactive) (shell-create-by-name "*shell-f10*")))
(define-key f12-map [f11]
  (lambda () (interactive) (shell-create-by-name "*shell-f11*")))

;;evil mode
(global-set-key (kbd "<f12> <f12>") 'evil-mode)

;;quick move other windows
(global-set-key (kbd "<f12> <left>") 'windmove-left)
(global-set-key (kbd "<f12> <right>") 'windmove-right)
(global-set-key (kbd "<f12> <up>") 'windmove-up)
(global-set-key (kbd "<f12> <down>") 'windmove-down)

;;for info
(global-set-key [f1] 'info)

(global-set-key [(control f2)]  'bc-set)
(global-set-key [(f2)]          'bc-previous)
(global-set-key [(shift f2)]    'bc-next)
(global-set-key [(meta f2)]     'bc-list)

(global-set-key [f3] 'my-last-buffer-go)
(global-set-key [C-f3] 'list-bookmarks)
(global-set-key [M-f3] 'line-to-top-of-window)
(global-set-key [S-f3] 'sourcepair-jump-to-headerfile)

(global-set-key [f4] 'kill-this-buffer)

(global-set-key [f5] 'speedbar-get-focus)
(global-set-key [C-f5] 'his-speedbar-no-separate-frame)
(global-set-key [S-f5] 'sr-speedbar-toggle)

(if (or (eq window-system 'w32)
        (eq window-system 'win32))
 (progn ;; For Windows
   (global-set-key [f6] 'edit-with-gvim)
   )
 (progn ;; For Linux
   (global-set-key [f6] 'get-term)
   (global-set-key [S-f6] 'multi-term-dedicated-toggle)
   (global-set-key [C-f6] 'multi-term-prev)
   (global-set-key [M-f6] 'multi-term-next))
 )

(global-set-key [f7] 'compile)
(global-set-key [S-f7] 'to-compilation)
(global-set-key [C-f7] 'next-error)
(global-set-key [M-f7] 'previous-error)

(global-set-key [f8] 'gdb-or-gud-go)
(global-set-key [M-f8] 'gud-kill)
(global-set-key [S-f8] 'gdb-many-windows)
(global-set-key [C-f8] 'gdb-restore-windows)

;;eshell,shell,terminal-emulator
(global-set-key [f9] 'shell)
(global-set-key [C-f9] 'to-scratch)
(global-set-key [M-f9] 'multi-shell-current-directory)
(global-set-key [S-f9] 'popup-term)


(global-set-key [C-f10] 'tool-bar-mode)
(global-set-key [M-f10] 'my-toggle-fullscreen)

(global-set-key [f11] 'linum-mode)
(global-set-key [C-f11] 'hl-line-mode)
(global-set-key [M-f11] 'blank-mode)
(global-set-key [S-f11] 'fci-mode)

(global-set-key [C-f12] 'my-unicad-switch)
(global-set-key [M-f12] 'my-os-file-switch)

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "C-1") 'delete-window)

(global-set-key (kbd "M-2") 'my-toggle-fullscreen)

(global-set-key (kbd "M-3") 'display-buffer-name)
(global-set-key (kbd "C-3") 'etags-stack-show)

(global-set-key (kbd "M-4") 'kill-this-buffer)
(global-set-key (kbd "C-4") 'delete-frame)

(global-set-key (kbd "M-5") 'gud-cont)
(global-set-key (kbd "C-5") 'gud-until)

(global-set-key (kbd "M-6") 'gud-break-remove)
(global-set-key (kbd "C-6") 'gdb-frame-stack-buffer)

(global-set-key (kbd "M-7") 'gud-next)
(global-set-key (kbd "C-7") 'gud-finish)

(global-set-key (kbd "M-8") 'gud-step)
(global-set-key (kbd "C-8") 'gud-jump)

(global-set-key (kbd "M-9") 'gud-print)
(global-set-key (kbd "C-9") 'gud-watch)

(global-set-key (kbd "M-0") 'other-frame)
(global-set-key (kbd "C-0") 'other-window)

(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c w") 'compare-windows)
(global-set-key (kbd "M-#") 'query-replace-regexp)

(global-set-key [M-left] 'winner-undo)
(global-set-key [M-right] 'winner-redo)

(global-set-key [M-up] 'enlarge-window)
(global-set-key [M-down] 'enlarge-window-horizontally)

(global-set-key [C-left] 'swbuff-switch-to-previous-buffer)
(global-set-key [C-right] 'swbuff-switch-to-next-buffer)

(global-set-key [(control ,)]  'mswbuff-switch-to-previous-buffer)
(global-set-key [(control .)]  'mswbuff-switch-to-next-buffer)

(global-set-key "%" 'match-paren)

(global-unset-key [backspace] )
(global-set-key [backspace] 'delete-backward-char)
(global-unset-key [delete] )
(global-set-key [delete] 'delete-char)

(global-set-key [(control tab)] 'tabbar-forward)
(if (or (eq window-system 'w32)
        (eq window-system 'win32))
    (global-set-key [(control shift tab)] 'tabbar-backward)
    (global-set-key [C-S-iso-lefttab] 'tabbar-backward))

(global-set-key (kbd "M-]") 'tabbar-forward-group)
(global-set-key (kbd "M-[") 'tabbar-backward-group)

; C-- is undo (removes the shift), C-= is redo
(global-set-key [(control -)] 'undo)
(global-set-key [(control =)] 'redo)
(global-set-key [(meta =)] 'redo)

(if (or (eq window-system 'w32)
        (eq window-system 'win32))
 (progn ;; For Windows
  (global-set-key [C-wheel-up] 'text-scale-increase)
  (global-set-key [C-wheel-down] 'text-scale-decrease))
 (progn ;; For Linux
  (global-set-key [C-mouse-4] 'text-scale-increase)
  (global-set-key [C-mouse-5] 'text-scale-decrease)))

;;shift+space for mark
(global-set-key (kbd "S-<SPC>") 'set-mark-command)

;;; key-setting.el ends here
