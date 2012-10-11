;;;; key-setting.el --- key config file
;;;

;;  x11        console
;;---------------------
;;f1-f12   -- esc-f1/f12
;;S-f1/f12 -- f1 -f1/f12
;;C-f1/f12 -- f2 -f1/f12
;;M-f1/f12 -- f3 -f1/f12
;;f12      -- f4 

(when (not window-system)
  ;;f2 key map
  (defvar f2-map (make-sparse-keymap) "Keymap for self related commands.")
  (define-key global-map [f2] f2-map)  
  ;;f3 key map
  (defvar f3-map (make-sparse-keymap) "Keymap for self related commands.")
  (define-key global-map [f3] f3-map)
)

;;f4 key map 
(defvar f4-map (make-sparse-keymap) "Keymap for self related commands.")
(define-key global-map [f4] f4-map)

;;switch to shells
(global-set-key (kbd "<f4> <f9>")  
  (lambda () (interactive) (switch-to-shell "*shell-f9*")))
(global-set-key (kbd "<f4> <f10>")    
  (lambda () (interactive) (switch-to-shell "*shell-f10*")))
(global-set-key (kbd "<f4> <f11>")  
  (lambda () (interactive) (switch-to-shell "*shell-f11*")))
(global-set-key (kbd "<f4> <f12>")  
  (lambda () (interactive) (switch-to-shell "*shell-f12*")))

;;for info
(when window-system
  (progn
    (global-set-key [S-f1]    'session-save)
    (global-set-key [C-f1]    'session-restore)
    (global-set-key [M-f1]    'recentf-open-files)
    (global-set-key [f4-f1]   'recentf-open-files-compl)
    
    (global-set-key   [f2]    'bc-next)
    (global-set-key [S-f2]    'bc-previous)
    (global-set-key [C-f2]    'bc-set)
    (global-set-key [M-f2]    'bc-list)
    
    
    (global-set-key   [f3]    'my-last-buffer-go)
    (global-set-key [C-f3]    'list-bookmarks)
    (global-set-key [M-f3]    'my-occur)
    
    (global-set-key [S-f4]    'undo-kill-buffer)
    (global-set-key [C-f4]    'bc-local-next)
    (global-set-key [M-f4]    'bc-local-previous)
    (global-set-key [f4-f4]   'kill-this-buffer)
    
    (global-set-key   [f5]    'speedbar-get-focus)
    (global-set-key [S-f5]    'sr-speedbar-toggle)
    (global-set-key [C-f5]    'line-to-top-of-window)
    (global-set-key [M-f5]    'etags-stack-show)
    
    (if (or (eq window-system 'w32)
            (eq window-system 'win32))
        (progn ;; For Windows
          (global-set-key   [f6]   'multi-shell-new)
          (global-set-key [S-f6]   'multi-shell-current-directory)
          (global-set-key [C-f6]   'multi-shell-next)
          (global-set-key [M-f6]   'multi-shell-prev)
          (global-set-key (kbd "C-x  <f6>") 'switch-to-shell)
          )   
        (progn ;; For Linux
          (global-set-key   [f6]   'get-term)
          (global-set-key [S-f6]   'multi-term-dedicated-toggle)
          (global-set-key [C-f6]   'multi-term-next)
          (global-set-key [M-f6]   'multi-term-prev)
          (global-set-key (kbd "C-x  <f6>")  'switch-to-term)
          ))
    
    (global-set-key   [f7]    'compile)
    (global-set-key [S-f7]    'switch-to-compilation)
    (global-set-key [C-f7]    'next-error)
    (global-set-key [M-f7]    'previous-error)
    
    (global-set-key   [f8]    'gdb)
    (global-set-key [S-f8]    'gud-kill)
    (global-set-key [C-f8]    'gdb-restore-windows)
    (global-set-key [M-f8]    'gdb-many-windows)
    
    (global-set-key   [f9]    'shell)
    (global-set-key [S-f9]    'multi-shell-new)
    (global-set-key [C-f9]    'switch-to-scratch)
    (global-set-key [M-f9]    'popup-term)
    (global-set-key (kbd "C-x  <f9>")  'switch-to-shell)
    
    (global-set-key [S-f10]   'tool-bar-mode)
    (global-set-key [C-f10]   'my-toggle-maxframe)
    (global-set-key [M-f10]  'my-toggle-fullscreen)
    
    (global-set-key   [f11]   'linum-mode)
    (global-set-key [S-f11]   'fci-mode)
    (global-set-key [C-f11]   'hl-line-mode)
    (global-set-key [M-f11]   'blank-mode)

    (global-set-key [f12]     'find-grep)
    (global-set-key [S-f12]   'rgrep)
    (global-set-key [C-f12]   'find-name-dired)
    (global-set-key [M-f12]   'my-c-rgrep)
    ))

(unless window-system
  (progn
    (global-set-key (kbd "<f1> <f1>")   'session-save)
    (global-set-key (kbd "<f2> <f1>")   'session-restore)
    (global-set-key (kbd "<f3> <f1>")   'recentf-open-files)
    (global-set-key (kbd "<f4> <f1>")   'recentf-open-files-compl)
    
    (define-key esc-map   [f2]          'bc-next)
    (global-set-key (kbd "<f1> <f2>")   'bc-previous)
    (global-set-key (kbd "<f2> <f2>")   'bc-set)
    (global-set-key (kbd "<f3> <f2>")   'bc-list)
    
    
    (define-key esc-map   [f3]          'my-last-buffer-go)
    (global-set-key (kbd "<f2> <f3>")   'list-bookmarks)
    (global-set-key (kbd "<f3> <f3>")   'my-occur)
    
    (global-set-key (kbd "<f1> <f4>")   'undo-kill-buffer)
    (global-set-key (kbd "<f2> <f4>")   'bc-local-next)
    (global-set-key (kbd "<f3> <f4>")   'bc-local-previous)
    (global-set-key (kbd "<f4> <f4>")   'kill-this-buffer)
    
    (define-key esc-map   [f5]          'speedbar-get-focus)
    (global-set-key (kbd "<f1> <f5>")   'sr-speedbar-toggle)
    (global-set-key (kbd "<f2> <f5>")   'line-to-top-of-window)
    (global-set-key (kbd "<f3> <f5>")   'etags-stack-show)
    
    (if (or (eq window-system 'w32)
            (eq window-system 'win32))
        (progn ;; For Windows
          (define-key esc-map   [f6]         'multi-shell-new)
          (global-set-key (kbd "<f1> <f6>")  'multi-shell-current-directory)
          (global-set-key (kbd "<f2> <f6>")  'multi-shell-next)
          (global-set-key (kbd "<f3> <f6>")  'multi-shell-prev)
          (global-set-key (kbd "C-x  <f6>")  'switch-to-shell)
          )   
        (progn ;; For Linux
          (define-key esc-map   [f6]         'get-term)
          (global-set-key (kbd "<f1> <f6>")  'multi-term-dedicated-toggle)
          (global-set-key (kbd "<f2> <f6>")  'multi-term-next)
          (global-set-key (kbd "<f3> <f6>")  'multi-term-prev)
          (global-set-key (kbd "C-x  <f6>")  'switch-to-term)
          ))
    
    (define-key esc-map   [f7]          'compile)
    (global-set-key (kbd "<f1> <f7>")   'switch-to-compilation)
    (global-set-key (kbd "<f2> <f7>")   'next-error)
    (global-set-key (kbd "<f3> <f7>")   'previous-error)
    
    (define-key esc-map   [f8]          'gdb)
    (global-set-key (kbd "<f1> <f8>")   'gud-kill)
    (global-set-key (kbd "<f2> <f8>")   'gdb-restore-windows)
    (global-set-key (kbd "<f3> <f8>")   'gdb-many-windows)
    
    (define-key esc-map   [f9]          'shell)
    (global-set-key (kbd "<f1> <f9>")   'multi-shell-new)
    (global-set-key (kbd "<f2> <f9>")   'switch-to-scratch)
    (global-set-key (kbd "<f3> <f9>")   'popup-term)
    (global-set-key (kbd "C-x  <f9>")   'switch-to-shell)
    
    (global-set-key (kbd "<f1> <f10>")  'tool-bar-mode)
    (global-set-key (kbd "<f2> <f10>")  'my-toggle-maxframe)
    (global-set-key (kbd "<f3> <f10>")  'my-toggle-fullscreen)
    
    (define-key esc-map   [f11]          'linum-mode)
    (global-set-key (kbd "<f1> <f11>")   'fci-mode)
    (global-set-key (kbd "<f2> <f11>")   'hl-line-mode)
    (global-set-key (kbd "<f3> <f11>")   'blank-mode)

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
(global-set-key (kbd "M-5")    'gud-go)
(global-set-key (kbd "M-6")    'gud-break-remove)
(global-set-key (kbd "M-7")    'gud-next)
(global-set-key (kbd "M-8")    'gud-step)
(global-set-key (kbd "M-9")    'gud-print)
(global-set-key (kbd "M-0")    'other-window)

(if window-system
    (progn
      (global-set-key (kbd "C-1") 'delete-window)
      (global-set-key (kbd "C-2") 'delete-frames)
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
      (global-set-key (kbd "<f2> 1") 'delete-window)
      (global-set-key (kbd "<f2> 2") 'delete-frames)
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
(global-set-key (kbd "<f1> 3") 'highlight-symbol-next)
(global-set-key (kbd "<f1> 4") 'highlight-symbol-prev)

;;gdb frame show setting
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
(define-key f4-map (kbd "C-1") 'my-utf-8)
(define-key f4-map (kbd "C-h") 'sourcepair-jump-to-headerfile)
(unless (or (eq window-system 'w32)
            (eq window-system 'win32))
  (define-key f4-map (kbd "C-d") 'open-with-nautilus)
  (define-key f4-map (kbd "C-t") 'open-with-terminal))

;;undo/redo
(if window-system
    (progn
      (define-key global-map (kbd "C--") 'undo)
      (define-key global-map (kbd "C-=") 'redo))
    (progn
      (define-key f2-map (kbd "-") 'undo)
      (define-key f2-map (kbd "=") 'redo))
    )

;;winner restore
(if window-system
    (progn
      (define-key global-map (kbd "C-,") 'winner-undo)
      (define-key global-map (kbd "C-.") 'winner-redo))
    (progn
      (define-key f3-map (kbd ",") 'winner-undo)
      (define-key f3-map (kbd ".") 'winner-redo))
    )

;;quick move other windows
(if window-system
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
(when (not window-system)
  (define-key f2-map (kbd "<SPC>") 'set-mark-command)
  )

;;;f3-map setting
;;window size change
(if window-system
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

(provide 'key-setting)

;;; key-setting.el ends here
