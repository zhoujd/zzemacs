;;;; key-setting.el --- key config file
;;;
;;;;emacs default setting
;;C-x @ S        event-apply-shift-modifier
;;C-x @ a        event-apply-alt-modifier
;;C-x @ c        event-apply-control-modifier
;;C-x @ h        event-apply-hyper-modifier
;;C-x @ m        event-apply-meta-modifier
;;C-x @ s        event-apply-super-modifier
(defvar zz/apps-key
  (if-ms-windows [apps] [menu])
  "zz/apps-key")

(apply-keys-to-map
 key-translation-map
 (list
  (kbd "M-] h")  'event-apply-hyper-modifier
  (kbd "M-] a")  'event-apply-alt-modifier
  (kbd "M-] s")  'event-apply-super-modifier

  (kbd "M-] c")  'event-apply-control-modifier
  (kbd "M-] m")  'event-apply-meta-modifier
  (kbd "M-] S")  'event-apply-shift-modifier
  ))

;;all of these sequences are translated from term/xterm.el .
;;if emacs can't create keymap correctly from you TERM
;;you can force to load it
(unless-ms-windows
 (if (and (not (display-graphic-p))
          (load-library "term/xterm"))
     (terminal-init-xterm)))

(apply-keys-to-map
 ctl-z-map
 (list
  (kbd "C-s") 'slime-selector
  (kbd "C-z") (lookup-key ctl-x-map "\C-z")

  (kbd "`")   'switch-to-term
  (kbd "1")   (switch-quick-buffer "*terminal<1>*")
  (kbd "2")   (switch-quick-buffer "*terminal<2>*")
  (kbd "3")   (switch-quick-buffer "*terminal<3>*")
  (kbd "4")   (switch-quick-buffer "*terminal<4>*")
  (kbd "5")   (switch-quick-buffer "*terminal<5>*")
  (kbd "6")   (switch-quick-buffer "*terminal<6>*")
  (kbd "7")   (switch-quick-buffer "*terminal<7>*")
  (kbd "8")   (switch-quick-buffer "*terminal<8>*")
  (kbd "9")   (switch-quick-buffer "*terminal<9>*")
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "M-] `")   'switch-to-shell
  (kbd "M-] 1")   (switch-quick-buffer "*shell<1>*")
  (kbd "M-] 2")   (switch-quick-buffer "*shell<2>*")
  (kbd "M-] 3")   (switch-quick-buffer "*shell<3>*")
  (kbd "M-] 4")   (switch-quick-buffer "*shell<4>*")
  (kbd "M-] 5")   (switch-quick-buffer "*shell<5>*")
  (kbd "M-] 6")   (switch-quick-buffer "*shell<6>*")
  (kbd "M-] 7")   (switch-quick-buffer "*shell<7>*")
  (kbd "M-] 8")   (switch-quick-buffer "*shell<8>*")
  (kbd "M-] 9")   (switch-quick-buffer "*shell<9>*")
  ))

(apply-keys-to-map
 global-map
 (list
  ;;ctrl number setting
  (kbd "C-`") 'imenu
  (kbd "C-1") 'delete-window
  (kbd "C-2") 'delete-frame
  (kbd "C-3") 'other-frame
  (kbd "C-4") 'undo-kill-buffer
  ;;gud control setting
  (kbd "C-5") 'gud-until
  (kbd "C-6") 'gud-remove
  (kbd "C-7") 'gud-finish
  (kbd "C-8") 'gud-jump
  (kbd "C-9") 'gud-pstar
  (kbd "C-0") 'gud-refresh
  (kbd "C--") 'redo
  (kbd "C-=") 'er/expand-region

  (kbd "C-_") 'undo
  (kbd "C-+") 'smartparens-mode

  ;;undo/redo
  (kbd "C-,") 'winner-undo
  (kbd "C-.") 'winner-redo
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "M-1") 'delete-other-windows
  (kbd "M-2") 'delete-other-frames
  (kbd "M-3") 'other-window
  (kbd "M-4") 'kill-this-buffer
  ;;gud control setting
  (kbd "M-5") 'gud-go
  (kbd "M-6") 'gud-break
  (kbd "M-7") 'gud-next
  (kbd "M-8") 'gud-step
  (kbd "M-9") 'gud-print
  (kbd "M-0") 'gud-watch

  ;;find-replace
  (kbd "M-#") 'query-replace-regexp
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-x C-b") 'ibuffer
  (kbd "C-c w")   'compare-windows
  (kbd "C-c h")   'helm-mini

  (kbd "%")       'match-paren

  [backspace]     'delete-backward-char
  [delete]        'delete-char

  [C-wheel-up]    (when-ms-windows 'text-scale-increase)
  [C-wheel-down]  (when-ms-windows 'text-scale-decrease)
  [C-mouse-4]     (unless-ms-windows 'text-scale-increase)
  [C-mouse-5]     (unless-ms-windows 'text-scale-decrease)

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

  ;;zz/apps-key for M-x
  zz/apps-key     'smex
  (kbd "M-x")     'smex
  (kbd "M-X")     'smex-major-mode-commands
  (kbd "C-x M-x") 'execute-extended-command
  ))

(apply-keys-to-map
 esc-map
 (list
  ;;quick move other windows
  [up]    (lookup-key global-map [M-up])
  [down]  (lookup-key global-map [M-down])
  [left]  (lookup-key global-map [M-left])
  [right] (lookup-key global-map [M-right])
  ))

;;number 0-1/-/=
(apply-keys-to-map
 zz/meta-map
 (list
  (kbd "1") (lookup-key global-map (kbd "M-1"))
  (kbd "2") (lookup-key global-map (kbd "M-2"))
  (kbd "3") (lookup-key global-map (kbd "M-3"))
  (kbd "4") (lookup-key global-map (kbd "M-4"))
  (kbd "5") (lookup-key global-map (kbd "M-5"))
  (kbd "6") (lookup-key global-map (kbd "M-6"))
  (kbd "7") (lookup-key global-map (kbd "M-7"))
  (kbd "8") (lookup-key global-map (kbd "M-8"))
  (kbd "9") (lookup-key global-map (kbd "M-9"))
  (kbd "0") (lookup-key global-map (kbd "M-0"))
  (kbd "-") (lookup-key global-map (kbd "M--"))
  (kbd "=") (lookup-key global-map (kbd "M-="))

  ;;find-replace
  (kbd "#") (lookup-key global-map (kbd "M-#"))

  ;;quick move other windows
  [up]      (lookup-key global-map [M-up])
  [down]    (lookup-key global-map [M-down])
  [left]    (lookup-key global-map [M-left])
  [right]   (lookup-key global-map [M-right])
  ))

(apply-keys-to-map
 zz/ctrl-map
 (list
  (kbd "`") (lookup-key global-map (kbd "C-`"))
  (kbd "1") (lookup-key global-map (kbd "C-1"))
  (kbd "2") (lookup-key global-map (kbd "C-2"))
  (kbd "3") (lookup-key global-map (kbd "C-3"))
  (kbd "4") (lookup-key global-map (kbd "C-4"))
  (kbd "5") (lookup-key global-map (kbd "C-5"))
  (kbd "6") (lookup-key global-map (kbd "C-6"))
  (kbd "7") (lookup-key global-map (kbd "C-7"))
  (kbd "8") (lookup-key global-map (kbd "C-8"))
  (kbd "9") (lookup-key global-map (kbd "C-9"))
  (kbd "0") (lookup-key global-map (kbd "C-0"))
  (kbd "-") (lookup-key global-map (kbd "C--"))
  (kbd "=") (lookup-key global-map (kbd "C-="))

  ;;winner restore
  (kbd ",") (lookup-key global-map (kbd "C-,"))
  (kbd ".") (lookup-key global-map (kbd "C-."))

  ;;Control tab quotes a tab => "\C-q\t"
  [(tab)]   (lookup-key global-map [(control tab)])

  (kbd "h") 'common-lisp-hyperspec
  (kbd "i") 'open-info-file
  ))

(apply-keys-to-map
 zz/shift-fn-map
 (list
  ;;window size change
  [up]      (lookup-key global-map [S-up])
  [down]    (lookup-key global-map [S-down])
  [left]    (lookup-key global-map [S-left])
  [right]   (lookup-key global-map [S-right])
  ))

;;switch to shells
(apply-keys-to-map
 f4-map
 (list
  (kbd "<f4>")  'kill-this-buffer

  (kbd "<f5>")  (lambda () (interactive) (terminator-open-template 0))
  (kbd "<f6>")  (lambda () (interactive) (terminator-open-template 1))
  (kbd "<f7>")  (lambda () (interactive) (terminator-open-template 2))
  (kbd "<f8>")  (lambda () (interactive) (terminator-open-template 3))

  (kbd "<f9>")  (start-quick-shell "*shell-f9*")
  (kbd "<f10>") (start-quick-shell "*shell-f10*")
  (kbd "<f11>") (start-quick-shell "*shell-f11*")
  (kbd "<f12>") (start-quick-shell "*shell-f12*")

  (kbd "C-=")   'er/expand-region
  (kbd "C--")   'smartparens-mode

  (kbd "C-b")   'browse-url
  (kbd "C-d")   (if-ms-windows
                 (execute-set-key "explorer" (list "explorer" "."))
                 (execute-set-key "thunar" '("thunar")))
  (kbd "C-h")   'sourcepair-jump-to-headerfile
  (kbd "C-l")   'command-history
  (kbd "C-r")   'add-code-review-note
  (kbd "C-s")   (if-not-ms-windows 'slime-connect-stumpwm)
  (kbd "C-t")   (unless-ms-windows 'open-with-terminal)

  (kbd "C-f")   'secondary-x-font
  (kbd "M-f")   'primary-x-font

  (kbd "M-r")   (if-not-ms-windows
                 (execute-set-key "rofi-run" '("rofi" "-show" "run")))
  (kbd "M-w")   (if-not-ms-windows
                 (execute-set-key "rofi-window" '("rofi" "-show" "window")))
  (kbd "M-s")   (if-not-ms-windows
                 (execute-set-key "rofi-ssh" '("rofi" "-show" "ssh")))
  (kbd "M-d")   (if-not-ms-windows
                 (execute-set-key "rofi-drun" '("rofi" "-show" "drun")))
  ))

(apply-keys-to-map
 f4-map
 (list
  (kbd "1")     'gdb-display-io-buffer
  (kbd "2")     'gdb-display-locals-for-thread
  (kbd "3")     'gdb-display-stack-for-thread
  (kbd "4")     'gdb-display-registers-for-thread
  (kbd "5")     'gdb-display-stack-buffer
  (kbd "6")     'gdb-display-breakpoints-buffer
  (kbd "7")     'gdb-display-assembler-buffer
  (kbd "8")     'gdb-display-memory-buffer
  (kbd "9")     'gdb-display-locals-buffer
  (kbd "0")     'gdb-display-gdb-buffer
  (kbd "-")     'gud-up
  (kbd "=")     'gud-down
  ))

;;execute start-process key
(apply-keys-to-map
 f4-e-map
 (list
  ;;quick terminal and shell buffer
  (kbd "5")  (lookup-key f4-map (kbd "<f5>"))
  (kbd "6")  (lookup-key f4-map (kbd "<f6>"))
  (kbd "7")  (lookup-key f4-map (kbd "<f7>"))
  (kbd "8")  (lookup-key f4-map (kbd "<f8>"))
  (kbd "9")  (lookup-key f4-map (kbd "<f9>"))
  (kbd "0")  (lookup-key f4-map (kbd "<f10>"))
  (kbd "-")  (lookup-key f4-map (kbd "<f11>"))
  (kbd "=")  (lookup-key f4-map (kbd "<f12>"))

  (kbd "c")  (if-not-ms-windows
              (execute-set-key "urxvt" (list "urxvt")))
  (kbd "d")  (if-not-ms-windows
              (execute-set-key "meld" '("meld")))
  (kbd "v")  (if-not-ms-windows
              (execute-set-key "evince" '("evince")))
  (kbd "f")  (if-not-ms-windows
              (execute-set-key "firefox" '("firefox")))
  (kbd "m")  (if-not-ms-windows
              (execute-set-key "xfce4-taskmanager" '("xfce4-taskmanager")))
  (kbd "h")  (if-not-ms-windows
              (execute-set-key "thunar" '("thunar")))
  (kbd "r")  (if-not-ms-windows
              (execute-set-key "remmina" '("remmina")))
  (kbd "t")  (if-not-ms-windows
              (execute-set-key "tmux" '("urxvt" "-e" "tmux")))
  ))

(apply-keys-to-map
 help-map
 (list
  (kbd "1")     'gdb-frame-io-buffer
  (kbd "2")     'gdb-frame-locals-for-thread
  (kbd "3")     'gdb-frame-stack-for-thread
  (kbd "4")     'gdb-frame-registers-for-thread
  (kbd "5")     'gdb-frame-stack-buffer
  (kbd "6")     'gdb-frame-breakpoints-buffer
  (kbd "7")     'gdb-frame-assembler-buffer
  (kbd "8")     'gdb-frame-memory-buffer
  (kbd "9")     'gdb-frame-locals-buffer
  (kbd "0")     'gdb-frame-gdb-buffer
  (kbd "-")     'gud-up
  (kbd "=")     'gud-down
  ))

(provide 'key-setting)

;;; key-setting.el ends here
