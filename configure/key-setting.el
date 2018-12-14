;;;; key-setting.el --- key config file
;;;

;;;emacs default setting
;;C-x @ S        event-apply-shift-modifier
;;C-x @ a        event-apply-alt-modifier
;;C-x @ c        event-apply-control-modifier
;;C-x @ h        event-apply-hyper-modifier
;;C-x @ m        event-apply-meta-modifier
;;C-x @ s        event-apply-super-modifier

(zz-load-path "elisp")
(require 'apply-keys)

(defvar zz-apps-key (if-ms-windows [apps] [menu])
  "zz-apps-key")

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
  (kbd "C-s")     'slime-selector
  (kbd "SPC")     'er/expand-region
  (kbd "C-z")     (unless (display-graphic-p)
                    (lookup-key ctl-x-map "\C-z"))

  (kbd "`")       'zz:switch-to-term
  (kbd "1")       (zz:quick-buffer zz:term-1 "*terminal<1>*")
  (kbd "2")       (zz:quick-buffer zz:term-2 "*terminal<2>*")
  (kbd "3")       (zz:quick-buffer zz:term-3 "*terminal<3>*")
  (kbd "4")       (zz:quick-buffer zz:term-4 "*terminal<4>*")
  (kbd "5")       (zz:quick-buffer zz:term-5 "*terminal<5>*")
  (kbd "6")       (zz:quick-buffer zz:term-6 "*terminal<6>*")
  (kbd "7")       (zz:quick-buffer zz:term-7 "*terminal<7>*")
  (kbd "8")       (zz:quick-buffer zz:term-8 "*terminal<8>*")
  (kbd "9")       (zz:quick-buffer zz:term-9 "*terminal<9>*")
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-c `")   'zz:switch-to-shell
  (kbd "C-c 1")   (zz:quick-buffer zz:shell-1 "*shell<1>*")
  (kbd "C-c 2")   (zz:quick-buffer zz:shell-2 "*shell<2>*")
  (kbd "C-c 3")   (zz:quick-buffer zz:shell-3 "*shell<3>*")
  (kbd "C-c 4")   (zz:quick-buffer zz:shell-4 "*shell<4>*")
  (kbd "C-c 5")   (zz:quick-buffer zz:shell-5 "*shell<5>*")
  (kbd "C-c 6")   (zz:quick-buffer zz:shell-6 "*shell<6>*")
  (kbd "C-c 7")   (zz:quick-buffer zz:shell-7 "*shell<7>*")
  (kbd "C-c 8")   (zz:quick-buffer zz:shell-8 "*shell<8>*")
  (kbd "C-c 9")   (zz:quick-buffer zz:shell-9 "*shell<9>*")
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-1")     'delete-window
  (kbd "C-2")     'delete-frame
  (kbd "C-3")     'other-frame
  (kbd "C-4")     'zz:undo-kill-buffer
  (kbd "C-5")     'gud-until
  (kbd "C-6")     'gud-remove
  (kbd "C-7")     'gud-finish
  (kbd "C-8")     'gud-jump
  (kbd "C-9")     'gud-pstar
  (kbd "C-0")     'gud-refresh

  ;;undo/redo
  (kbd "C--")     'redo
  (kbd "C-_")     'undo
  
  (kbd "C-,")     'winner-undo
  (kbd "C-.")     'winner-redo
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "M-1")     'delete-other-windows
  (kbd "M-2")     'delete-other-frames
  (kbd "M-3")     'other-window
  (kbd "M-4")     'kill-this-buffer
  (kbd "M-5")     'gud-go
  (kbd "M-6")     'gud-break
  (kbd "M-7")     'gud-next
  (kbd "M-8")     'gud-step
  (kbd "M-9")     'gud-print
  (kbd "M-0")     'gud-watch

  (kbd "M-#")     'query-replace-regexp
  (kbd "M-+")     'smartparens-mode
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-x C-b") 'ibuffer
  (kbd "C-c w")   'compare-windows
  (kbd "C-c h")   'helm-command-prefix
  (kbd "%")       'zz:match-paren

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

  ;;zz-apps-key for M-x
  zz-apps-key     'smex
  (kbd "M-X")     'smex-major-mode-commands
  (kbd "C-x M-x") 'execute-extended-command
  ))

(apply-keys-to-map
 esc-map
 (list
  ;;quick move other windows
  [up]            (lookup-key global-map [M-up])
  [down]          (lookup-key global-map [M-down])
  [left]          (lookup-key global-map [M-left])
  [right]         (lookup-key global-map [M-right])
  ))

(apply-keys-to-map
 shift-fn-map
 (list
  ;;window size change
  [up]            (lookup-key global-map [S-up])
  [down]          (lookup-key global-map [S-down])
  [left]          (lookup-key global-map [S-left])
  [right]         (lookup-key global-map [S-right])
  ))

;;switch to shells
(apply-keys-to-map
 f4-map
 (list
  (kbd "<f4>")    'kill-this-buffer

  (kbd "<f5>")    'zz:terminator-0
  (kbd "<f6>")    'zz:terminator-1
  (kbd "<f7>")    'zz:terminator-2
  (kbd "<f8>")    'zz:terminator-3

  (kbd "<f9>")    (zz:quick-shell zz:shell-f9  "*shell-f9*")
  (kbd "<f10>")   (zz:quick-shell zz:shell-f10 "*shell-f10*")
  (kbd "<f11>")   (zz:quick-shell zz:shell-f11 "*shell-f11*")
  (kbd "<f12>")   (zz:quick-shell zz:shell-f12 "*shell-f12*")

  (kbd "C-b")     'browse-url
  (kbd "C-d")     (if-ms-windows
                   (zz:execute-key zz:explorer '("explorer" "."))
                   (zz:execute-key zz:thunar '("thunar")))
  (kbd "C-h")     'sourcepair-jump-to-headerfile
  (kbd "C-l")     'command-history
  (kbd "C-r")     'zz:add-code-review-note
  (kbd "C-s")     (if-not-ms-windows 'zz:slime-connect-stumpwm)
  (kbd "C-t")     (unless-ms-windows 'zz:open-with-terminal)

  (kbd "C-f")     'zz:secondary-x-font
  (kbd "M-f")     'zz:primary-x-font

  (kbd "M-r")     (if-not-ms-windows
                   (zz:execute-key zz:rofi-run '("rofi" "-show" "run")))
  (kbd "M-w")     (if-not-ms-windows
                   (zz:execute-key zz:rofi-window '("rofi" "-show" "window")))
  (kbd "M-s")     (if-not-ms-windows
                   (zz:execute-key zz:rofi-ssh '("rofi" "-show" "ssh")))
  (kbd "M-d")     (if-not-ms-windows
                   (zz:execute-key zz:rofi-drun '("rofi" "-show" "drun")))
  ))

(apply-keys-to-map
 f4-map
 (list
  (kbd "1")       'gdb-display-io-buffer
  (kbd "2")       'gdb-display-locals-for-thread
  (kbd "3")       'gdb-display-stack-for-thread
  (kbd "4")       'gdb-display-registers-for-thread
  (kbd "5")       'gdb-display-stack-buffer
  (kbd "6")       'gdb-display-breakpoints-buffer
  (kbd "7")       'gdb-display-assembler-buffer
  (kbd "8")       'gdb-display-memory-buffer
  (kbd "9")       'gdb-display-locals-buffer
  (kbd "0")       'gdb-display-gdb-buffer
  (kbd "-")       'gud-up
  (kbd "=")       'gud-down
  ))

;;execute start-process key
(apply-keys-to-map
 f4-e-map
 (list
  ;;quick terminal and shell buffer
  (kbd "5")       (lookup-key f4-map (kbd "<f5>"))
  (kbd "6")       (lookup-key f4-map (kbd "<f6>"))
  (kbd "7")       (lookup-key f4-map (kbd "<f7>"))
  (kbd "8")       (lookup-key f4-map (kbd "<f8>"))
  (kbd "9")       (lookup-key f4-map (kbd "<f9>"))
  (kbd "0")       (lookup-key f4-map (kbd "<f10>"))
  (kbd "-")       (lookup-key f4-map (kbd "<f11>"))
  (kbd "=")       (lookup-key f4-map (kbd "<f12>"))

  (kbd "c")       (if-not-ms-windows
                   (zz:execute-key zz:urxvt '("urxvt")))
  (kbd "d")       (if-not-ms-windows
                   (zz:execute-key zz:meld '("meld")))
  (kbd "v")       (if-not-ms-windows
                   (zz:execute-key zz:evince '("evince")))
  (kbd "f")       (if-not-ms-windows
                   (zz:execute-key zz:firefox '("firefox")))
  (kbd "m")       (if-not-ms-windows
                   (zz:execute-key zz:taskmanager '("xfce4-taskmanager")))
  (kbd "h")       (if-not-ms-windows
                   (zz:execute-key zz:thunar '("thunar")))
  (kbd "r")       (if-not-ms-windows
                   (zz:execute-key zz:remmina '("remmina")))
  (kbd "t")       (if-not-ms-windows
                   (zz:execute-key zz:tmux '("urxvt" "-e" "tmux")))
  ))

(apply-keys-to-map
 help-map
 (list
  (kbd "1")       'gdb-frame-io-buffer
  (kbd "2")       'gdb-frame-locals-for-thread
  (kbd "3")       'gdb-frame-stack-for-thread
  (kbd "4")       'gdb-frame-registers-for-thread
  (kbd "5")       'gdb-frame-stack-buffer
  (kbd "6")       'gdb-frame-breakpoints-buffer
  (kbd "7")       'gdb-frame-assembler-buffer
  (kbd "8")       'gdb-frame-memory-buffer
  (kbd "9")       'gdb-frame-locals-buffer
  (kbd "0")       'gdb-frame-gdb-buffer
  (kbd "-")       'gud-up
  (kbd "=")       'gud-down
  ))

;;helm key setting
(apply-keys-to-map
 global-map
 (list
  (kbd "C-x b")   'helm-buffers-list
  (kbd "C-x C-f") 'helm-find-files
  (kbd "C-x f")   'helm-projectile-find-file
  (kbd "C-M-z")   'helm-resume
  (kbd "M-x")     'helm-M-x

  (kbd "C-`")     'helm-imenu
  (kbd "C-~")     'helm-semantic

  (kbd "M-.")     'helm-etags-plus-select
  (kbd "M-*")     'helm-etags-plus-history
  (kbd "C-=")     'helm-etags-plus-history-go-back
  (kbd "C-+")     'helm-etags-plus-history-go-forward

  (kbd "C-c M-i") 'helm-multi-swoop
  (kbd "C-x M-i") 'helm-multi-swoop-all
  ))

(apply-keys-to-map
 help-map
 (list
  (kbd "M-a")     'helm-apropos
  (kbd "M-t")     'helm-world-time
  (kbd "M-g")     'helm-grep-do-git-grep
  (kbd "M-m")     'helm-man-woman
  (kbd "SPC")     'helm-all-mark-rings
  ))

(apply-keys-to-map
 helm-command-map
 (list
  (kbd "C-b")     'helm-mini
  ))

(apply-keys-to-map
 helm-map
 (list
  (kbd "C-p")     'helm-previous-line
  (kbd "C-n")     'helm-next-line
  (kbd "C-M-n")   'helm-next-source
  (kbd "C-M-p")   'helm-previous-source
  (kbd "M-N")     'helm-next-source
  (kbd "M-P")     'helm-previous-source
  ))


(provide 'key-setting)

;;; key-setting.el ends here
