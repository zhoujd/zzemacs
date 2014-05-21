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
  (kbd "C-z h") 'event-apply-hyper-modifier
  (kbd "C-z a") 'event-apply-alt-modifier
  (kbd "M-[ h") 'event-apply-hyper-modifier
  (kbd "M-[ a") 'event-apply-alt-modifier
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
  (kbd "C-z")  (lookup-key ctl-x-map "\C-z")
  ))

(apply-keys-to-map
 global-map
 (list
  ;;ctrl number setting
  (kbd "C-`") 'imenu
  (kbd "C-1") 'delete-window
  (kbd "C-2") 'delete-frame
  (kbd "C-3") 'ibuffer
  (kbd "C-4") 'delete-frame
  ;;gud control setting
  (kbd "C-5") 'gud-until
  (kbd "C-6") 'gud-remove
  (kbd "C-7") 'gud-finish
  (kbd "C-8") 'gud-jump
  (kbd "C-9") 'gud-pstar
  (kbd "C-0") 'other-frame
  (kbd "C--") 'redo
  (kbd "C-=") 'er/expand-region
  ;;undo/redo
  (kbd "C-,") 'winner-undo
  (kbd "C-.") 'winner-redo

  ;;Control tab quotes a tab => "\C-q\t"
  [(control tab)] "\C-q\t"
  ))
 
(apply-keys-to-map
 global-map
 (list
  (kbd "M-1") 'delete-other-windows
  (kbd "M-2") 'delete-other-frames
  (kbd "M-3") 'iswitchb-buffer
  (kbd "M-4") 'kill-this-buffer
  ;;gud control setting
  (kbd "M-5") (if-ms-windows 'gud-cont 'gud-go)
  (kbd "M-6") 'gud-break
  (kbd "M-7") 'gud-next
  (kbd "M-8") 'gud-step
  (kbd "M-9") 'gud-print
  (kbd "M-0") 'other-window
  
  ;;find-replace
  (kbd "M-#") 'query-replace-regexp
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-c s")   'slime-selector
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
  (kbd "i") 'zz-info-open-file
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
  "3" (when-ms-windows (execute-set-key
                        "git-shell"
                        '("cmd" "/c" "start" "sh" "--login" "-i")))
  "t" 'open-with-terminal
  "f" (execute-set-key "firefox"  '("firefox" "http://www.baidu.com"))
  "b" (execute-set-key "bcompare" '("bcompare"))
  ;;;for linux quick run
  "`" (if-not-ms-windows (execute-set-key "gmrun" '("gmrun")))
  "m" (if-not-ms-windows (execute-set-key
                          "gnome-system-monitor" '("gnome-system-monitor")))
  "c" (if-not-ms-windows (execute-set-key
                          "gnome-control-center" '("gnome-control-center")))
  "e" (if-not-ms-windows (execute-set-key "evince" '("evince")))
  "n" (if-not-ms-windows (execute-set-key "nautilus" '("nautilus" "--no-desktop")))
  "r" (if-not-ms-windows (execute-set-key "remmina" '("remmina")))
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

  (kbd "C-=")   'er/expand-region
  (kbd "C--")   (if-emacs24-3 'smartparens-mode)
  
  (kbd "C-1")   'my-utf-8
  (kbd "C-b")   'browse-url
  (kbd "C-d")   (if-ms-windows
                 (execute-set-key
                  "explorer"
                  (list "explorer"
                        (my-trans-path-sep
                         default-directory "/" "\\")))
                 'open-with-nautilus)  
  (kbd "C-h")   'sourcepair-jump-to-headerfile
  (kbd "C-l")   'command-history
  (kbd "C-r")   'add-code-review-note
  (kbd "C-s")   (if-not-ms-windows 'slime-connect-stumpwm)
  (kbd "C-t")   (unless-ms-windows 'open-with-terminal)
  ))

;;quick shell setting
(apply-keys-to-map
 f4-map
 (list
  (kbd "9")     (lookup-key f4-map [f9])
  (kbd "0")     (lookup-key f4-map [f10])
  (kbd "-")     (lookup-key f4-map [f11])
  (kbd "=")     (lookup-key f4-map [f12])
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
  
  ;;window size change
  [up]      (lookup-key global-map [S-up])
  [down]    (lookup-key global-map [S-down])
  [left]    (lookup-key global-map [S-left])
  [right]   (lookup-key global-map [S-right])  
  ))

(provide 'key-setting)

;;; key-setting.el ends here
