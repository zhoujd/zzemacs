
;;;; keyz-setting.el --- Ctrl-z f4 key config file
;;;

(zz-load-path "elisp")
(require 'apply-keys)

(apply-keys-to-map
 ctrl-z-map
 (list
  (kbd "C-s")     'slime-selector
  (kbd "C-z")     'repeat
  (kbd "SPC")     'er/expand-region
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
  (kbd "\\")      'gud-refresh
  (kbd "`")       'gdb-restore-windows
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
  (kbd "C-d")     (if-ms-windows (zz:execute-key zz:explorer '("explorer" "."))
                                 (zz:execute-key zz:thunar '("thunar")))
  (kbd "C-h")     'sourcepair-jump-to-headerfile
  (kbd "C-l")     'command-history
  (kbd "C-r")     'zz:add-code-review-note
  (kbd "C-s")     'zz:slime-connect-stumpwm
  (kbd "C-t")     'zz:open-with-terminal

  (kbd "C-f")     'zz:secondary-x-font
  (kbd "M-f")     'zz:primary-x-font

  (kbd "M-r")     (zz:execute-key zz:rofi-run '("rofi" "-show" "run"))
  (kbd "M-w")     (zz:execute-key zz:rofi-window '("rofi" "-show" "window"))
  (kbd "M-s")     (zz:execute-key zz:rofi-ssh '("rofi" "-show" "ssh"))
  (kbd "M-d")     (zz:execute-key zz:rofi-drun '("rofi" "-show" "drun"))
  ))

;;execute start-process key
(apply-keys-to-map
 f4-e-map
 (list
  ;;quick terminal and shell buffer
  (kbd "1")       'zz:popup-term
  (kbd "2")       'zz:get-term
  (kbd "3")       'zz:local-shell
  (kbd "4")       'zz:remote-shell
  (kbd "5")       (lookup-key f4-map (kbd "<f5>"))
  (kbd "6")       (lookup-key f4-map (kbd "<f6>"))
  (kbd "7")       (lookup-key f4-map (kbd "<f7>"))
  (kbd "8")       (lookup-key f4-map (kbd "<f8>"))
  (kbd "9")       (lookup-key f4-map (kbd "<f9>"))
  (kbd "0")       (lookup-key f4-map (kbd "<f10>"))
  (kbd "-")       (lookup-key f4-map (kbd "<f11>"))
  (kbd "=")       (lookup-key f4-map (kbd "<f12>"))
  (kbd "\\")      'zz:baidu
  (kbd "`")       'zz:google

  (kbd "c")       (zz:execute-key zz:urxvt   '("urxvt"))
  (kbd "d")       (zz:execute-key zz:meld    '("meld"))
  (kbd "v")       (zz:execute-key zz:evince  '("evince"))
  (kbd "f")       (zz:execute-key zz:firefox '("firefox"))
  (kbd "h")       (zz:execute-key zz:thunar  '("thunar"))
  (kbd "r")       (zz:execute-key zz:remmina '("remmina"))
  (kbd "t")       (zz:execute-key zz:tmux    '("urxvt" "-e" "tmux"))
  ))

(apply-keys-to-map
 ctrl-z-map
 (list
  (kbd "1")       (lookup-key global-map [f1])
  (kbd "2")       (lookup-key global-map [f2])
  (kbd "3")       (lookup-key global-map [f3])
  (kbd "4")       (lookup-key f4-map     [f4])
  (kbd "5")       (lookup-key global-map [f5])
  (kbd "6")       (lookup-key global-map [f6])
  (kbd "7")       (lookup-key global-map [f7])
  (kbd "8")       (lookup-key global-map [f8])
  (kbd "9")       (lookup-key global-map [f9])
  (kbd "0")       (lookup-key global-map [f10])
  (kbd "-")       (lookup-key global-map [f11])
  (kbd "=")       (lookup-key global-map [f12])

  (kbd "!")       (lookup-key global-map [S-f1])
  (kbd "@")       (lookup-key global-map [S-f2])
  (kbd "#")       (lookup-key global-map [S-f3])
  (kbd "$")       (lookup-key global-map [S-f4])
  (kbd "%")       (lookup-key global-map [S-f5])
  (kbd "^")       (lookup-key global-map [S-f6])
  (kbd "&")       (lookup-key global-map [S-f7])
  (kbd "*")       (lookup-key global-map [S-f8])
  (kbd "(")       (lookup-key global-map [S-f9])
  (kbd ")")       (lookup-key global-map [S-f10])
  (kbd "_")       (lookup-key global-map [S-f11])
  (kbd "+")       (lookup-key global-map [S-f12])

  (kbd "C-1")     (lookup-key global-map [C-f1])
  (kbd "C-2")     (lookup-key global-map [C-f2])
  (kbd "C-3")     (lookup-key global-map [C-f3])
  (kbd "C-4")     (lookup-key global-map [C-f4])
  (kbd "C-5")     (lookup-key global-map [C-f5])
  (kbd "C-6")     (lookup-key global-map [C-f6])
  (kbd "C-7")     (lookup-key global-map [C-f7])
  (kbd "C-8")     (lookup-key global-map [C-f8])
  (kbd "C-9")     (lookup-key global-map [C-f9])
  (kbd "C-0")     (lookup-key global-map [C-f10])
  (kbd "C--")     (lookup-key global-map [C-f11])
  (kbd "C-=")     (lookup-key global-map [C-f12])

  (kbd "M-1")     (lookup-key global-map [M-f1])
  (kbd "M-2")     (lookup-key global-map [M-f2])
  (kbd "M-3")     (lookup-key global-map [M-f3])
  (kbd "M-4")     (lookup-key global-map [M-f4])
  (kbd "M-5")     (lookup-key global-map [M-f5])
  (kbd "M-6")     (lookup-key global-map [M-f6])
  (kbd "M-7")     (lookup-key global-map [M-f7])
  (kbd "M-8")     (lookup-key global-map [M-f8])
  (kbd "M-9")     (lookup-key global-map [M-f9])
  (kbd "M-0")     (lookup-key global-map [M-f10])
  (kbd "M--")     (lookup-key global-map [M-f11])
  (kbd "M-=")     (lookup-key global-map [M-f12])
  ))

(apply-keys-to-map
 ctrl-z-map
 (list
  (kbd "B")       (lookup-key global-map [S-left])
  (kbd "F")       (lookup-key global-map [S-right])
  (kbd "N")       (lookup-key global-map [S-down])
  (kbd "P")       (lookup-key global-map [S-up])

  (kbd "C-b")     (lookup-key global-map [C-left])
  (kbd "C-f")     (lookup-key global-map [C-right])
  (kbd "C-n")     (lookup-key global-map [C-down])
  (kbd "C-p")     (lookup-key global-map [C-up])

  (kbd "M-b")     (lookup-key global-map [M-left])
  (kbd "M-f")     (lookup-key global-map [M-right])
  (kbd "M-n")     (lookup-key global-map [M-down])
  (kbd "M-p")     (lookup-key global-map [M-up])
  ))

(apply-keys-to-map
 fn-map
 (list
  ;;quick move other windows
  (kbd "p")       (lookup-key global-map [up])
  (kbd "n")       (lookup-key global-map [down])
  (kbd "b")       (lookup-key global-map [left])
  (kbd "f")       (lookup-key global-map [right])
  ))

(apply-keys-to-map
 meta-fn-map
 (list
  ;;quick move other windows
  (kbd "p")       (lookup-key global-map [M-up])
  (kbd "n")       (lookup-key global-map [M-down])
  (kbd "b")       (lookup-key global-map [M-left])
  (kbd "f")       (lookup-key global-map [M-right])
  ))

(apply-keys-to-map
 shift-fn-map
 (list
  ;;window size change
  (kbd "p")       (lookup-key global-map [S-up])
  (kbd "n")       (lookup-key global-map [S-down])
  (kbd "b")       (lookup-key global-map [S-left])
  (kbd "f")       (lookup-key global-map [S-right])
  ))

(apply-keys-to-map
 ctrl-fn-map
 (list
  (kbd "p")       (lookup-key global-map [C-up])
  (kbd "n")       (lookup-key global-map [C-down])
  (kbd "b")       (lookup-key global-map [C-left])
  (kbd "f")       (lookup-key global-map [C-right])
  ))


(provide 'keyz-setting)

;;; keyz-setting.el ends here
