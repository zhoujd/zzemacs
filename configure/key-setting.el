;;;; key-setting.el --- key config file
;;;

;;;emacs default setting
;;C-x @ S         event-apply-shift-modifier
;;C-x @ a         event-apply-alt-modifier
;;C-x @ c         event-apply-control-modifier
;;C-x @ h         event-apply-hyper-modifier
;;C-x @ m         event-apply-meta-modifier
;;C-x @ s         event-apply-super-modifier

(zz:load-path "elisp")
(require 'apply-keys)

(apply-keys-to-map
 key-translation-map
 (list
  (kbd "M-] h")   'event-apply-hyper-modifier
  (kbd "M-] a")   'event-apply-alt-modifier
  (kbd "M-] s")   'event-apply-super-modifier
  (kbd "M-] c")   'event-apply-control-modifier
  (kbd "M-] m")   'event-apply-meta-modifier
  (kbd "M-] S")   'event-apply-shift-modifier
  ))

(apply-keys-to-map
 ctrl-z-map
 (list
  (kbd "SPC")     'er/expand-region
  (kbd "RET")     'zz:sudo-edit-current-file
  (kbd "TAB")     'zz:insert-file-name
  (kbd "`")       'ztree-diff-1
  (kbd "M-`")     'ztree-dir-1
  (kbd "/")       'zz:ctrl-z-help
  (kbd "b")       'helm-bm
  (kbd "d")       'docker
  (kbd "o")       'zz:last-buffer-go
  (kbd "C-r")     'recentf-open-files
  (kbd "C-s")     'slime-selector
  (kbd "t")       'zz:trans-shell
  (kbd "C-t")     'translate-shell-brief
  (kbd "M-t")     'translate-shell
  (kbd "C-z")     'repeat
  ))

(apply-keys-to-map
 f4-map
 (list
  (kbd "1")       (zz:quick-shell zz:shell-f1  "*shell<f1>*")
  (kbd "2")       (zz:quick-shell zz:shell-f2  "*shell<f2>*")
  (kbd "3")       (zz:quick-shell zz:shell-f3  "*shell<f3>*")
  (kbd "4")       (zz:quick-shell zz:shell-f4  "*shell<f4>*")
  (kbd "5")       (zz:quick-shell zz:shell-f5  "*shell<f5>*")
  (kbd "6")       (zz:quick-shell zz:shell-f6  "*shell<f6>*")
  (kbd "7")       (zz:quick-shell zz:shell-f7  "*shell<f7>*")
  (kbd "8")       (zz:quick-shell zz:shell-f8  "*shell<f8>*")
  (kbd "9")       (zz:quick-shell zz:shell-f9  "*shell<f9>*")
  (kbd "0")       (zz:quick-shell zz:shell-f10 "*shell<f10>*")
  (kbd "-")       'zz:cd-shell
  (kbd "=")       'zz:remote-shell
  (kbd "\\")      'zz:get-shell
  (kbd "DEL")     'zz:get-shell
  (kbd "`")       'zz:switch-to-fn-shell

  (kbd "C-1")     (zz:quick-buffer zz:shell-1  "*shell<1>*")
  (kbd "C-2")     (zz:quick-buffer zz:shell-2  "*shell<2>*")
  (kbd "C-3")     (zz:quick-buffer zz:shell-3  "*shell<3>*")
  (kbd "C-4")     (zz:quick-buffer zz:shell-4  "*shell<4>*")
  (kbd "C-5")     (zz:quick-buffer zz:shell-5  "*shell<5>*")
  (kbd "C-6")     (zz:quick-buffer zz:shell-6  "*shell<6>*")
  (kbd "C-7")     (zz:quick-buffer zz:shell-7  "*shell<7>*")
  (kbd "C-8")     (zz:quick-buffer zz:shell-8  "*shell<8>*")
  (kbd "C-9")     (zz:quick-buffer zz:shell-9  "*shell<9>*")
  (kbd "C-0")     (zz:quick-buffer zz:shell-10 "*shell<10>*")
  (kbd "C--")     'zz:cd-shell
  (kbd "C-=")     'zz:remote-shell
  (kbd "C-\\")    'zz:get-shell
  (kbd "C-DEL")   'zz:get-shell
  (kbd "C-`")     'zz:switch-to-c-shell

  (kbd "M-1")     (zz:quick-buffer zz:term-1  "*terminal<1>*")
  (kbd "M-2")     (zz:quick-buffer zz:term-2  "*terminal<2>*")
  (kbd "M-3")     (zz:quick-buffer zz:term-3  "*terminal<3>*")
  (kbd "M-4")     (zz:quick-buffer zz:term-4  "*terminal<4>*")
  (kbd "M-5")     (zz:quick-buffer zz:term-5  "*terminal<5>*")
  (kbd "M-6")     (zz:quick-buffer zz:term-6  "*terminal<6>*")
  (kbd "M-7")     (zz:quick-buffer zz:term-7  "*terminal<7>*")
  (kbd "M-8")     (zz:quick-buffer zz:term-8  "*terminal<8>*")
  (kbd "M-9")     (zz:quick-buffer zz:term-9  "*terminal<9>*")
  (kbd "M-0")     (zz:quick-buffer zz:term-10 "*terminal<10>*")
  (kbd "M--")     'zz:cd-term
  (kbd "M-=")     'zz:remote-term
  (kbd "M-\\")    'multi-term
  (kbd "M-DEL")   'multi-term
  (kbd "M-`")     'zz:switch-to-term
  ))

;;switch to shells
(apply-keys-to-map
 f4-map
 (list
  [f4]            'kill-this-buffer
  (kbd "SPC")     'helm-ispell

  (kbd "b")       'helm-switchb-shell-list
  (kbd "M-b")     'helm-mt
  (kbd "d")       'helm-switchb-dired-list
  (kbd "t")       'helm-tramp

  (kbd "C-b")     'browse-url
  (kbd "C-d")     (if-ms-windows (zz:execute-key zz:explorer '("explorer" "."))
                                 (zz:execute-key zz:thunar '("thunar")))
  (kbd "C-h")     'sourcepair-jump-to-headerfile
  (kbd "C-l")     'command-history
  (kbd "C-r")     'zz:add-code-review-note
  (kbd "C-s")     'zz:slime-connect-stumpwm
  (kbd "C-t")     'zz:open-with-terminal
  (kbd "M-t")     'zz:transparency-toggle
  (kbd "C-n")     'backlight
  (kbd "C-u")     'disk-usage

  (kbd "M-d")     (zz:execute-key zz:rofi-drun   '("rofi" "-show" "drun"))
  (kbd "M-r")     (zz:execute-key zz:rofi-run    '("rofi" "-show" "run"))
  (kbd "M-s")     (zz:execute-key zz:rofi-ssh    '("rofi" "-show" "ssh"))
  (kbd "M-w")     (zz:execute-key zz:rofi-win    '("rofi" "-show" "window"))

  (kbd "/")       'bongo
  ))

;;execute start-process key
(apply-keys-to-map
 f4-e-map
 (list
  (kbd "\\")      'zz:baidu
  (kbd "`")       'zz:google

  (kbd "c")       (zz:execute-key zz:urxvt       '("urxvt"))
  (kbd "f")       (zz:execute-key zz:firefox     '("firefox"))
  (kbd "g")       (zz:execute-key zz:chrome      '("google-chrome"))
  (kbd "m")       (zz:execute-key zz:evolution   '("evolution"))

  (kbd "h")       (zz:execute-key zz:thunar      '("thunar"))
  (kbd "v")       (zz:execute-key zz:vim         '("urxvt" "-e" "vim"))
  (kbd "t")       (zz:execute-key zz:tmux        '("urxvt" "-e" "tmux"))

  (kbd "d")       (zz:execute-key zz:rofi-drun   '("rofi" "-show" "drun"))
  (kbd "r")       (zz:execute-key zz:rofi-run    '("rofi" "-show" "run"))
  (kbd "s")       (zz:execute-key zz:rofi-ssh    '("rofi" "-show" "ssh"))
  (kbd "w")       (zz:execute-key zz:rofi-win    '("rofi" "-show" "window"))

  (kbd "M-g")     (zz:execute-key zz:gitg        '("gitg"))
  (kbd "M-m")     (zz:execute-key zz:meld        '("meld"))
  (kbd "M-t")     (zz:execute-key zz:trans       '("urxvt" "-e" "trans" "-I"))
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-1")     'delete-window
  (kbd "C-2")     'delete-frame
  (kbd "C-3")     'zz:last-frame-go
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
  (kbd "M-3")     'zz:last-buffer-go
  (kbd "M-4")     'zz:kill-this-buffer
  (kbd "M-5")     'gud-go
  (kbd "M-6")     'gud-break
  (kbd "M-7")     'gud-next
  (kbd "M-8")     'gud-step
  (kbd "M-9")     'gud-print
  (kbd "M-0")     'gdb-restore-windows

  (kbd "M-#")     'query-replace-regexp
  (kbd "M-+")     'smartparens-mode
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-x C-b") 'ibuffer
  (kbd "C-x C-f") 'zz:find-file
  (kbd "C-x g")   'magit-status
  (kbd "C-x M-g") 'git-gutter+-mode
  (kbd "C-c w")   'compare-windows
  (kbd "C-c ;")   'iedit-mode
  (kbd "C-c M-;") 'iedit-dwim
  (kbd "C-c M-h") 'discover-my-major
  (kbd "%")       'zz:match-paren

  (kbd "C-h")     'delete-backward-char
  [backspace]     'delete-backward-char
  [delete]        'delete-char

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

  ;;smex
  (kbd "M-X")     'smex
  (kbd "C-x M-x") 'execute-extended-command
  (kbd "C-c M-x") 'smex-major-mode-commands
  ))

;;helm key setting
(apply-keys-to-map
 global-map
 (list
  (kbd "C-c h")   'helm-command-prefix
  (kbd "C-x b")   'helm-buffers-list
  (kbd "C-x M-b") 'helm-mini
  (kbd "C-c b")   'helm-switchb-shell-list
  (kbd "C-c M-b") 'helm-mt
  (kbd "C-c d")   'helm-switchb-dired-list
  (kbd "C-x f")   'helm-find-files
  (kbd "C-x M-f") 'helm-projectile-find-file
  (kbd "C-c f")   'helm-recentf
  (kbd "C-c M-f") 'helm-sudo-find-file
  (kbd "C-M-z")   'helm-resume
  (kbd "M-x")     'helm-M-x

  (kbd "C-`")     'helm-imenu
  (kbd "C-~")     'helm-semantic

  (kbd "C-'")     'next-multiframe-window
  (kbd "C-\"")    'previous-multiframe-window

  (kbd "M-.")     'helm-etags-plus-select
  (kbd "M-*")     'helm-etags-plus-history
  (kbd "C-=")     'helm-etags-plus-history-go-back
  (kbd "C-+")     'helm-etags-plus-history-go-forward

  (kbd "C-c M-i") 'helm-multi-swoop
  (kbd "C-x M-i") 'helm-multi-swoop-all
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-M-1")   'zz:urxvt
  (kbd "C-M-2")   'zz:chrome
  (kbd "C-M-3")   'zz:firefox
  (kbd "C-M-4")   'zz:meld
  (kbd "C-M-5")   'zz:rofi-drun
  (kbd "C-M-6")   'zz:rofi-run
  (kbd "C-M-7")   'zz:rofi-ssh
  (kbd "C-M-8")   'zz:rofi-win
  (kbd "C-M-9")   'zz:tmux
  (kbd "C-M-0")   'zz:trans
  (kbd "C-M--")   'zz:baidu
  (kbd "C-M-=")   'zz:google
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
 helm-map
 (list
  (kbd "C-p")     'helm-previous-line
  (kbd "C-n")     'helm-next-line
  (kbd "C-M-n")   'helm-next-source
  (kbd "C-M-p")   'helm-previous-source
  ))

(apply-keys-to-map
 f4-g-map
 (list
  (kbd "p")       'practice-words
  (kbd "C-p")     'typing-of-emacs
  (kbd "s")       'snake
  (kbd "C-s")     'sokoban
  (kbd "C-t")     'tetris
  (kbd "M-t")     'autotetris
  (kbd "1")       'typit-basic-test
  (kbd "2")       'typit-advanced-test
  ))


(provide 'key-setting)

;;; key-setting.el ends here
