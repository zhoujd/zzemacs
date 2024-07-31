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
 function-key-map
 (list
  [(control shift iso-lefttab)] [(control shift tab)]
  [(meta shift iso-lefttab)] [(meta shift tab)]
  [(meta control shift iso-lefttab)] [(meta control shift tab)]
  ))

(apply-keys-to-map
 key-translation-map
 (list
  (kbd "M-ESC h") 'event-apply-hyper-modifier
  (kbd "M-ESC a") 'event-apply-alt-modifier
  (kbd "M-ESC s") 'event-apply-super-modifier
  (kbd "M-ESC c") 'event-apply-control-modifier
  (kbd "M-ESC m") 'event-apply-meta-modifier
  (kbd "M-ESC S") 'event-apply-shift-modifier
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
  (kbd "a")       'zz:display-current-time
  (kbd "C-a")     'helm-world-time
  (kbd "M-a")     'world-time-list
  (kbd "b")       'helm-bm
  (kbd "d")       'zz:nnn
  (kbd "C-d")     'zz:helm-nnn
  (kbd "M-d")     'docker
  (kbd "n")       'neotree-toggle
  (kbd "o")       'zz:last-buffer-go
  (kbd "C-p")     'proced
  (kbd "r")       'zz:revert-buffer
  (kbd "C-r")     'recentf-open-files
  (kbd "C-s")     'slime-selector
  (kbd "t")       'trashed
  (kbd "C-t")     'translate-shell
  (kbd "M-t")     'translate-shell-brief
  (kbd "v")       'zz:evil-toggle
  (kbd "w")       'easy-kill
  (kbd "C-z")     'repeat
  (kbd ",")       'winner-undo
  (kbd ".")       'winner-redo
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
  (kbd "0")       'zz:cd-shell
  (kbd "-")       'zz:local-shell
  (kbd "=")       'zz:remote-shell
  (kbd "\\")      'zz:get-shell
  (kbd "DEL")     'zz:get-shell
  (kbd "`")       'zz:home-shell

  (kbd "C-1")     (zz:quick-shell zz:shell-1  "*shell<1>*")
  (kbd "C-2")     (zz:quick-shell zz:shell-2  "*shell<2>*")
  (kbd "C-3")     (zz:quick-shell zz:shell-3  "*shell<3>*")
  (kbd "C-4")     (zz:quick-shell zz:shell-4  "*shell<4>*")
  (kbd "C-5")     (zz:quick-shell zz:shell-5  "*shell<5>*")
  (kbd "C-6")     (zz:quick-shell zz:shell-6  "*shell<6>*")
  (kbd "C-7")     (zz:quick-shell zz:shell-7  "*shell<7>*")
  (kbd "C-8")     (zz:quick-shell zz:shell-8  "*shell<8>*")
  (kbd "C-9")     (zz:quick-shell zz:shell-9  "*shell<9>*")
  (kbd "C-0")     'zz:helm-cd-shell
  (kbd "C--")     'zz:helm-local-shell
  (kbd "C-=")     'zz:helm-remote-shell
  (kbd "C-\\")    'zz:get-shell
  [C-backspace]   'zz:get-shell
  (kbd "C-`")     'zz:home-shell

  (kbd "M-1")     (zz:quick-buffer zz:term-1  "*terminal<1>*")
  (kbd "M-2")     (zz:quick-buffer zz:term-2  "*terminal<2>*")
  (kbd "M-3")     (zz:quick-buffer zz:term-3  "*terminal<3>*")
  (kbd "M-4")     (zz:quick-buffer zz:term-4  "*terminal<4>*")
  (kbd "M-5")     (zz:quick-buffer zz:term-5  "*terminal<5>*")
  (kbd "M-6")     (zz:quick-buffer zz:term-6  "*terminal<6>*")
  (kbd "M-7")     (zz:quick-buffer zz:term-7  "*terminal<7>*")
  (kbd "M-8")     (zz:quick-buffer zz:term-8  "*terminal<8>*")
  (kbd "M-9")     (zz:quick-buffer zz:term-9  "*terminal<9>*")
  (kbd "M-0")     'zz:helm-cd-term
  (kbd "M--")     'zz:helm-local-term
  (kbd "M-=")     'tramp-term
  (kbd "M-\\")    'multi-term
  (kbd "M-DEL")   'multi-term
  (kbd "M-`")     'zz:home-term
  ))

;;switch to shells
(apply-keys-to-map
 f4-map
 (list
  [f4]            'kill-this-buffer
  (kbd "SPC")     'helm-ispell
  (kbd "/")       'bongo

  (kbd "b")       'helm-switchb-shell-list
  (kbd "M-b")     'helm-mt
  (kbd "d")       'helm-switchb-dired-list
  (kbd "t")       'helm-tramp
  (kbd "z")       'helm-stumpwm-commands
  (kbd "v")       'multi-vterm
  (kbd "w")       'langtool-check
  (kbd "W")       'langtool-check-done
  (kbd "l")       'langtool-switch-default-language
  (kbd "s")       'langtool-show-message-at-point
  (kbd "c")       'langtool-correct-buffer
  (kbd "k")       'keep-lines
  (kbd "f")       'flush-lines
  (kbd "r")       'vr/replace
  (kbd "q")       'vr/query-replace

  (kbd "C-b")     'browse-url
  (kbd "C-d")     (if-ms-windows
                   (zz:exec-key zz:explorer '("explorer" "."))
                   (zz:exec-key zz:thunar '("thunar")))
  (kbd "C-e")     'eshell
  (kbd "C-h")     'sourcepair-jump-to-headerfile
  (kbd "C-l")     'command-history
  (kbd "C-k")     'vkill
  (kbd "C-r")     'zz:add-code-review-note
  (kbd "C-s")     'zz:slime-connect-stumpwm
  (kbd "C-t")     'zz:open-with-terminal
  (kbd "M-t")     'zz:transparency-toggle
  (kbd "C-u")     'disk-usage
  (kbd "C-v")     'virt-manager

  (kbd "M-d")     (zz:exec-key zz:rofi-drun   '("rofi" "-show" "drun"))
  (kbd "M-r")     (zz:exec-key zz:rofi-run    '("rofi" "-show" "run"))
  (kbd "M-s")     (zz:exec-key zz:rofi-ssh    '("rofi" "-show" "ssh"))
  (kbd "M-w")     (zz:exec-key zz:rofi-win    '("rofi" "-show" "window"))
  ))

;;execute start-process key
(apply-keys-to-map
 f4-e-map
 (list
  (kbd "\\")      'zz:baidu
  (kbd "`")       'zz:google

  (kbd "c")       (zz:exec-key zz:urxvt       '("urxvt"))
  (kbd "f")       (zz:exec-key zz:firefox     '("firefox"))
  (kbd "g")       (zz:exec-key zz:chrome      '("google-chrome"))
  (kbd "m")       (zz:exec-key zz:evolution   '("evolution"))

  (kbd "h")       (zz:exec-key zz:thunar      '("thunar"))
  (kbd "v")       (zz:exec-key zz:vim         '("urxvt" "-e" "vim"))
  (kbd "t")       (zz:exec-key zz:tmux        '("urxvt" "-e" "tmux"))

  (kbd "d")       (zz:exec-key zz:rofi-drun   '("rofi" "-show" "drun"))
  (kbd "r")       (zz:exec-key zz:rofi-run    '("rofi" "-show" "run"))
  (kbd "s")       (zz:exec-key zz:rofi-ssh    '("rofi" "-show" "ssh"))
  (kbd "w")       (zz:exec-key zz:rofi-win    '("rofi" "-show" "window"))

  (kbd "M-g")     (zz:exec-key zz:gitg        '("gitg"))
  (kbd "M-m")     (zz:exec-key zz:meld        '("meld"))
  (kbd "M-t")     (zz:exec-key zz:trans       '("urxvt" "-e" "trans" "-I"))
  ))

(apply-keys-to-map
 f4-p-map
 (list
  (kbd "1")       'zz:python-scratch
  (kbd "2")       'zz:sh-scratch
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-1")     'delete-window
  (kbd "C-2")     'delete-frame
  (kbd "C-3")     'zz:last-frame-go
  (kbd "C-4")     'zz:undo-kill-buffer
  (kbd "C-5")     'gud-jump
  (kbd "C-6")     'gud-remove
  (kbd "C-7")     'gud-until
  (kbd "C-8")     'gud-finish
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
  (kbd "M-5")     'gud-cont
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
  (kbd "C-x C-r") 'zz:sudo-find-file
  (kbd "C-x d")   'zz:dired
  (kbd "C-x g")   'magit-status
  (kbd "C-x M-g") 'magit-dispatch
  (kbd "C-c w")   'compare-windows
  (kbd "C-c ;")   'iedit-mode
  (kbd "C-c M-;") 'iedit-dwim
  (kbd "C-c M-h") 'discover-my-major
  (kbd "%")       'zz:match-paren
  (kbd "M-<tab>") 'buffer-flip

  (kbd "C-h")     'delete-backward-char
  [backspace]     'delete-backward-char
  (kbd "C-d")     'delete-forward-char
  [delete]        'delete-forward-char

  ;;org-mode
  (kbd "C-c a")   'org-agenda
  (kbd "C-c c")   'org-capture

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

  ;;M-x
  (kbd "M-x")     'helm-M-x
  (kbd "C-x M-x") 'amx
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
  (kbd "C-c g")   'helm-ls-git
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
 esc-map
 (list
  (kbd "M-,")     'gud-up
  (kbd "M-.")     'gud-down
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
  (kbd "g")       'gomoku
  (kbd "p")       'practice-words
  (kbd "C-p")     'typing-of-emacs
  (kbd "s")       'snake
  (kbd "C-s")     'sokoban
  (kbd "C-t")     'tetris
  (kbd "M-t")     'autotetris
  (kbd "1")       'typit-basic-test
  (kbd "2")       'typit-advanced-test
  ))

(apply-keys-to-map
 search-map
 (list
  (kbd "o")       'helm-occur
  (kbd "i")       'helm-swoop
  ))

(apply-keys-to-map
 isearch-mode-map
 (list
  (kbd "M-i")     'helm-swoop-from-isearch
  (kbd "M-o")     'helm-occur-from-isearch
 ))


(provide 'key-setting)

;;; key-setting.el ends here
