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
  (kbd "M-] h")   'event-apply-hyper-modifier
  (kbd "M-] a")   'event-apply-alt-modifier
  (kbd "M-] s")   'event-apply-super-modifier
  (kbd "M-] c")   'event-apply-control-modifier
  (kbd "M-] m")   'event-apply-meta-modifier
  (kbd "M-] S")   'event-apply-shift-modifier
  ))

;;all of these sequences are translated from term/xterm.el .
;;if emacs can't create keymap correctly from you TERM
;;you can force to load it
(unless-ms-windows
 (if (and (not (display-graphic-p))
          (load-library "term/xterm"))
     (terminal-init-xterm)))

(apply-keys-to-map
 mode-specific-map
 (list
  (kbd "1")       (zz:quick-buffer zz:shell-1  "*shell<1>*")
  (kbd "2")       (zz:quick-buffer zz:shell-2  "*shell<2>*")
  (kbd "3")       (zz:quick-buffer zz:shell-3  "*shell<3>*")
  (kbd "4")       (zz:quick-buffer zz:shell-4  "*shell<4>*")
  (kbd "5")       (zz:quick-buffer zz:shell-5  "*shell<5>*")
  (kbd "6")       (zz:quick-buffer zz:shell-6  "*shell<6>*")
  (kbd "7")       (zz:quick-buffer zz:shell-7  "*shell<7>*")
  (kbd "8")       (zz:quick-buffer zz:shell-8  "*shell<8>*")
  (kbd "9")       (zz:quick-buffer zz:shell-9  "*shell<9>*")
  (kbd "0")       (zz:quick-buffer zz:shell-10 "*shell<10>*")
  (kbd "-")       'zz:local-shell
  (kbd "=")       'zz:remote-shell
  (kbd "\\")      (lookup-key global-map [S-f9])
  (kbd "`")       'zz:switch-to-shell

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
  (kbd "M--")     'eshell
  (kbd "M-=")     'zz:popup-term
  (kbd "M-\\")    'multi-term
  (kbd "M-`")     'zz:switch-to-term
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

;;helm key setting
(apply-keys-to-map
 global-map
 (list
  (kbd "C-x b")   'helm-buffers-list
  (kbd "C-x f")   'helm-find-files
  (kbd "C-x M-f") 'helm-projectile-find-file
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

(apply-keys-to-map
 global-map
 (list
  (kbd "C-M-1")   (lookup-key global-map [C-f1])
  (kbd "C-M-2")   (lookup-key global-map [C-f2])
  (kbd "C-M-3")   (lookup-key global-map [C-f3])
  (kbd "C-M-4")   (lookup-key global-map [C-f4])
  (kbd "C-M-5")   (lookup-key global-map [C-f5])
  (kbd "C-M-6")   (lookup-key global-map [C-f6])
  (kbd "C-M-7")   (lookup-key global-map [C-f7])
  (kbd "C-M-8")   (lookup-key global-map [C-f8])
  (kbd "C-M-9")   (lookup-key global-map [C-f9])
  (kbd "C-M-0")   (lookup-key global-map [C-f10])
  (kbd "C-M--")   (lookup-key global-map [C-f11])
  (kbd "C-M-=")   (lookup-key global-map [C-f12])
  ))


(provide 'key-setting)

;;; key-setting.el ends here
