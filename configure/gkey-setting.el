;;;; gkey-setting.el --- gdb key function config file
;;;

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
  (kbd "M-ESC h")   'event-apply-hyper-modifier
  (kbd "M-ESC a")   'event-apply-alt-modifier
  (kbd "M-ESC s")   'event-apply-super-modifier
  (kbd "M-ESC c")   'event-apply-control-modifier
  (kbd "M-ESC m")   'event-apply-meta-modifier
  (kbd "M-ESC S")   'event-apply-shift-modifier
  ))

(define-fn-key (gethash "f8" fn-key-table)
  [f8]              'gud-gdb
  [S-f8]            'gdb
  [C-f8]            'gdb-restore-windows
  [M-f8]            'gud-refresh
  (kbd "C-x <f8>")  'gdb-many-windows
  (kbd "C-c <f8>")  'gud-tooltip-mode
  "f8 key binding")

(define-fn-key (gethash "f11" fn-key-table)
  [f11]             'linum-mode
  [S-f11]           'hl-line-mode
  [C-f11]           'blank-mode
  [M-f11]           'fci-mode
  (kbd "C-x <f11>") 'menu-bar-mode
  (kbd "C-c <f11>") 'tool-bar-mode
  "f11 key binding")

(apply-keys-to-map
 global-map
 (list
  (kbd "C-x C-b")   'ibuffer
  ))

(apply-keys-to-map
 f4-map
 (list
  (kbd "1")         (zz:quick-shell zz:shell-f1  "*shell<f1>*")
  (kbd "2")         (zz:quick-shell zz:shell-f2  "*shell<f2>*")
  (kbd "3")         (zz:quick-shell zz:shell-f3  "*shell<f3>*")
  (kbd "4")         (zz:quick-shell zz:shell-f4  "*shell<f4>*")
  (kbd "5")         (zz:quick-shell zz:shell-f5  "*shell<f5>*")
  (kbd "6")         (zz:quick-shell zz:shell-f6  "*shell<f6>*")
  (kbd "7")         (zz:quick-shell zz:shell-f7  "*shell<f7>*")
  (kbd "8")         (zz:quick-shell zz:shell-f8  "*shell<f8>*")
  (kbd "9")         (zz:quick-shell zz:shell-f9  "*shell<f9>*")
  (kbd "0")         'zz:cd-shell
  (kbd "-")         'zz:local-shell
  (kbd "=")         'zz:remote-shell
  (kbd "\\")        'zz:get-shell
  (kbd "DEL")       'zz:get-shell
  (kbd "`")         'zz:home-shell
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "C-1")       'delete-window
  (kbd "C-2")       'delete-frame
  (kbd "C-3")       'zz:last-frame-go
  (kbd "C-4")       'zz:undo-kill-buffer
  (kbd "C-5")       'gud-jump
  (kbd "C-6")       'gud-remove
  (kbd "C-7")       'gud-until
  (kbd "C-8")       'gud-finish
  (kbd "C-9")       'gud-pstar
  (kbd "C-0")       'gud-refresh

  (kbd "C--")       'redo
  (kbd "C-_")       'undo

  (kbd "C-,")       'winner-undo
  (kbd "C-.")       'winner-redo
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "M-1")       'delete-other-windows
  (kbd "M-2")       'delete-other-frames
  (kbd "M-3")       'zz:last-buffer-go
  (kbd "M-4")       'zz:kill-this-buffer
  (kbd "M-5")       'gud-cont
  (kbd "M-6")       'gud-break
  (kbd "M-7")       'gud-next
  (kbd "M-8")       'gud-step
  (kbd "M-9")       'gud-print
  (kbd "M-0")       'gdb-restore-windows

  (kbd "M-#")       'query-replace-regexp
  ))

(apply-keys-to-map
 esc-map
 (list
  (kbd "M-,")       'gud-up
  (kbd "M-.")       'gud-down
  ))


(provide 'gkey-setting)

;;; gkey-setting.el ends here
