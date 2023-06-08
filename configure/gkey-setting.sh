;;;; gkey-setting.el --- gdb key function config file
;;;


(zz:load-path "elisp")
(require 'apply-keys)

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


(provide 'gkey-setting)

;;; gkey-setting.el ends here
