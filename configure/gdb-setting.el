;;;; gdb-setting.el --- gdb common file

;;GDB-MI: https://www.emacswiki.org/emacs/GDB-MI
(setq gud-gdb-command-name "gdb -q --fullname")

(defun zz:gud-break-remove ()
  "Set/clear breakpoin."
  (interactive)
  (save-excursion
   (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)
       (gud-remove nil)
       (gud-break nil))))

(defun zz:gud-kill ()
  "Kill gdb process."
  (interactive)
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  (kill-process (get-buffer-process gud-comint-buffer)))

(defun zz:gud-restore ()
  "Retore and refresh"
  (interactive)
  (gdb-restore-windows)
  (gud-refresh))

;;realgud
(when-emacs25
 (zz:load-path "site-lisp/realgud")
 (require 'realgud)
 (zz:load-path "site-lisp/realgud-lldb")
 (require 'realgud-lldb))

(add-hook
 'gdb-mode-hook
 (lambda ()
   (define-key gud-mode-map [tab] 'company-complete-selection)))


(provide 'gdb-setting)

;;; gdb-setting.el ends here
