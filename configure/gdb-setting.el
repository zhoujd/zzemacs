;;;; gdb-setting.el --- gdb common file

;;GDB-MI: https://www.emacswiki.org/emacs/GDB-MI

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

(add-hook
 'gdb-mode-hook
 '(lambda ()
    (gud-def gud-break-main "break main" nil "Set breakpoint at main.")))


(provide 'gdb-setting)

;;; gdb-setting.el ends here
