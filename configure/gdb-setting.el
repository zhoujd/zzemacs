;;;; gdb-setting.el --- gdb common file

;;GDB-MI: https://www.emacswiki.org/emacs/GDB-MI
(require 'gdb-mi)
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

;; GDB-MI non-stop
(defun gdb-non-stop-handler ()
  (goto-char (point-min))
  (print (buffer-substring-no-properties (point-min) (point-max)))
  (if (re-search-forward "No symbol" nil t)
      (progn
        (message
         "This version of GDB doesn't support non-stop mode.  Turning it off.")
        (setq gdb-non-stop nil)
        (setq gdb-supports-non-stop nil))
      (progn
        (setq gdb-supports-non-stop t)
        (gdb-input "-gdb-set mi-async on" 'ignore)
        (gdb-input "-list-target-features" 'gdb-check-mi-async))))

;; Force gdb-mi to not dedicate any windows
(advice-add 'gdb-display-buffer
            :around (lambda (orig-fun &rest r)
                      (let ((window (apply orig-fun r)))
                        (set-window-dedicated-p window nil)
                        window)))

(advice-add 'gdb-set-window-buffer
            :around (lambda (orig-fun name &optional ignore-dedicated window)
                      (funcall orig-fun name ignore-dedicated window)
                      (set-window-dedicated-p window nil)))

(provide 'gdb-setting)

;;; gdb-setting.el ends here
