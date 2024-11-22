;;;; gdb-setting.el --- gdb common file
;;GDB: emacs --eval '(gdb "gdb -q -i=mi a.out")'
;;GUD-GDB: emacs --eval '(gud-gdb "gdb -q --fullname a.out")'

;;GDB-MI: https://www.emacswiki.org/emacs/GDB-MI
(require 'gdb-mi)

;;gud-gdb
(setq gud-gud-gdb-command-name "gdb -q --fullname")
;;gdb-mi
(setq gud-gdb-command-name "gdb -q -i=mi")

(defun zz/gud-break-remove ()
  "Set/clear breakpoin."
  (interactive)
  (save-excursion
    (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)
        (gud-remove nil)
        (gud-break nil))))

(defun zz/gud-restore ()
  "Retore and refresh"
  (interactive)
  (gdb-restore-windows)
  (gud-refresh))

;;GUD quit
(defun zz/gud-quit ()
  (interactive)
  (gud-basic-call "quit"))

(defun zz/gud-hook ()
  (local-set-key (kbd "C-q") 'zz/gud-quit))
(add-hook 'gud-mode-hook 'zz/gud-hook)

(defun zz/gdb-hook ()
  (define-key gud-mode-map [tab] 'company-complete-selection))
(add-hook 'gdb-mode-hook 'zz/gdb-hook)

;;RealGUD: https://github.com/realgud/realgud
(defun zz/load-realgud ()
  (interactive)
  (zz/load-path "site-lisp/realgud")
  (require 'realgud)
  (zz/load-path "site-lisp/realgud-lldb")
  (require 'realgud-lldb))

;;GDB-MI non-stop
(defun gdb-non-stop-handler ()
  (goto-char (point-min))
  (print (buffer-substring-no-properties (point-min) (point-max)))
  (if (re-search-forward "No symbol" nil t)
      (progn
        (message "This version of GDB doesn't support non-stop mode. Turning it off.")
        (setq gdb-non-stop nil)
        (setq gdb-supports-non-stop nil))
      (progn
        (setq gdb-supports-non-stop t)
        (gdb-input "-gdb-set mi-async on" 'ignore)
        (message "GDB on non-stop mode")
        )))

;;Ensure that all source files are opened in the same window when gdb is running
(add-to-list 'display-buffer-alist
             (cons 'gdb-source-code-buffer-p
                   (cons 'display-buffer-use-some-window nil)))

(defun gdb-source-code-buffer-p (bufName action)
  "Return whether BUFNAME is a source code buffer and gdb is running."
  (let ((buf (get-buffer bufName)))
    (and buf
         (or
          (eq gud-minor-mode 'gdbmi)
          (eq gud-minor-mode 'gdb))
         (with-current-buffer buf
           (derived-mode-p buf 'c++-mode 'c-mode)))))


(provide 'gdb-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; gdb-setting.el ends here
