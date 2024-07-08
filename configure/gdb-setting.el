;;;; gdb-setting.el --- gdb common file
;;GDB: emacs --eval '(gdb "gdb -q -i=mi a.out")'
;;GUD-GDB: emacs --eval '(gud-gdb "gdb -q --fullname a.out")'

;;GDB-MI: https://www.emacswiki.org/emacs/GDB-MI
(require 'gdb-mi)

(defun zz:gud-break-remove ()
  "Set/clear breakpoin."
  (interactive)
  (save-excursion
   (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)
       (gud-remove nil)
       (gud-break nil))))

(defun zz:gud-restore ()
  "Retore and refresh"
  (interactive)
  (gdb-restore-windows)
  (gud-refresh))

;;GUD quit
(defun zz:gud-quit ()
  (interactive)
  (gud-basic-call "quit"))

(add-hook 'gud-mode-hook
          (lambda ()
            (local-set-key (kbd "C-q") 'zz:gud-quit)))

(add-hook 'gdb-mode-hook
          (lambda ()
            (define-key gud-mode-map [tab] 'company-complete-selection)))

;;Enable realgud
(when-emacs25
 (zz:load-path "site-lisp/realgud")
 (require 'realgud)
 (zz:load-path "site-lisp/realgud-lldb")
 (require 'realgud-lldb))

;;GDB-MI non-stop
(defun gdb-non-stop-handler ()
  (goto-char (point-min))
  (print (buffer-substring-no-properties (point-min) (point-max)))
  (if (re-search-forward "No symbol" nil t)
      (progn
        (message "This version of GDB doesn't support non-stop mode.  Turning it off.")
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


;; For the consistency of gdb-select-window's calling convention...
(defun gdb-set-header (header)
  (cond ((eq header 'locals) gdb-locals-header)
        ((eq header 'registers) gdb-registers-header)
        ((eq header 'breakpoints) gdb-breakpoints-header)
        ((eq header 'threads) gdb-threads-header)
        (t nil)))

(defun gdb-comint-buffer-name ()
  (buffer-name gud-comint-buffer))
(defun gdb-source-buffer-name ()
  (buffer-name (window-buffer gdb-source-window)))

(defun gdb-select-window (header)
  "Switch directly to the specified GDB window.
Moves the cursor to the requested window, switching between
`gdb-many-windows' \"tabs\" if necessary in order to get there.

Recognized window header names are: 'comint, 'locals, 'registers,
'stack, 'breakpoints, 'threads, and 'source."

  (interactive "Sheader: ")

  (let* ((header-alternate (case header
                             ('locals      'registers)
                             ('registers   'locals)
                             ('breakpoints 'threads)
                             ('threads     'breakpoints)))
         (buffer (intern (concat "gdb-" (symbol-name header) "-buffer")))
         (buffer-names (mapcar (lambda (header)
                                 (funcall (intern (concat "gdb-"
                                                          (symbol-name header)
                                                          "-buffer-name"))))
                               (if (null header-alternate)
                                   (list header)
                                 (list header header-alternate))))
         (window (if (eql header 'source)
                     gdb-source-window
                   (or (get-buffer-window (car buffer-names))
                       (when (not (null (cadr buffer-names)))
                         (get-buffer-window (cadr buffer-names)))))))

    (when (not (null window))
      (let ((was-dedicated (window-dedicated-p window)))
        (select-window window)
        (set-window-dedicated-p window nil)
        (when (member header '(locals registers breakpoints threads))
          (switch-to-buffer (gdb-get-buffer-create buffer))
          (setq header-line-format (gdb-set-header buffer)))
        (set-window-dedicated-p window was-dedicated))
      t)))

;; Use global keybindings for the window selection functions so that they
;; work from the source window too...
(mapcar (lambda (setting)
          (lexical-let ((key    (car setting))
                        (header (cdr setting)))
            (global-set-key (concat "\C-cg" key) #'(lambda ()
                                                        (interactive)
                                                        (gdb-select-window header)))))
        '(("c" . comint)
          ("l" . locals)
          ("r" . registers)
          ("u" . source)
          ("s" . stack)
          ("b" . breakpoints)
          ("t" . threads)))


(provide 'gdb-setting)

;;; gdb-setting.el ends here
