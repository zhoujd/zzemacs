;;; gdb select window
;;For the consistency of gdb-select-window's calling convention...

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


(provide 'gdb-window)

;;; gdb-window.el ends here
