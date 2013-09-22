;;;; common-setting.el --- common config file
;;

(defmacro if-ms-windows (if-cause &optional else-cause)
  `(if ,(eq system-type 'windows-nt)
       ,if-cause ,else-cause))

(defmacro if-not-ms-windows (if-cause &optional else-cause)
  `(if-ms-windows ,else-cause ,if-cause))

(defmacro unless-ms-windows (&rest body)
  `(if-not-ms-windows (progn ,@body)))

(defmacro when-ms-windows (&rest body)
  `(if-ms-windows (progn ,@body)))

(defmacro with-utf-8-env (&rest body)
  `(let ((curr-lang current-language-environment))
     (set-language-environment 'utf-8)
     ,@body
     (set-language-environment curr-lang)))

(defmacro with-chinese-env (&rest body)
  `(let ((curr-lang current-language-environment))
     (set-language-environment 'Chinese-GB18030)
     ,@body
     (set-language-environment curr-lang)))

(defmacro if-emacs24-3 (if-cause &optional else-cause)
  `(if ,(and (>= emacs-major-version 24)
             (>= emacs-minor-version 3))
       ,if-cause ,else-cause))

(defmacro when-emacs24-3 (&rest body)
  `(when ,(and (>= emacs-major-version 24)
               (>= emacs-minor-version 3))
     ,@body))

;;marcro for start-process
(defmacro execute-set-key (key-map key name args)
  `(define-key ,key-map (kbd ,key)
     (lambda ()
       (interactive)
       (apply 'start-process ,name nil ,args))))


(provide 'macro-setting)

;;; common-setting.el ends here
