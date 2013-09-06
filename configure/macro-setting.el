;;;; common-setting.el --- common config file
;;

(defmacro if-ms-windows (if-cause &optional else-cause)
  `(if ,(or (eq window-system 'w32) (eq window-system 'win32))
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

(provide 'macro-setting)

;;; common-setting.el ends here
