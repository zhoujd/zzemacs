;;;; macro-setting.el --- macro config file
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

(defmacro if-emacs25 (if-cause &optional else-cause)
  `(if ,(and (>= emacs-major-version 25)
             (>= emacs-minor-version 1))
       ,if-cause ,else-cause))

(defmacro if-not-emacs25 (if-cause &optional else-cause)
  `(if-emacs25 ,else-cause ,if-cause))

(defmacro when-emacs25 (&rest body)
  `(if-emacs25 (progn ,@body)))

(defmacro unless-emacs25 (&rest body)
  `(if-not-emacs25 (progn ,@body)))

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

;;marcro for start-process
(defmacro execute-set-key (name args)
  `(lambda ()
     (interactive)
     (apply 'start-process ,name nil ,args)))

;;start shell in environment setting
(defmacro with-start-shell (&rest body)
  `(let ((env-shell      shell-file-name)
         (env-shell-cmd  shell-command-switch)
         (env-shell-file explicit-shell-file-name)
         (env-shell      (getenv "SHELL")))
     ,@body
     (setq shell-file-name env-shell)
     (setq shell-command-switch env-shell-cmd)
     (setq explicit-shell-file-name env-shell-file)
     (setenv "SHELL" env-shell)
     ))

;;(defkeys-map global-map
;;  ((kbd "M-1") "hello")
;;  ((kbd "M-2") "zhoujd"))
(defmacro defkeys-map (map &rest keys)
  (let ((ks (mapcar
             (lambda (k)
               (cons 'define-key (cons map k)))
             keys))) 
    `(progn ,@ks)))

;;run command macro
(defmacro run-command-sort (cmd)
  `(cond ((fboundp 'async-shell-command) (async-shell-command ,cmd))
         ((fboundp 'shell-command) (shell-command ,cmd))
         (t (message "no shell command function can be run"))))

;;start shell
(defmacro start-quick-shell (name)
  `(lambda () (interactive) (start-shell ,name)))

;;switch term
(defmacro start-quick-term (name)
  `(lambda ()
     (interactive)
     (if (get-buffer ,name)
         (switch-to-buffer ,name)
         (message "%s is not exist" ,name))))

;;start slime
(defmacro defslime-start (name lisp)
 `(defun ,name ()
     (interactive)
     (slime ,lisp)))

(provide 'macro-setting)

;;; macro-setting.el ends here
