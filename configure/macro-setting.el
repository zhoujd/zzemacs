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

;;(defkeys-map global-map
;;  ((kbd "M-1") "hello")
;;  ((kbd "M-2") "zhoujd"))
(defmacro defkeys-map (map &rest keys)
  (let ((ks (mapcar
             (lambda (k)
               (cons 'define-key (cons map k)))
             keys))) 
    `(progn ,@ks)))

;;lisp indent
(defmacro defindent (operator indentation)
  `(put ',operator 'lisp-indent-function ,indentation))

;;marcro for start-process
(defmacro zz:execute-key (fn-name args)
  `(defun ,fn-name ()
     (interactive)
     (apply 'start-process "execute-key" nil ,args)))

;;run command macro
(defmacro zz:run-command (cmd)
  `(cond ((fboundp 'async-shell-command) (async-shell-command ,cmd))
         ((fboundp 'shell-command) (shell-command ,cmd))
         (t (message "no shell command function can be run"))))

;;start shell
(defmacro zz:quick-shell (fn-name buf-name)
  `(defun ,fn-name ()
     (interactive)
     (my:start-shell ,buf-name)))

;;start terminator
(defmacro zz:quick-termintor (fn-name type)
  `(defun ,fn-name ()
     (interactive)
     (terminator-open-template ,type)))

;;switch term
(defmacro zz:quick-buffer (fn-name buf-name)
  `(defun ,fn-name ()
     (interactive)
     (if (get-buffer ,buf-name)
         (switch-to-buffer ,buf-name)
         (message "%s is not exist" ,buf-name))))

;;start slime
(defmacro zz:slime-start (fn-name lisp)
 `(defun ,fn-name ()
     (interactive)
     (slime ,lisp)))

;;dired sort
(defmacro zz:dired-sort (fn-name switch)
  `(defun ,fn-name ()
     (interactive)
     (dired-sort-other (concat dired-listing-switches ,switch))))


(provide 'macro-setting)

;;; macro-setting.el ends here
