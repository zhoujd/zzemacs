;;;; macro-setting.el --- macro config file
;;

(defmacro if-ms-windows (then-cause &optional else-cause)
  `(if ,(eq system-type 'windows-nt)
       ,then-cause ,else-cause))

(defmacro if-not-ms-windows (then-cause &optional else-cause)
  `(if-ms-windows ,else-cause ,then-cause))

(defmacro unless-ms-windows (&rest body)
  `(if-not-ms-windows (progn ,@body)))

(defmacro when-ms-windows (&rest body)
  `(if-ms-windows (progn ,@body)))

(defmacro if-emacs25 (then-cause &optional else-cause)
  `(if ,(and (>= emacs-major-version 25)
             (>= emacs-minor-version 1))
       ,then-cause ,else-cause))

(defmacro if-not-emacs25 (then-cause &optional else-cause)
  `(if-emacs25 ,else-cause ,then-cause))

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

(defmacro with-ido-read (&rest body)
  `(let ((completing-read-function 'ido-completing-read))
     (ido-mode t)
     ,@body))

(defmacro with-helm-read (&rest body)
  `(let ((completing-read-function 'helm-comp-read))
     ,@body))

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
(defmacro zz/indent (operator indentation)
  `(put ',operator 'lisp-indent-function ,indentation))

;;marcro for start-process
(defmacro zz/exec-key (fn-name args)
  `(defun ,fn-name ()
     (interactive)
     (apply 'start-process "execute-key" nil ,args)))

;;run command macro
(defmacro zz/run-command (cmd)
  `(cond ((fboundp 'async-shell-command) (async-shell-command ,cmd))
         ((fboundp 'shell-command) (shell-command ,cmd))
         (t (message "no shell command function can be run"))))

;;start shell
(defmacro zz/quick-shell (fn-name buf-name)
  `(defun ,fn-name ()
     (interactive)
     (zz/start-shell ,buf-name)))

;;start terminator
(defmacro zz/quick-termintor (fn-name type)
  `(defun ,fn-name ()
     (interactive)
     (terminator-open-template ,type)))

;;switch term
(defmacro zz/quick-buffer (fn-name buf-name)
  `(defun ,fn-name ()
     (interactive)
     (if (get-buffer ,buf-name)
         (switch-to-buffer ,buf-name)
         (message "%s is not exist" ,buf-name))))

;;start slime
(defmacro zz/slime-start (fn-name lisp)
 `(defun ,fn-name ()
     (interactive)
     (slime ,lisp)))

;;dired sort
(defmacro zz/dired-sort (fn-name switch)
  `(defun ,fn-name ()
     (interactive)
     (dired-sort-other (concat dired-listing-switches ,switch))))

;;gud command
(defmacro zz/gud-cmd (fn-name cmd)
  `(defun ,fn-name ()
     (interactive)
     (if (or
          (eq gud-minor-mode 'gdbmi)
          (eq gud-minor-mode 'gdb))
         (call-interactively ,cmd)
         (message "Run gdb then %s" ,cmd))))


(provide 'macro-setting)

;;; macro-setting.el ends here
