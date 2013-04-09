#!/usr/bin/emacs --script
;;;;this script for init emacs configure
;;$cd <setup.el>
;;$emacs --script setup.el

(message "hello, zzemacs")

;;check os type
(if (eq system-type 'windows-nt)
  (message "emacs run on windows")
  (message "emacs run on un*x"))

(if (eq system-type 'windows-nt)
    (progn
      (defvar zz-winnt-home-path "c:/develop/znix/home/zhoujd")
      (defvar zz-home-path (getenv "APPDATA"))
      (defvar zz-dotemacs-content (list
                                   (format ";;;this is .emacs for zhoujd, generate by install.el.")
                                   (format "(setenv \"HOME\" \"%s\")" zz-winnt-home-path)
                                   (format "(setq default-directory \"~\")")
                                   (format "(load-file \"~/zzemacs/.emacs\")")
                                   ))
      )
    (progn
      (defvar zz-emacs-root  default-directory)
      (defvar zz-home-path (getenv "HOME"))
      (defvar zz-dotemacs-content (list
                                   (format ";;;this is .emacs for zhoujd, generate by install.el.")
                                   (format "(defvar zzemacs-path \"%s\")" zz-emacs-root)
                                   (format "%s" "(load-file (concat zzemacs-path \".emacs\"))")
                                   ))
      ))

(defvar zz-dotemacs-path (concat zz-home-path "/.emacs"))

(defun zz-create-file (fpath)
  "Process the file at path FPATH ..."
  (let ((tmp-buf-name (file-name-nondirectory fpath)))
    (set-buffer (get-buffer-create tmp-buf-name))  
    (goto-char 0)
    (dolist (item zz-dotemacs-content)
      (insert item)
      (insert "\n"))
    (write-file fpath)
    (kill-buffer tmp-buf-name)))

(message "setup .emacs to %s" zz-home-path)
(zz-create-file zz-dotemacs-path)


