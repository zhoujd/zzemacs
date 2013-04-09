#!/usr/bin/emacs --script
;;check os type
(message "hello, zzemacs")
(if (eq system-type 'windows-nt)
  (message "emacs run on windows")
  (message "emacs run on un*x"))

(defvar zz-home-path (getenv "HOME"))
(defvar zz-dotemacs-path (concat zz-home-path "/.emacs"))

(defvar zz-dotemacs-content (list
                             (format "%s" ";;;this is .emacs for zhoujd, generate by install.el.")
                             (format "(defvar zzemacs-path \"%s\")" default-directory)
                             (format "%s" "(load-file (concat zzemacs-path \".emacs\"))")
                             ))

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

(zz-create-file zz-dotemacs-path)


