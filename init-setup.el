#!/usr/bin/emacs --script
;;;;this script for init emacs configure
;;;;http://ergoemacs.org/emacs/emacs.html
;;$cd <init-setup.el>
;;$emacs --script init-setup.el

(message "zzemacs setup script start ...")
(message "zzemacs run on %s" system-type)

(defun zz-create-file (fpath content)
  "Process the file at path FPATH ..."
  (let ((tmp-buf-name (file-name-nondirectory fpath)))
    (set-buffer (get-buffer-create tmp-buf-name))  
    (goto-char 0)
    (dolist (item content)
      (insert item)
      (insert "\n"))
    (write-file fpath)
    (kill-buffer tmp-buf-name)))

(defun zz-setup-dotemacs-nt ()
  (defvar zz-winnt-home-path "c:/zznix/home/zhoujd")
  (defvar zz-home-path (getenv "APPDATA"))
  (defvar zz-dotemacs-content (list
                               (format ";;;this is .emacs for zhoujd, generate by install.el.")
                               (format "(setenv \"HOME\" \"%s\")" zz-winnt-home-path)
                               (format "(setq default-directory \"~\")")
                               (format "(load-file \"~/zzemacs/.emacs\")")
                               ))
  (defvar zz-dotemacs-path (concat zz-home-path "/.emacs"))
  (message "setup .emacs to %s on %s" zz-home-path system-type)
  (zz-create-file zz-dotemacs-path zz-dotemacs-content))

(defun zz-setup-dotemacs-unix ()
  (defvar zz-emacs-root  default-directory)
  (defvar zz-home-path (getenv "HOME"))
  (defvar zz-dotemacs-content (list
                               (format ";;;this is .emacs for zhoujd, generate by install.el.")
                               (format "(defvar zzemacs-path \"%s\")" zz-emacs-root)
                               (format "(load-file (concat zzemacs-path \".emacs\"))")
                               ))
  (defvar zz-dotemacs-path (concat zz-home-path "/.emacs"))
  (message "setup .emacs to %s on %s" zz-home-path system-type)
  (zz-create-file zz-dotemacs-path zz-dotemacs-content))

(defun zz-setup-dotemacs ()
  "setup dotemacs"
  (if (eq system-type 'windows-nt)
      (zz-setup-dotemacs-nt)
      (zz-setup-dotemacs-unix)))

(defun zz-setup-font ()
  "setup font to system font path"
  (if (eq system-type 'windows-nt)
      '()
      (progn
        (let ((sys-font-path "/usr/share/fonts/truetype/"))
          (if (= 0 (user-uid))
              (progn
                (copy-file (concat default-directory "font/consola.ttf") sys-font-path t)
                (copy-file (concat default-directory "font/MSYHMONO.ttf") sys-font-path t)
                (message "setup font to %s" sys-font-path))
              (message "setup font need run with sudo.")
              )))))

(defun zz-setup-third-party ()
  "setup third partys"
  (let ((third-setup-name (format "%s/third-party/setup.el" zz-home-path)))
    (if (file-exists-p third-setup-name)
        (load third-setup-name)
        (message "setup third party %s does not exist" third-setup-name))))

(zz-setup-dotemacs)
(zz-setup-font)
(zz-setup-third-party)


(message "zzemacs setup script end ...")

(provide 'init-setup)
;;;;init-setup.el is end
