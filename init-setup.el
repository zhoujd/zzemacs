#!/usr/local/bin/emacs --script
;;;;this script for init emacs configure
;;;;http://ergoemacs.org/emacs/emacs.html
;;$cd <init-setup.el>
;;$emacs --script init-setup.el

(message "zzemacs setup script start ...")
(message "system-type:%s system-name:%s" system-type system-name)

;;print argument is setting
(when argv
  (message "input argument are %s" argv))

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

(defvar zz-home-path (if (eq system-type 'windows-nt)
                         (getenv "APPDATA")
                         (getenv "HOME"))
  "home path setting")

(defvar zz-dotemacs-path (concat zz-home-path "/.emacs")
  "dot emacs path setting")

(defun zz-setup-dotemacs-nt ()
  "setup zzemacs on nt"
  (let* ((zz-winnt-home-path "c:/zznix/home/zhoujd")
         (zz-dotemacs-content (list
                               (format ";;;this is .emacs for zhoujd, generate by install.el.")
                               (format "(setenv \"HOME\" \"%s\")" zz-winnt-home-path)
                               (format "(setq default-directory \"~\")")
                               (format "(defvar zzemacs-path (concat default-directory \"/zzemacs/\"))")
                               (format "(if (file-exists-p (concat zzemacs-path \".emacs\"))")
                               (format "    (load-file (concat zzemacs-path \".emacs\"))")
                               (format "    (message \"zzemacs has not install\"))")
                               )))
    (message "setup .emacs to %s on %s" zz-home-path system-type)
    (zz-create-file zz-dotemacs-path zz-dotemacs-content)))

(defun zz-setup-dotemacs-unix ()
  "setup zzemacs on unix"
  (let* ((zz-emacs-root default-directory)
         (zz-dotemacs-content (list
                               (format ";;;this is .emacs for zhoujd, generate by install.el.")
                               (format "(defvar zzemacs-path \"%s\")" zz-emacs-root)
                               (format "(if (file-exists-p (concat zzemacs-path \".emacs\"))")
                               (format "    (load-file (concat zzemacs-path \".emacs\"))")
                               (format "    (message \"zzemacs has not install\"))")
                               )))
    (message "setup .emacs to %s on %s" zz-home-path system-type)
    (zz-create-file zz-dotemacs-path zz-dotemacs-content)))

(defun zz-setup-dotemacs ()
  "setup dotemacs"
  (if (eq system-type 'windows-nt)
      (zz-setup-dotemacs-nt)
      (zz-setup-dotemacs-unix)))

(defun zz-setup-font ()
  "setup font to system font path"
  (if (eq system-type 'windows-nt)
      (let ((sys-font-path (concat (getenv "SystemRoot") "/Fonts")))
         ;;(copy-file (concat default-directory "font/consola.ttf") sys-font-path t)
         ;;(copy-file (concat default-directory "font/MSYHMONO.ttf") sys-font-path t)
         (message "please copy font to %s on %s by your self" sys-font-path system-type))
      (let ((fonts-conf-path "~/.fonts.conf")
            (fonts-conf-content (list
                                 (format "<?xml version=\"1.0\"?>")
                                 (format "<!DOCTYPE fontconfig SYSTEM \"fonts.dtd\">")
                                 (format "<!-- /etc/fonts/fonts.conf file to configure system font access -->")
                                 (format "<fontconfig>")
                                 (format "  <!-- Font directory list -->")
                                 (format "  <dir>%s/zzemacs/font</dir>" zz-home-path)
                                 (format "</fontconfig>")
                                 )))
        (zz-create-file fonts-conf-path fonts-conf-content)
        (message "setup font to %s on %s" fonts-conf-path system-type))))

(defun zz-setup-third-party ()
  "setup third partys"
  (let ((third-setup-name (format "%s/third-party/setup.el" (concat default-directory "zzemacs"))))
    (if (file-exists-p third-setup-name)
        (load third-setup-name)
        (message "setup third party %s does not exist" third-setup-name))))

(zz-setup-dotemacs)
(zz-setup-font)
(zz-setup-third-party)


(message "zzemacs setup script end ...")

(provide 'init-setup)
;;;;init-setup.el is end
