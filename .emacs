;;;; .emacs --- emacs config file
;;;

(defvar zzemacs-path "~/zzemacs/" "zzemacs`s path")

(defun zz-add-os-path (path)
  (interactive "DDirectory: ")
  (setenv "PATH" (concat path path-separator (getenv "PATH")))
  (setq exec-path (cons path exec-path)))

(defun zz-load-path (path)
  "my add to list"
  (add-to-list 'load-path (concat zzemacs-path path)))

(defun zz-load-file (file)
  "my load file"
  (load-file (concat zzemacs-path file)))

(defun zz-load-configure (file)
  "my load configure file"
  (load-file (concat zzemacs-path "configure/" file)))

(zz-add-os-path (concat zzemacs-path "bin"))
(zz-load-path "configure")
(mapcar '(lambda (setting)
          (require setting)
          (message "Load %s end ..." setting))
        '(
          key-setting
          common-setting
          dired-setting
          prog-setting
          other-setting
          shell-setting
          sample-setting
          media-setting
          c-setting
          lisp-setting
          scheme-setting
          perl-setting
          python-setting
          java-setting
          csharp-setting
          php-setting
          muse-setting
          ))

;;develop setting for tags path etc.
(defvar zz-dev-set-file "temp-setting.el")
(when (file-exists-p (concat zzemacs-path "configure/" zz-dev-set-file))
  (zz-load-configure zz-dev-set-file))

;;configure saved from menu "Save Options"
(setq custom-file (concat zzemacs-path "custom.el"))


;;;; .emacs end here
