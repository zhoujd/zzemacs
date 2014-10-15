;;;; .emacs --- emacs config file
;;;

;;open debug-on-error when starup begin
(custom-set-variables
 '(debug-on-error t)
 '(byte-compile-warnings nil)
 )

(defvar zzemacs-path (format "%s/zzemacs" (getenv "HOME"))
  "zzemacs`s path")

(defun zz-add-os-path (path)
  (interactive "DDirectory: ")
  (when (file-exists-p path)
      (setenv "PATH" (concat path path-separator (getenv "PATH")))
      (setq exec-path (cons path exec-path))))

(defun zz-load-path (path)
  "my add to list"
  (when (not (member path load-path))
    (add-to-list 'load-path (concat zzemacs-path "/" path))))

(defun zz-load-file (file)
  "my load file"
  (load-file (concat zzemacs-path "/" file)))

(defun zz-load-configure (file)
  "my load configure file"
  (load-file (concat zzemacs-path "/configure/" file)))

(zz-load-path "configure")
(mapc (lambda (setting)
        (require setting)
        (message "Load %s end ..." setting))
      '(
        macro-setting
        common-setting
        env-setting
        dired-setting
        prog-setting
        vc-setting
        ediff-setting
        other-setting
        shell-setting
        sample-setting
        media-setting
        w3m-setting
        evil-setting
        c-setting
        lisp-setting
        scheme-setting
        perl-setting
        python-setting
        doc-setting
        keymap-setting
        fn-setting
        key-setting
        ))

;;develop setting for tags path etc.
(defvar zz-dev-set-file "temp-setting.el")
(when (file-exists-p (concat zzemacs-path "/configure/" zz-dev-set-file))
  (zz-load-configure zz-dev-set-file))

;;configure saved from menu "Save Options"
(setq custom-file (concat zzemacs-path "/custom.el"))

;;close debug-on-error when starup end
(custom-set-variables
 '(debug-on-error nil)
 '(byte-compile-warnings t)
 )

;;;; .emacs end here
