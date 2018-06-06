;;;; .emacs --- emacs config file
;;;

(defvar zzemacs-path (format "%s/zzemacs" (getenv "HOME"))
  "zzemacs`s path")

(defun zz-add-os-path (path)
  (interactive "DDirectory: ")
  (when (file-exists-p path)
      (setenv "PATH" (concat path path-separator (getenv "PATH")))
      (setq exec-path (cons path exec-path))))

(defun zz-add-lib-path (path)
  (interactive "DDirectory: ")
  (when (file-exists-p path)
      (setenv "LD_LIBRARY_PATH" (concat path path-separator (getenv "LD_LIBRARY_PATH")))))

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
        complete-setting
        ac-setting
        prog-setting
        gdb-setting
        vc-setting
        ediff-setting
        other-setting
        shell-setting
        sample-setting
        w3m-setting
        doc-setting
        game-setting
        dired-setting
        ;;media-setting
        ;;evil-setting
        c-setting
        lisp-setting
        scheme-setting
        perl-setting
        python-setting
        ;;matlab-setting
        ;;spell-setting
        ;;gnuplot-setting
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

;;;; .emacs end here
