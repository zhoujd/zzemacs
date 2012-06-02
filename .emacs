;;;; .emacs --- emacs config file
;;;
(defvar zzemacs-path "~/zzemacs/" "zzemacs`s path")
(setq zz-bin-path (concat zzemacs-path "bin"))

(setenv "PATH" (concat zz-bin-path path-separator (getenv "PATH")))
(setq exec-path (cons zz-bin-path exec-path))

(defvar zz-setting-list '(
                          "common-setting.el"
                          "other-setting.el"
                          "media-setting.el"
                          "prog-setting.el"
                          "c-setting.el"
                          "lisp-setting.el"
                          "perl-setting.el"
                          "python-setting.el"
                          "java-setting.el"
                          "csharp-setting.el"
                          "key-setting.el"
                          ))

(defun zz-load-path (path)
  "my add to list"
  (add-to-list 'load-path (concat zzemacs-path path)))

(defun zz-load-file (file)
  "my load file"
  (load-file (concat zzemacs-path file)))

(defun zz-load-configure (file)
  "my load configure file"
  (load-file (concat zzemacs-path "configure/" file)))

(mapcar 'zz-load-configure zz-setting-list)

;;develop setting for tags path etc.
(defvar zz-dev-set-file "temp-setting.el")
(when (file-exists-p (concat zzemacs-path "configure/" zz-dev-set-file))
  (zz-load-configure zz-dev-set-file))

;;;; .emacs end here
