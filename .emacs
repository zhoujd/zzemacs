;;;; .emacs --- emacs config file
;;;

(defvar zzemacs-path (format "%s/zzemacs" (getenv "HOME"))
  "zzemacs`s path")

(defun zz:load-path (path)
  "my add to list"
  (when (not (member path load-path))
    (add-to-list 'load-path (concat zzemacs-path "/" path))))

(defun zz:load-file (file)
  "my load file"
  (load-file (concat zzemacs-path "/" file)))

(defun zz:load-configure (file)
  "my load configure file"
  (load-file (concat zzemacs-path "/configure/" file)))

(zz:load-path "configure")
(mapc (lambda (setting)
        (require setting)
        (message "Load %s end ..." setting))
      '(
        macro-setting
        common-setting
        server-setting
        lib-setting
        env-setting
        cedet-setting
        complete-setting
        company-setting
        prog-setting
        helm-setting
        gdb-setting
        vc-setting
        ediff-setting
        other-setting
        shell-setting
        term-setting
        sample-setting
        w3m-setting
        doc-setting
        game-setting
        dired-setting
        evil-setting
        c-setting
        lisp-setting
        scheme-setting
        perl-setting
        python-setting
        go-setting
        docker-setting
        ;;media-setting
        ;;matlab-setting
        ;;spell-setting
        ;;gnuplot-setting
        ;;package-setting
        keymap-setting
        fn-setting
        key-setting
        fkey-setting
        ))

;;;; .emacs end here
