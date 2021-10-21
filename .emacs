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

(defconst zz:emacs-start-time (current-time)
  "track of loading time")

(zz:load-path "configure")
(mapc (lambda (setting)
        (require setting)
        (message "Load %s end ..." setting))
      '(
        macro-setting
        common-setting
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
        dired-setting
        shell-setting
        term-setting
        eshell-setting
        other-setting
        sample-setting
        w3m-setting
        doc-setting
        evil-setting
        c-setting
        lisp-setting
        scheme-setting
        perl-setting
        python-setting
        go-setting
        groovy-setting
        docker-setting
        vagrant-setting
        kube-setting
        http-setting
        media-setting
        game-setting
        typing-setting
        ;;matlab-setting
        ;;spell-setting
        ;;gnuplot-setting
        ;;package-setting
        keymap-setting
        fn-setting
        key-setting
        fkey-setting
        ))

(let ((elapsed (float-time (time-subtract (current-time)
                                          zz:emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))


;;;; .emacs end here
