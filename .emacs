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
        lib-setting
        env-setting
        other-setting
        sample-setting
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
        vterm-setting
        eshell-setting
        w3m-setting
        doc-setting
        evil-setting
        c-setting
        lisp-setting
        scheme-setting
        perl-setting
        python-setting
        go-setting
        sql-setting
        groovy-setting
        docker-setting
        virt-setting
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

(message "Loading settings...done (%s)" (emacs-init-time))


;; Local Variables:
;; coding: utf-8
;; End:
;;;; .emacs end here
