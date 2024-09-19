;;;; .emacs --- emacs config file
;;;

(defvar zzemacs-path (format "%s/zzemacs" (getenv "HOME"))
  "zzemacs`s path")

(defun zz/load-path (path)
  "add to list"
  (when (not (member path load-path))
    (add-to-list 'load-path (concat zzemacs-path "/" path))))

(zz/load-path "configure")
(mapc (lambda (setting)
        (require setting)
        (message "Load %s...end" setting))
      '(
        macro-setting
        common-setting
        lib-setting
        env-setting
        tramp-setting
        company-setting
        sample-setting
        shell-setting
        gdb-setting
        other-setting
        cedet-setting
        complete-setting
        prog-setting
        helm-setting
        vc-setting
        ediff-setting
        dired-setting
        term-setting
        vterm-setting
        eshell-setting
        im-setting
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
        gnuplot-setting
        matlab-setting
        spell-setting
        ui-setting
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
