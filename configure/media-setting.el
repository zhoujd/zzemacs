;;;; media-setting.el --- media setting file
;;

(zz:load-path "site-lisp")

;; https://www.mpg123.de/download.shtml
(require 'mpg123)
(setq mpg123-lang 0)
(add-hook 'mpg123-hook
          (lambda ()
            (local-unset-key (kbd "<down-mouse-1>"))))

(require 'mpg123-remote)
(setq mpg123-show-help t)
(setq mpg123-lazy-slider t)
(setq mpg123-display-slider nil)

(defun zz:mpg123-format-name-function (artist album title tracknum filename)
  (concat (if (or mpg123-omit-id3-artist
                  (string= artist ""))
              ""
              (concat artist " - "))
          (if (string< "" title)
              title
              filename)))

(setq mpg123-format-name-function 'zz:mpg123-format-name-function)


(provide 'media-setting)

;;; media-setting.el ends here
