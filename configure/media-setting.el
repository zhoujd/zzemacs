;;;; media-setting.el --- media setting file
;;

(zz:load-path "site-lisp")

;;mpv
(require 'mpv)

;;bingo
(require 'bongo)
(setq bongo-logo nil)
(setq bongo-mode-line-indicator-mode nil)
(setq bongo-enabled-backends '(mpv vlc))


(provide 'media-setting)

;;; media-setting.el ends here
