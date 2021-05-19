;;;; media-setting.el --- media setting file
;;

(zz:load-path "site-lisp")

;;mpv
(require 'mpv)

;;bingo
(require 'bongo)
(setq bongo-logo nil)
(setq bongo-enabled-backends '(mpv vlc))
(setq bongo-mode-line-indicator-mode nil)
(setq bongo-mode-line-indicator-format nil)
(setq bongo-display-track-icons nil)
(setq bongo-track-mark-icon-file-name nil)


(provide 'media-setting)

;;; media-setting.el ends here
