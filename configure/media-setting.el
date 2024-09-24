;;;; media-setting.el --- media setting file
;;

(zz/load-path "site-lisp")

;;mpv
(require 'mpv)
(defun mpa-play (path)
  (interactive "fFile: ")
  (mpv-start (expand-file-name path) "--no-video"))
(defalias 'mpv 'mpv-play)
(defalias 'mpa 'mpa-play)

;;bingo
;;amixer: Mixer attach default error: No such file or directory
(when (string-empty-p
       (shell-command-to-string  "amixer | grep error"))
  (require 'bongo)
  (setq bongo-logo nil)
  (setq bongo-enabled-backends '(vlc mpv))
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-mode-line-indicator-format nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-track-mark-icon-file-name nil)
  (setq bongo-vlc-program-name "cvlc"))


(provide 'media-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; media-setting.el ends here
