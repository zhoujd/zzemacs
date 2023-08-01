;;;; dired-setting.el --- dired config file
;;;

;;Dired-x is a build-in model
(require 'dired-x)

;;human-readable sizes, hidden files and dir first
(setq dired-listing-switches "-aBhl --group-directories-first")

;;total used in directory xxK available yyG
(setq dired-free-space-program "df")
(setq dired-free-space-args "-Pkh")

;;allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always)) ;;"always" means no asking
(setq dired-recursive-deletes (quote top)) ;;"top" means ask once

;;Now, go to dired, then call split-window-vertically,
;;then go to another dired dir. Now, when you press C to copy,
;;the other dir in the split pane will be default destination.
;;Same for R (rename; move).
(setq dired-dwim-target t)

;;The same buffer for viewing directory, instead of spawning many
;;In dired, you can press 'a' instead of 'Enter' to open the dir
;;Disabled commands
(put 'dired-find-alternate-file 'disabled nil)

;;If you want Enter ↵ and ^ (parent dir) to use the same buffer
;;put the following in your emacs init file:
;(defkeys-map dired-mode-map
;  ((kbd "<return>") 'dired-find-alternate-file)
;  ((kbd "^")        (lambda () (interactive) (find-alternate-file ".."))))

;;http://www.emacswiki.org/emacs/w32-browser.el
(when-ms-windows
  (require 'w32-browser)
  (eval-after-load "dired"
    '(defkeys-map dired-mode-map
       ((kbd "C-c C-e") (lambda ()
                          (interactive)
                          (w32-browser (dired-replace-in-string
                                       "/" "\\"
                                       (dired-get-filename))))))))

(add-hook 'dired-mode-hook
          (lambda ()
            (interactive)
            (make-local-variable  'dired-sort-map)
            (setq dired-sort-map (make-sparse-keymap))
            (defkeys-map dired-mode-map
              ("s"   dired-sort-map))
            (defkeys-map dired-sort-map
              ("s"   (zz:dired-sort zz:dired-sort-size     "S"))
              ("x"   (zz:dired-sort zz:dired-sort-extend   "X"))
              ("t"   (zz:dired-sort zz:dired-sort-time     "t"))
              ("n"   (zz:dired-sort zz:dired-sort-name     ""))
              ("g"   (zz:dired-sort zz:dired-sort-no-group "G"))
              ("h"   (zz:dired-sort zz:dired-sort-human    "h"))
              )))

;;https://github.com/muennich/sxiv
;;https://wiki.archlinux.org/title/Sxiv
(setq dired-guess-shell-alist-user
      (list
       (list "\\.rm$"     "mpv")
       (list "\\.rmvb$"   "mpv")
       (list "\\.avi$"    "mpv")
       (list "\\.asf$"    "mpv")
       (list "\\.wmv$"    "mpv")
       (list "\\.mkv$"    "mpv")
       (list "\\.mp4$"    "mpv")
       (list "\\.webm$"   "mpv")
       (list "\\.mp3$"    "mpa")
       (list "\\.wma$"    "mpa")
       (list "\\.htm$"    "firefox")
       (list "\\.html$"   "firefox")
       (list "\\.chm$"    "xchm")
       (list "\\.pdf$"    "zathura")
       (list "\\.pptx$"   "libreoffice")
       (list "\\.ppt$"    "libreoffice")
       (list "\\.docx$"   "libreoffice")
       (list "\\.doc$"    "libreoffice")
       (list "\\.xlsx$"   "libreoffice")
       (list "\\.xls$"    "libreoffice")
       (list "\\.jpeg$"   "sxiv")
       (list "\\.jpg$"    "sxiv")
       (list "\\.png$"    "sxiv")
       (list "\\.gif$"    "sxiv")
       (list "\\.bmp$"    "sxiv")
       (list "\\.vsdx"    "drawio")
       (list "\\.drawio"  "drawio")
       ))

;;Direx
(require 'direx)
(require 'direx-project)
(defkeys-map global-map
  ((kbd "C-x C-j") 'direx:jump-to-directory))

(zz:load-path "site-lisp/dired-hacks")
(require 'dired-filter)

;;image dired
(require 'image-dired)
(setq image-dired-external-viewer "sxiv")
(setq image-dired-thumb-size 130)

;;dired play with vlc/mpv
(zz:load-path "elisp")
(require 'dired-play)

;;dired-mode-map
(defkeys-map dired-mode-map
  ((kbd "/") dired-filter-map)
  ((kbd "r") 'dired-play-start)
  ((kbd "z") 'dired-play-stop))

;;https://github.com/thomp/dired-launch
;;J (dired-launch-command) launches the file using the preferred application
;;K (dired-launch-with-prompt-command) prompts for the application and then launches the file
(require 'dired-launch)
(dired-launch-enable)


;;dirvish
(zz:load-path "site-lisp/dirvish")
(require 'dirvish)


(provide 'dired-setting)

;;; dired-setting.el ends here
