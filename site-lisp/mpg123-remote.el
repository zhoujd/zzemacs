;;; mpg123-remote.el --- stuff to help control mpg123.el from the outside world.

;; Copyright (C) 2004 Len Trigg.

;; Author: Len Trigg <lenbok@myrealbox.com>
;; Maintainer: Len Trigg <lenbok@myrealbox.com>
;; Created: 2003-10-22
;; Version: 1.2

;;; Commentary:

;; Introduction:
;;
;; If you use mpg123.el as your music playing application you
;; sometimes have the need to control it from either outside the
;; mpg123 buffer, or outside emacs entirely. This file provides
;; functions that are intended to be bound to global keys or invoked
;; using gnuclient.
;;
;; For global key bindings, place a command like this in your emacs
;; startup file.
;;
;;   (global-set-key "\M-o" 'remote-mpg123-pause) ;mpg123 Pause/play toggle
;;
;; For remote use, you have to be running emacs in server mode, by
;; calling either (server-start) for GNU Emacs, or (gnuserv-start) in
;; the case of XEmacs.
;;
;; You can then invoke the remote-mpg123-* functions externally like this:
;;
;;   gnuclient -batch -f "remote-mpg123-load \"$file\" "
;;
;;   gnuclient -batch -f "remote-mpg123-pause"
;;
;; Here is a small shell script that makes use of the functions:
;; 
;; #!/bin/sh
;; res=$(gnuclient -batch -f "fboundp 'mpg123" 2>&1)
;; if [ "$res" != "t" ]; then
;;     echo $res
;;     echo "mpg123 is not available"
;;     exit 1
;; fi
;; case "$1" in
;;   --load)
;;         shift
;;         file=$*
;;         gnuclient -batch -f "remote-mpg123-load \"$file\" " 2>&1
;;         ;;
;;   --append)
;;         shift
;;         file=$*
;;         gnuclient -batch -f "remote-mpg123-append \"$file\" " 2>&1
;;         ;;
;;   --pause)
;;         gnuclient -batch -f "remote-mpg123-pause" 2>&1
;;         ;;
;;   --next)
;;         gnuclient -batch -f "remote-mpg123-next" 2>&1
;;         ;;
;;   --prev)
;;         gnuclient -batch -f "remote-mpg123-prev" 2>&1
;;         ;;
;;   *)
;;         echo "Usage: $0 {--load FILE | --append FILE | --pause | --next | --prev}"
;;         exit 1
;; esac
;;

;;; Code:

;; Dependencies:
(require 'mpg123)

(defun mpg123-active-p ()
"Returns the mpg123 buffer if mpg123 is active, otherwise nil. Songs
need not be actually playing. This may be used as an indicator as to
whether mpg123 functions can be called."
  (and (boundp 'mpg123*buffer) 
       (get-buffer mpg123*buffer)))

(defun remote-mpg123-load (file)
  "Loads FILE into mpg123 and starts playing."
  (let ((cb (current-buffer)))
    (unwind-protect
        (progn
          (if (mpg123-active-p)
              (save-excursion
                (set-buffer mpg123*buffer)
                (mpg123-quit t)))
          (mpg123 file)
          (mpg123-play))
      (switch-to-buffer cb))))

(defun remote-mpg123-append (file)
  "Appends FILE to mpg123 playlist without altering playing song."
  (let ((cb (current-buffer)))
    (unwind-protect
        (progn
          (if (mpg123-active-p)
              (save-excursion
                (set-buffer mpg123*buffer)
                (mpg123-add-new file))
            (mpg123 file)))
      (switch-to-buffer cb))))

(defun remote-mpg123-pause ()
  "Pauses (or unpauses) mpg123 playing song."
  (interactive)
  (if (mpg123-active-p)
      (save-excursion
        (set-buffer mpg123*buffer)
        (mpg123-play-stop))))

(defun remote-mpg123-next ()
  "Instructs mpg123 to jump to the next song."
  (interactive)
  (if (mpg123-active-p)
      (save-excursion
        (set-buffer mpg123*buffer)
        (mpg123-> 1))))

(defun remote-mpg123-prev ()
  "Instructs mpg123 to jump to the previous song."
  (interactive)
  (if (mpg123-active-p)
      (save-excursion
        (set-buffer mpg123*buffer)
        (mpg123-< 1))))

(defun remote-mpg123-current-file ()
  "Returns the filename of the currently playing song."
  (interactive)
  (if (mpg123-active-p)
      (save-excursion
        (set-buffer mpg123*buffer)
        (mpg123:get-music-info mpg123*cur-music-number 'filename))))

(provide 'mpg123-remote)

