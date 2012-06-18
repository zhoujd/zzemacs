;;; multi-shell.el --- Multi-Shell Manager

;; Filename: multi-shell.el
;; Description: Multi-Shell Manager
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-10 12:03:20
;; Version: 0.2.2
;; Last-Updated: 2009-06-29 18:04:34
;;           By: Andy Stewart
;; URL:
;; Keywords: multi-shell, shell
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `shell' `ansi-color' `cl'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Multi-Shell Manager
;;
;; Here, have below command you can use:
;;
;;      `multi-shell-new'                create a new shell buffer.
;;      `multi-shell-next'               switch to next shell buffer.
;;      `multi-shell-prev'               switch to previous shell buffer.
;;      `multi-shell-current-directory'  create a new shell with current-directory.
;;
;;      choose your like key bind above command. ;)

;;; Require
(require 'shell)
(require 'ansi-color)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup multi-shell nil
  "Multi-Shell Manager"
  :group 'shell)

(defcustom multi-shell-command nil
  "The command that multi-shell will use.
If nil, will try to get value of
environment variable `SHELL' or `ESHELL'."
  :type 'string
  :group 'multi-shell)

(defcustom multi-shell-use-ansi-color t
  "Whether translate ANSI escape sequences into faces.
And default is t."
  :type 'boolean
  :group 'multi-shell)

(defcustom multi-shell-try-create t
  "Try to create a new shell buffer when switch.

When use `multi-shell-next' or `multi-shell-prev' to switch shell buffer,
try to create a new shell buffer if haven't any shell buffers exist."
  :type 'boolean
  :group 'multi-shell)

(defcustom multi-shell-default-dir "~/"
  "The default directory for create shell buffer,
when current local directory is no exist."
  :type 'string
  :group 'multi-shell)

(defcustom multi-shell-buffer-name "multi-shell"
  "The name of shell buffer."
  :type 'string
  :group 'multi-shell)

(defcustom multi-shell-bottom-window-height -13
  "The height of bottom window create."
  :type 'integer
  :group 'multi-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun multi-shell-new ()
  "Create a new multi-shell buffer."
  (interactive)
  (let* ((shell-buffer (multi-shell-get-buffer)))
    (set-buffer shell-buffer)
    ;; Add `ansi-color' to shell-mode
    (if multi-shell-use-ansi-color
        (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
      (remove-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))
    ;; Load mode
    (save-window-excursion
      (shell (buffer-name shell-buffer)))
    ;; Handle shell buffer close
    (multi-shell-handle-close)
    ;; Switch shell buffer
    (switch-to-buffer shell-buffer)
    ;; Add hook to be sure `shell' interrupt subjob before buffer killed
    (add-hook 'kill-buffer-hook 'multi-shell-handle-kill-buffer)
    ))

(defun multi-shell-get-buffer ()
  "Get shell buffer."
  (let* ((shell-list-length (length (multi-shell-list)))          ;get length of shell list
         (index (if shell-list-length (1+ shell-list-length) 1))) ;setup new shell index
    (with-temp-buffer
      ;; switch to current local directory,
      ;; if in-existence, switch to `multi-shell-default-dir'.
      (cd (or default-directory (expand-file-name multi-shell-default-dir)))
      ;; adjust value N when max index of shell buffer is less than length of shell list
      (while (buffer-live-p (get-buffer (format "*%s<%s>*" multi-shell-buffer-name index)))
        (incf index))
      ;; Set explicit shell program
      (setq explicit-shell-file-name (or multi-shell-command
                                         (getenv "SHELL")
                                         (getenv "ESHELL")
                                         "/bin/sh"))
      ;; Return buffer
      (get-buffer-create (format "*%s<%s>*" multi-shell-buffer-name index))
      )))

(defun multi-shell-handle-close ()
  "This function for close current shell buffer.
When `exit' from shell buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun multi-shell-handle-kill-buffer ()
  "Function that hook `kill-buffer-hook'."
  ;; Interrupt the current subjob
  ;; when have alive process with current shell buffer
  (when (and (eq major-mode 'shell-mode)
             (comint-check-proc (current-buffer)))
    (comint-interrupt-subjob)))

(defun multi-shell-list ()
  "The shell buffers presently active."
  ;; Autload command `remove-if-not'.
  (autoload 'remove-if-not "cl-seq")
  (sort
   (remove-if-not (lambda (b)
                    (setq case-fold-search t)
                    (string-match
                     (format "^\\\*%s<[0-9]+>\\\*$" multi-shell-buffer-name)
                     (buffer-name b)))
                  (buffer-list))
   (lambda (a b)
     (< (string-to-number
         (cadr (split-string (buffer-name a) "[<>]")))
        (string-to-number
         (cadr (split-string (buffer-name b)  "[<>]")))))))

(defun multi-shell-switch (direction offset)
  "Switch to shell buffer.
If DIRECTION is `NEXT', switch to next shell buffer.
if DIRECTION is `PREVIOUS', switch to previous shell buffer.
Default OFFSET is 1.
If option `multi-shell-try-create' is non-nil, will create a new shell buffer
if have any shell buffer exist."
  (let (shells this-buffer)
    (setq shells (multi-shell-list))
    (if (consp shells)
        (progn
          (setf (cdr (last shells)) shells)
          (setq this-buffer (position (current-buffer) (multi-shell-list)))
          (if this-buffer
              (if (eql direction 'NEXT)
                  (switch-to-buffer (nth (+ this-buffer offset) shells))
                (switch-to-buffer (nth (+ (- (length (multi-shell-list)) offset)
                                          this-buffer) shells)))
            (switch-to-buffer (car shells))))
      (if multi-shell-try-create
          (progn
            (multi-shell-new)
            (message "Create a new `multi-shell' buffer."))
        (message "Haven't any `multi-shell' buffer exist.")))))

(defun multi-shell-next (&optional offset)
  "Switch to next shell buffer."
  (interactive "P")
  (multi-shell-switch 'NEXT (or offset 1)))

(defun multi-shell-prev (&optional offset)
  "Switch to previous shell buffer."
  (interactive "P")
  (multi-shell-switch 'PREVIOUS (or offset 1)))

(defun multi-shell-current-directory ()
  "Open shell that start at current directory."
  (interactive)
  (split-window-vertically multi-shell-bottom-window-height)
  (other-window 1)
  (multi-shell-new))

(provide 'multi-shell)

;;; multi-shell.el ends here

;;; LocalWords:  multi SPC
