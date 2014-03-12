;;; isb-filter.el --- add filter to iswitchb mode

;; Copyright (C) 2014 Free Software Foundation, Inc.
;;
;; Author:  <zjd-405@163.com>
;; Maintainer:  <zjd-405@163.com>
;; Created: 26 Feb 2014
;; Version: 0.01
;; Keywords

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'isb-filter)

;;; Code:

(require 'iswitchb)

(defun isb-filter-cond (name type)
  (let* ((isb-filter-dired (with-current-buffer
                            name (derived-mode-p 'dired-mode)))
         (isb-filter-emacs (or (string-match "^\\*.*\\*$" name)
                               (string-match "^ " name)))
         (isb-filter-common (and (not isb-filter-dired)
                                 (not isb-filter-emacs)))
         (isb-filter-select nil))
    (case type
      ((isb-dired)  (setq isb-filter-select isb-filter-dired))
      ((isb-emacs)  (setq isb-filter-select isb-filter-emacs))
      ((isb-common) (setq isb-filter-select isb-filter-common)))
    isb-filter-select))

(defun isb-filter-buffer (type)
  (let ((isb-filter-list '()))
    (dolist (name (iswitchb-make-buflist nil))
            (when (isb-filter-cond name type)
              (setq isb-filter-list (cons name isb-filter-list))))
    (if isb-filter-list
        (setq isb-filter-list (reverse isb-filter-list))
        (setq isb-filter-list iswitchb-matches))
    isb-filter-list))

(defun isb-show-dired ()
   (interactive)
   (setq iswitchb-buflist (isb-filter-buffer 'isb-dired))
   (setq iswitchb-rescan t)
   (delete-minibuffer-contents))

(defun isb-show-common ()
   (interactive)
   (setq iswitchb-buflist (isb-filter-buffer 'isb-common))
   (setq iswitchb-rescan t)
   (delete-minibuffer-contents))

(defun isb-show-emacs ()
   (interactive)
   (setq iswitchb-buflist (isb-filter-buffer 'isb-emacs))
   (setq iswitchb-rescan t)
   (delete-minibuffer-contents))

(defun isb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))


(provide 'isb-filter)
;;; isb-filter.el ends here
