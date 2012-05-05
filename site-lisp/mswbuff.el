;;; mswbuff.el --- Quick switch between Emacs buffers.

;; Copyright (C) 1998, 2000, 2001, 2003 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 12 Nov 1998
;; Keywords: extensions convenience
;; Revision: $Id: mswbuff.el,v 1.19 2003/04/25 11:31:22 ponced Exp $

(defconst mswbuff-version "3.2")

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This package provides the commands `mswbuff-switch-to-next-buffer'
;; and `mswbuff-switch-to-previous-buffer' to respectively switch to
;; the next or previous buffer in the buffer list.

;; The `mswbuff-exclude-buffer-regexps' defines a list of regular
;; expressions for excluded buffers. The default setting excludes
;; buffers whose name begin with a blank character. To exclude all the
;; internal buffers (that is *scratch*, *Message*, etc...) you could
;; use the following regexps '("^ .*" "^\\*.*\\*").

;; Switching buffers pops-up a status window at the bottom of the
;; selected window. The status window shows the list of switchable
;; buffers where the switched one is hilighted using
;; `mswbuff-current-buffer-face'. This window is automatically
;; discarded after any command is executed or after the delay
;; specified by `mswbuff-clear-delay'.
;;
;; To install and use, put this file on your Emacs-Lisp load path and
;; add the following into your ~/.emacs startup file:
;;
;; (require 'mswbuff)
;;

;;; History:
;;

;;; Code:
(require 'timer)

;;; Options
;;
(defgroup mswbuff nil
  "Quick switch between Emacs buffers."
  :group 'extensions
  :group 'convenience
  :prefix "mswbuff-")

(defcustom mswbuff-status-window-layout nil
  "*Method used to ensure the switched buffer is always visible.
This occurs when the buffer list is larger than the status window
width. The possible choices are:

- - 'Default' If there is only one window in the frame (ignoring the
              minibuffer one and the status window itself) the status
              window height is adjusted.
              Otherwise horizontal scrolling is used.
- - 'Scroll'  Horizontal scrolling is always used.
- - 'Adjust'  Only adjust the window height."
  :group 'mswbuff
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Scroll"  scroll)
                 (const :tag "Adjust"  adjust)))

(defcustom mswbuff-clear-delay 3
  "*Time in seconds to delay before discarding the status window."
  :group 'mswbuff
  :type '(number :tag "seconds")) 

(defcustom mswbuff-separator ", "
  "*String used to separate buffer names in the status line."
  :group 'mswbuff
  :type 'string)

(defcustom mswbuff-header ""
  "*Status line header string."
  :group 'mswbuff
  :type 'string)

(defcustom mswbuff-trailer ""
  "*Status line trailer string."
  :group 'mswbuff
  :type 'string)

(defcustom mswbuff-window-min-text-height 1
  "*Minimum text height of the status window."
  :group 'mswbuff
  :type 'integer)

(defface mswbuff-default-face '((t nil))
  "*Default face used for buffer names."
  :group 'mswbuff)

(defface mswbuff-current-buffer-face '((t (:foreground "red" :bold t :underline t)))
  "*Face used to highlight the current buffer name."
  :group 'mswbuff)

(defface mswbuff-separator-face '((t (:foreground "blue")))
  "*Face used for separators."
  :group 'mswbuff)

(defcustom mswbuff-exclude-buffer-regexps '("^ ")
  "*List of regular expressions for excluded buffers.
The default setting excludes buffers whose name begin with a blank
character.  To exclude all the internal buffers (that is *scratch*,
*Message*, etc...) you could use the following regexps:
  (\"^ \" \"^\\*.*\\*\")."
  :group 'mswbuff
  :type '(repeat (regexp :format "%v")))

(defcustom mswbuff-load-hook '(mswbuff-default-load-hook)
  "Hook run when package has been loaded.
See also `mswbuff-default-load-hook'."
  :group 'mswbuff
  :type 'hook)

;;; Internals
;;
(defconst mswbuff-status-buffer-name "*mswbuff*"
  "Name of the working buffer used to display the buffer list.")

(defun mswbuff-include-p (name)
  "Return non-nil if buffer NAME can be included.
That is if NAME matches none of the `mswbuff-exclude-buffer-regexps'."
  (let ((rl (cons (regexp-quote mswbuff-status-buffer-name)
                  mswbuff-exclude-buffer-regexps)))
    (while (and rl (not (string-match (car rl) name)))
      (setq rl (cdr rl)))
    (null rl)))

(defun mswbuff-buffer-list ()
  "Return the list of switchable buffers.
That is without the ones whose name matches
`mswbuff-exclude-buffer-regexps'."
  (let ((blist (delq nil
                     (mapcar (function
                              (lambda (buf)
                                (and (mswbuff-include-p (buffer-name buf))
                                     buf)))
                             (buffer-list)))))
    (or (memq (current-buffer) blist)
        (setq blist (cons (current-buffer) blist)))
    blist))

(if (fboundp 'count-lines)
    (defalias 'mswbuff-count-lines 'count-lines)
  (defun mswbuff-count-lines (start end)
    "Return number of lines between START and END.
This is usually the number of newlines between them,
but can be one more if START is not equal to END
and the greater of them is not at the start of a line."
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (if (eq selective-display t)
            (save-match-data
              (let ((done 0))
                (while (re-search-forward "[\n\C-m]" nil t 40)
                  (setq done (+ 40 done)))
                (while (re-search-forward "[\n\C-m]" nil t 1)
                  (setq done (+ 1 done)))
                (goto-char (point-max))
                (if (and (/= start end)
                         (not (bolp)))
                    (1+ done)
                  done)))
          (- (buffer-size) (forward-line (buffer-size))))))))

(defun mswbuff-window-lines ()
  "Return the number of lines in current buffer.
This number may be greater than the number of actual lines in the
buffer if any wrap on the display due to their length."
  (mswbuff-count-lines (point-min) (point-max)))

(defun mswbuff-adjust-window (&optional text-height)
  "Adjust window height to fit its buffer contents.
If optional TEXT-HEIGHT is non-nil adjust window height to this
value."
  (setq text-height (max mswbuff-window-min-text-height
                         (or text-height
                             (mswbuff-window-lines))))
  (if (fboundp 'set-window-text-height)
      (set-window-text-height nil text-height)
    (let ((height (window-height))
          (lines  (+ 2 text-height)))
      (enlarge-window (- lines height))))
  (goto-char (point-min)))

;; Used to prevent discarding the status window on some mouse event.
(defalias 'mswbuff-ignore 'ignore)

;;; Compatibility
(cond
 ;; GNU Emacs 21
 ((and (not (featurep 'xemacs))
       (> emacs-major-version 20))
  
  (defun mswbuff-scroll-window (position)
    "Adjust horizontal scrolling to ensure that POSITION is visible."
    (setq truncate-lines t)
    (let ((automatic-hscrolling t))
      (goto-char position)))

  ;; Use mouse-1, mouse-3 on mode line buffer identification to
  ;; respectively switch to previous or next buffer.  And mouse-2 to
  ;; kill the current buffer.
  (let ((map mode-line-buffer-identification-keymap))
    (define-key map [mode-line mouse-1]
      'mswbuff-switch-to-previous-buffer)
    (define-key map [mode-line drag-mouse-1]
      'mswbuff-ignore)
    (define-key map [mode-line down-mouse-1]
      'mswbuff-ignore)
    (define-key map [mode-line mouse-2]
      'mswbuff-kill-this-buffer)
    (define-key map [mode-line mouse-3]
      'mswbuff-switch-to-next-buffer))

  )
 
 ;; GNU Emacs 20 or XEmacs
 (t

  (defconst mswbuff-extra-space 3
    "Extra space left in a line of the status window.
The default value correspond to the truncated glyphs + one space.")
  
  (defun mswbuff-scroll-window (position)
    "Adjust horizontal scrolling to ensure that POSITION is visible."
    (setq truncate-lines t)
    ;; Don't display the XEmacs horizontal scrollbar
    (let* ((window (selected-window))
           (wdth (window-width window))
           (hscr (window-hscroll window)))
      (if (featurep 'xemacs)
          (set-specifier horizontal-scrollbar-visible-p nil window))
      (if (>= position (+ wdth hscr))
          (set-window-hscroll window (- (+ position mswbuff-extra-space) wdth))
        (if (< position hscr)
            (set-window-hscroll window (- position mswbuff-extra-space))))))
  
  ))

(defun mswbuff-one-window-p (window)
  "Return non-nil if there is only one window in this frame.
Ignore WINDOW and the minibuffer window."
  (let ((count 0))
    (walk-windows #'(lambda (w)
                      (or (eq w window) (setq count (1+ count)))))
    (= count 1)))

(defvar mswbuff-buffer-list-holder nil
  "Hold the current displayed buffer list.")

(defun mswbuff-layout-status-line (window bcurr)
  "Layout a status line in WINDOW current buffer.
BCURR is the buffer name to highlight."
  (let ((blist  mswbuff-buffer-list-holder)
        (head   (or mswbuff-header    "" ))
        (separ  (or mswbuff-separator " "))
        (trail  (or mswbuff-trailer   "" ))
        (width  (window-width window))
        (lines  0)
        (adjust (or (eq mswbuff-status-window-layout 'adjust)
                    (and
		     (not (eq mswbuff-status-window-layout 'scroll))
		     (mswbuff-one-window-p window))
		    ))
        start end buffer bname fillr)
    (save-selected-window
      (select-window window)
      (setq header-line-format nil) ;; Hide Emacs 21 header line.
      (erase-buffer)
      (setq start (point))
      (insert head)
      (if (> (point) start)
          (set-text-properties
           start (point) '(face mswbuff-separator-face)))
      (while blist
        (setq buffer (car blist)
              blist  (cdr blist))
        (when (buffer-live-p buffer)
          (setq bname (buffer-name buffer)
                start (point)
                fillr (if blist separ trail))
          (when (and adjust
                     (> (- (+ start (length bname) (length fillr))
                           (* lines width))
                        width))
            (newline)
            (setq start (point)
                  lines (1+ lines)))
          (insert bname)
          (cond
           ((string-equal bname bcurr)
            (setq end (point))
            (set-text-properties
             start end '(face mswbuff-current-buffer-face)))
           (t
            (set-text-properties
             start (point) '(face mswbuff-default-face))))
          (setq start (point))
          (insert fillr)
          (if (> (point) start)
              (set-text-properties
               start (point) '(face mswbuff-separator-face)))))
      (if adjust
          (mswbuff-adjust-window)
        (mswbuff-adjust-window 1)
        (mswbuff-scroll-window end)))))

(defvar mswbuff-timer nil
  "Timer used to discard the status window.")

(defun mswbuff-show-status-window ()
  "Pop-up a status window at the bottom of the selected window.
The status window shows the list of switchable buffers where the
switched one is hilighted using `mswbuff-current-buffer-face'. It is
automatically discarded after any command is executed or after the
delay specified by `mswbuff-clear-delay'."
  (if (or mswbuff-buffer-list-holder
          (setq mswbuff-buffer-list-holder (mswbuff-buffer-list)))
      (let ((bcurr (buffer-name))
            (window-min-height 1)
            cursor-in-non-selected-windows)
        (with-current-buffer (get-buffer-create mswbuff-status-buffer-name)
          (let ((w (or (get-buffer-window mswbuff-status-buffer-name)
                       (split-window-vertically -2))))
            (set-window-buffer w (current-buffer))
            (mswbuff-layout-status-line w bcurr)
            (add-hook 'pre-command-hook 'mswbuff-pre-command-hook)
            (if (timerp mswbuff-timer)
                (cancel-timer mswbuff-timer))
            (setq mswbuff-timer (run-with-timer
                                mswbuff-clear-delay nil
                                #'mswbuff-discard-status-window)))))
    (message "No buffers eligible for switching.")))

(defun mswbuff-discard-status-window ()
  "Discard the status window."
  (let ((w (get-buffer-window mswbuff-status-buffer-name))
        (b (get-buffer mswbuff-status-buffer-name)))
    (and w (delete-window w))
    (and b (kill-buffer b))))

(defun mswbuff-pre-command-hook ()
  "Track successive calls to switch commands.
Run as a `pre-command-hook'."
  (if (memq this-command '(mswbuff-switch-to-previous-buffer
                           mswbuff-switch-to-next-buffer
                           mswbuff-kill-this-buffer
                           mswbuff-ignore))
      nil
    (mswbuff-discard-status-window)
    (setq mswbuff-buffer-list-holder nil))
  (if (timerp mswbuff-timer)
      (cancel-timer mswbuff-timer))
  (setq mswbuff-timer nil)
  (remove-hook 'pre-command-hook 'mswbuff-pre-command-hook))

(defun mswbuff-previous-buffer ()
  "Display and activate the buffer at the end of the buffer list."
  (let ((l (mswbuff-buffer-list)))
    (and l (switch-to-buffer (nth (1- (length l)) l)))))

(defun mswbuff-next-buffer ()
  "Display and activate the next buffer in the buffer list."
  (let ((l (nreverse (mswbuff-buffer-list))))
    (while (cdr l)
      (switch-to-buffer (car l))
      (setq l (cdr l)))))

;;; Commands
;;

;;;###autoload
(defun mswbuff-switch-to-previous-buffer ()
  "Switch to the previous buffer in the buffer list."
  (interactive)
  (mswbuff-previous-buffer)
  (mswbuff-show-status-window))

;;;###autoload
(defun mswbuff-switch-to-next-buffer ()
  "Switch to the next buffer in the buffer list."
  (interactive)
  (mswbuff-next-buffer)
  (mswbuff-show-status-window))

;;;###autoload
(defun mswbuff-kill-this-buffer ()
  "Kill the current buffer.
And update the status window if showing."
  (interactive)
  (kill-buffer (current-buffer))
  (and (get-buffer-window mswbuff-status-buffer-name)
       (mswbuff-show-status-window)))

(defun mswbuff-default-load-hook ()
  "Default hook run when package has been loaded.
Define keyboard shortcut [C-f6] for `mswbuff-switch-to-next-buffer' and
\[C-S-f6] for `mswbuff-switch-to-previous-buffer'."
  (global-set-key [(control f6)]       'mswbuff-switch-to-next-buffer)
  (global-set-key [(control shift f6)] 'mswbuff-switch-to-previous-buffer))

(provide 'mswbuff)

(run-hooks 'mswbuff-load-hook)

;;; mswbuff.el ends here
