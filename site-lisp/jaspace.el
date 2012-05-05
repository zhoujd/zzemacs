;;; -*- coding: shift_jis-dos; -*-
;;; jaspace.el -- make Japanese 2-byte whitespaces visible.
;;; $Id: jaspace.el,v 1.1 2006/01/21 00:30:17 makoto Exp $

;; Copyright (C) 2002-2003, Satomi I.

;; This file is NOT a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2 of the License, or any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; `jaspace-mode' make Japanese 2-byte whitespaces visible by taking
;; advantage of buffer-display-table and font-lock features.
;;
;; This code has been tested on GNU Emacs 21.2 (msvc-nt) only. Not sure if
;; it works properly on Emacs 20 or earlier.
;;
;; Known Problems:
;;
;; - When a file is dragged from the shell (at least on Windows NT),
;;   jaspace-mode does not enabled after some action has been taken
;;   (for example, click on the buffer or type any key). Is it related to
;;   font-lock-support-mode?

;;; Install:

;; Just add the following line to your .emacs somewhere after fundamental
;; features, especially coding systems, have been set.
;;
;; (require 'jaspace)
;;
;; Or (load) or (autoload) if you like to do so.
;; Then you can use the command jaspace-mode(, -on, -off) or Minor Modes
;; menu on the mode line to turn jaspace-mode on/off.
;;
;; To change the alternate string for a Japanese character:
;; (setq jaspace-alternate-jaspace-string "__")  ; or any other string
;;
;; To enable end-of-line marker:
;; (setq jaspace-alternate-eol-string "\xab\n")  ; or any other string
;;
;; See the comments in the code for more information on customizable
;; variables.

;;; Code:

(eval-and-compile
  (if (featurep 'xemacs)
          (error "jaspace-mode does not work on XEmacs")))

(eval-when-compile
  (require 'cl))

(require 'font-lock)
(condition-case nil
        (require 'whitespace)
  (error nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customizable variables

(defgroup jaspace nil
  "Minor mode to make Japanese whitespaces visible."
  :group 'convenience)

(defcustom jaspace-alternate-jaspace-string "¢¢"
  "Alternate string to indicate a Japanese whitespace."
  :type '(choice (const nil)
                                 string)
  :group 'jaspace)

(defcustom jaspace-alternate-eol-string nil
  "Alternate string to be used as an end-of-line marker. Be sure to append
a `\n' at the end of the string when you customize this value.

NOTE: Do not set this variable on Emacs 20 or ealier. It may cause all
lines on the buffer to be concatenated."
  :type '(choice (const nil)
                                 string)
  :group 'jaspace)

(defcustom jaspace-highlight-tabs nil
  "Non-nil means jaspace-mode also highlights tab characters, if only
font-lock mode is enabled. This option does nothing but add a font-lock
keyword for a tab character.

Set this value to nil value if you are using other (more sophisticated)
tab-highlighting feature."
  :type 'boolean
  :group 'jaspace)

(defface jaspace-highlight-jaspace-face
  '((((class color) (background light)) (:foreground "azure3"))
        (((class color) (background dark)) (:foreground "pink4")))
  "Face used to highlight Japanese whitespaces when font-lock mode is on."
  :group 'jaspace)

(defface jaspace-highlight-eol-face
  '((((class color) (background light)) (:foreground "darkseagreen"))
        (((class color) (background dark)) (:foreground "darkcyan")))
  "Face used to highlight end-of-line markers when font-lock mode is on.
Used only when `jaspace-alternate-eol-string' is defined (non-nil)."
  :group 'jaspace)

(defface jaspace-highlight-tab-face
  (if (<= 21 emacs-major-version)
          '((((class color) (background light))
                 (:strike-through t :foreground "gray80"))
                (((class color) (background dark))
                 (:strike-through t :foreground "gray20")))
        '((((class color) (background light)) (:background "gray90"))
          (((class color) (background dark)) (:background "gray10"))))
  "Face used to highlight tab characters when font-lock mode is on."
  :group 'jaspace)

(defcustom jaspace-mode-string " JaSp"
  "String displayed on the mode-line to when jaspase-mode is on."
  :group 'jaspace
  :type 'string)

(defcustom jaspace-follow-font-lock t
  "Non-nil means jaspace-mode follows the font-lock mode, i.e., will be
automatically turned off when font-lock mode is disabled."
  :group 'jaspace
  :type 'boolean)

(defcustom jaspace-disable-on-read-only-buffers nil
  "Non-nil means disable jaspace-mode when the buffer is or has become
read-only."
  :group 'jaspace
  :type 'boolean)

(defcustom jaspace-mode-hook nil
  "Hook to run after jaspace-mode is activated."
   :group 'jaspace
   :type 'hook)

(defcustom jaspace-modes
  (if (boundp 'whitespace-modes)
          (append whitespace-modes (list 'lisp-interaction-mode))
        '(asm-mode awk-mode autoconf-mode c-mode c++-mode cc-mode change-log-mode
          cperl-mode emacs-lisp-mode java-mode html-mode lisp-mode
          lisp-interaction-mode m4-mode makefile-mode objc-mode pascal-mode
          perl-mode sh-mode shell-script-mode sgml-mode xml-mode))
  "List of major mode symbols to enable jaspace-mode automatically."
  :group 'jaspace
  :type '(repeat (symbol :tag "Major Mode")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants and local variables

(defconst jaspace-font-lock-keywords
  '(("¡¡" 0 'jaspace-highlight-jaspace-face prepend)
        ("\n" 0 'jaspace-highlight-eol-face prepend)
        ("\t" 0 'jaspace-highlight-tab-face prepend)
        ("\t$" 0 (if (and (boundp 'show-trailing-whitespace)
                                          show-trailing-whitespace)
                                 'trailing-whitespace
                           'jaspace-highlight-tab-face) prepend)))

(defvar jaspace-mode nil
  "Non-nil means `jaspace-mode' is currently enabled.
Do not set this variable directly, use the command
`jaspace-mode'(, -on, -off) instead.")
(defvar jaspace-base-mode -1)
(defvar jaspace-ignore-command nil)

(make-variable-buffer-local 'jaspace-mode)
(make-variable-buffer-local 'jaspace-base-mode)
(make-variable-buffer-local 'jaspace-ignore-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands

(defun jaspace-mode (&optional arg)
  "Toggle jaspace-mode.
With prefix argument ARG, turn jaspace-mode on if ARG is positive,
othrewise off."
  (interactive "P")
  (if arg (if (< 0 (prefix-numeric-value arg)) (jaspace-mode-on)
                        (jaspace-mode-off))
        (if jaspace-mode (jaspace-mode-off)
          (jaspace-mode-on))))

(defun jaspace-mode-on ()
  "Force jaspace-mode on."
  (interactive)
  (unless jaspace-mode
        (jaspace-mode-enter)
        (setq jaspace-base-mode 1)
        (let ((jaspace-ignore-command t))
          (run-hooks 'jaspace-mode-hook)
          (if (interactive-p) (message "jaspace-mode is on.")
                t))))

(defun jaspace-mode-off ()
  "Force jaspace-mode off."
  (interactive)
  (when jaspace-mode
        (jaspace-mode-quit)
        (setq jaspace-base-mode 0)
        (if (interactive-p) (message "jaspace-mode is off.")
          t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(defun jaspace-font-lock-keywords ()
  (let ((keywords (list (assoc "¡¡" jaspace-font-lock-keywords))))
        (if (and jaspace-alternate-eol-string
                         (string-lessp "" jaspace-alternate-eol-string))
                (setq keywords
                          (append keywords
                                          (list (assoc "\n" jaspace-font-lock-keywords)))))
        (if jaspace-highlight-tabs
                (setq keywords
                          (append keywords
                                          (list (assoc "\t" jaspace-font-lock-keywords)
                                                        (assoc "\t$" jaspace-font-lock-keywords)))))
        keywords))

(defun jaspace-mode-enter ()
  (let ((jaspace-ignore-command t))
        (if (null buffer-display-table)
                (setq buffer-display-table (copy-sequence standard-display-table)))
        ;; make Japanese whitespace visible.
        (if (and jaspace-alternate-jaspace-string
                         (string-lessp "" jaspace-alternate-jaspace-string))
                (aset buffer-display-table
                          (string-to-char "¡¡")
                          (vconcat jaspace-alternate-jaspace-string)))
        ;; set end-of-line marker if necessary.
        (if (and jaspace-alternate-eol-string
                         (string-lessp "" jaspace-alternate-eol-string))
                (aset buffer-display-table
                          (string-to-char "\n")
                          (vconcat jaspace-alternate-eol-string)))
        ;; update font-lock keywords.
        (font-lock-add-keywords nil (jaspace-font-lock-keywords))
        (if font-lock-mode (font-lock-fontify-buffer))
        (force-mode-line-update)
        (setq jaspace-mode t)))

(defun jaspace-mode-quit ()
  (let ((jaspace-ignore-command t))
        (aset buffer-display-table (string-to-char "¡¡") nil)
        (when (and jaspace-alternate-eol-string
                           (string-lessp "" jaspace-alternate-eol-string))
          (aset buffer-display-table (string-to-char "\n") nil))
        (font-lock-remove-keywords nil jaspace-font-lock-keywords)
        (if font-lock-mode (font-lock-fontify-buffer))
        (force-mode-line-update)
        (setq jaspace-mode nil)))

(defun jaspace-update ()
  (if (< jaspace-base-mode 0)
          (setq jaspace-base-mode (if (memq major-mode jaspace-modes) 1 0)))
  (cond ((and buffer-read-only jaspace-disable-on-read-only-buffers)
                 (if jaspace-mode (jaspace-mode-quit)))
                (jaspace-follow-font-lock
                 (when (< 0 jaspace-base-mode)
                   (if font-lock-mode
                           (or jaspace-mode (jaspace-mode-enter))
                         (if jaspace-mode (jaspace-mode-quit)))))
                ((and (< 0 jaspace-base-mode) (null jaspace-mode))
                 (jaspace-mode-enter))
                ((and (= 0 jaspace-base-mode) jaspace-mode)
                 (jaspace-mode-quit))))

(defun jaspace-post-command-hook ()
  (or jaspace-ignore-command (jaspace-update)))

;; Modify by zhoujd 20080814 begin
;;(add-hook 'post-command-hook 'jaspace-post-command-hook t)
;; Modify by zhoujd 20080814 end

;; add me to the list of minor modes.
(if (boundp 'minor-mode-alist)
        (add-to-list 'minor-mode-alist '(jaspace-mode jaspace-mode-string)))
(if (boundp 'mode-line-mode-menu)
        (define-key mode-line-mode-menu [jaspace-mode]
          '(menu-item "Japanese Whitespaces" jaspace-mode
                                  :button (:toggle . jaspace-mode))))

;; mmm-mode support.
(defadvice mmm-update-mode-info
        (around jaspace-mmm-update-mode-info (mode &optional force) activate)
  (let ((font-lock-keywords-alist font-lock-keywords-alist))
        (or jaspace-mode (jaspace-update))
        (if jaspace-mode
                (font-lock-add-keywords mode (jaspace-font-lock-keywords)))
        ad-do-it))

(provide 'jaspace)
