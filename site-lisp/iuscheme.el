;;; iuscheme.el --- Scheme support, Indiana Univeristy style
;;; 
;;; Originally by Chris Haynes <chaynes@indiana.edu>, 
;;; massively edited by Erik Hilsdale <ehilsdal@indiana.edu>

;;; I've removed the CPS-specific indentation stuff into the file
;;; c311indent.el .  The CPS indentation menu option does a
;;; more-than-adequate job, but it's less than perfect.  It makes up
;;; for the imperfection by being completely standard. -erik

(require 'cmuscheme)

;; -------- Key redefinitions ----

;; CMU likes RET to go to the beginning of the line, and LFD to to a
;; return and an indent. IU tends to like it the other way around.

(define-key scheme-mode-map "\n" 'newline)
(define-key scheme-mode-map "\r" 'newline-and-indent)

;; We also want something like this when running a Scheme process.

(define-key inferior-scheme-mode-map "\n" 'newline)
(define-key inferior-scheme-mode-map "\r" 'scheme-return)

;; -------- Making return work --------

;; The problem with comint mode is that it's extremely line-based.
;; Scheme is not so line based.  This fixes the problem: a Scheme
;; expression is only sent to the Scheme process when it is full and
;; balanced.

(defun scheme-return ()
  "Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents."
  (interactive)
  (let ((input-start (process-mark (get-buffer-process (current-buffer)))))
    (if (< (point) input-start)
	(comint-send-input)		; this does magic stuff
      (let ((state (save-excursion
		     (parse-partial-sexp input-start (point)))))
	(if (and (< (car state) 1)	; depth in parens is zero
		 (not (nth 3 state))	; not in a string
		 (not (save-excursion	; nothing after the point
			(search-forward-regexp "[^ \t\n\r]" nil t))))
	    (comint-send-input)		; then go for it.
	  (newline-and-indent))))))

;;; -------- Indenting definitions ----

;; if TAB indents lines, it might make sense for C-c TAB to indent
;; definitions.  Or maybe not, but here it is anyway.

(defun scheme-indent-definition ()
  "Fix indentation of the current definition."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (scheme-indent-sexp)))

(define-key scheme-mode-map "\C-c\C-i" 'scheme-indent-definition)
(define-key inferior-scheme-mode-map "\C-c\C-i" 'scheme-indent-definition)

;;; -------- Menus ----------

(require 'easymenu)

(defvar scheme-menu-value
  '("Scheme"
   ("Indentation"
    ["Indent as normal"		(setq scheme-indent-offset nil)
     scheme-indent-offset]
    ["Indent for CPS"		(setq scheme-indent-offset 2)
     (not scheme-indent-offset)])
   "--"
   ["Forward Expression"	forward-sexp t]
   ["Backward Expression"	backward-sexp t]
   ["Kill Expression"		kill-sexp t]
   ["Transpose Expressions"	transpose-sexps t]
   ["Re-indent Expression"	scheme-indent-sexp t]
   "--"
   ["Re-indent Definition"	scheme-indent-definition t]
   ["Send Definition"		scheme-send-definition (running-schemep)]
   "--"
   ["Run Scheme"		run-scheme (not (running-schemep))])
  "The initial Scheme-mode menu, as a list")

(defvar inferior-scheme-menu-value
  '("Scheme"
    ("Indentation"
     ["Indent as normal"	(setq scheme-indent-offset nil)
      scheme-indent-offset]
     ["Indent for CPS"		(setq scheme-indent-offset 2)
      (not scheme-indent-offset)])
    "--"
    ["Forward Expression"	forward-sexp t]
    ["Backward Expression"	backward-sexp t]
    ["Kill Expression"		kill-sexp t]
    ["Transpose Expressions"	transpose-sexps t]
    ["Re-indent Expression"	scheme-indent-sexp t]
    "--"
    ["Load file"		scheme-load-file (running-schemep)]
    "--"
    ["Restart Scheme"		run-scheme (not (running-schemep))])
  "The initial inferior-Scheme-mode menu, as a list")

(if (string-match "XEmacs\\|Lucid" emacs-version)
    (progn
      (add-hook 'scheme-mode-hook
		(function
		 (lambda ()
		   (easy-menu-add scheme-menu-value))))
      (add-hook 'inferior-scheme-mode-hook
		(function
		 (lambda ()
		   (easy-menu-add inferior-scheme-menu-value)))))
  (easy-menu-define scheme-mode-menu scheme-mode-map
		    "Menu for Scheme mode"
		    scheme-menu-value)
  (easy-menu-define inferior-scheme-mode-menu inferior-scheme-mode-map
		    "Menu for Scheme mode"
		    inferior-scheme-menu-value))

(defun scheme-set-cps-indent (flag)
  "If passed t, sets scheme-indent-offset to 2.  If passed nil, sets
scheme-indent-offset to nil"
  (setq scheme-indent-offset (if flag 2 nil)))
		  
(defun running-schemep () 
  "This procedure should return t if there is a running Scheme process
somewhere, nil otherwise.  It can be confused if you use multiple
Scheme processes, but nobody really does."
  (and scheme-buffer (comint-check-proc scheme-buffer)))
  
;;; ---- end iuscheme.el

(provide 'iuscheme)
