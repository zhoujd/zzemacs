;;; python-mode.el --- Major mode for editing Python programs

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 2003-2006 http://sf.net/projects/python-mode
;;         1995-2002 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Maintainer: python-mode@python.org
;; Created:    Feb 1992
;; Keywords:   python languages oop

(defconst py-version "$Revision: 4.78 $"
  "`python-mode' version number.")

;; This software is provided as-is, without express or implied
;; warranty.  Permission to use, copy, modify, distribute or sell this
;; software, without fee, for any purpose and by any individual or
;; organization, is hereby granted, provided that the above copyright
;; notice and this paragraph appear in all copies.

;;; Commentary:

;; This is a major mode for editing Python programs.  It was developed by Tim
;; Peters after an original idea by Michael A. Guravage.  Tim subsequently
;; left the net and in 1995, Barry Warsaw inherited the mode.  Tim's now back
;; but disavows all responsibility for the mode.  In fact, we suspect he
;; doesn't even use Emacs any more.  In 2003, python-mode.el was moved to its
;; own SourceForge project apart from the Python project, and now is
;; maintained by the volunteers at the python-mode@python.org mailing list.

;; pdbtrack support contributed by Ken Manheimer, April 2001.  Skip Montanaro
;; has also contributed significantly to python-mode's development.

;; Please use the SourceForge Python project to submit bugs or
;; patches:
;;
;;     http://sourceforge.net/projects/python

;; INSTALLATION:

;; To install, just drop this file into a directory on your load-path and
;; byte-compile it.  To set up Emacs to automatically edit files ending in
;; ".py" using python-mode add the following to your ~/.emacs file (GNU
;; Emacs) or ~/.xemacs/init.el file (XEmacs):
;;    (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;;    (setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                       interpreter-mode-alist))
;;    (autoload 'python-mode "python-mode" "Python editing mode." t)
;;
;; In XEmacs syntax highlighting should be enabled automatically.  In GNU
;; Emacs you may have to add these lines to your ~/.emacs file:
;;    (global-font-lock-mode t)
;;    (setq font-lock-maximum-decoration t)

;; FOR MORE INFORMATION:

;; There is some information on python-mode.el at

;;     http://www.python.org/emacs/python-mode/
;;
;; It does contain links to other packages that you might find useful,
;; such as pdb interfaces, OO-Browser links, etc.

;; BUG REPORTING:

;; As mentioned above, please use the SourceForge Python project for
;; submitting bug reports or patches.  The old recommendation, to use
;; C-c C-b will still work, but those reports have a higher chance of
;; getting buried in my mailbox.  Please include a complete, but
;; concise code sample and a recipe for reproducing the bug.  Send
;; suggestions and other comments to python-mode@python.org.

;; When in a Python mode buffer, do a C-h m for more help.  It's
;; doubtful that a texinfo manual would be very useful, but if you
;; want to contribute one, I'll certainly accept it!

;;; Code:

(require 'comint)
(require 'custom)
(require 'cl)
(require 'compile)
(require 'ansi-color)


;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defgroup python nil
  "Support for the Python programming language, <http://www.python.org/>"
  :group 'languages
  :prefix "py-")

(defcustom py-tab-always-indent t
  "*Non-nil means TAB in Python mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
  :type 'boolean
  :group 'python)

(defcustom py-python-command "python"
  "*Shell command used to start Python interpreter."
  :type 'string
  :group 'python)

(make-obsolete-variable 'py-jpython-command 'py-jython-command)
(defcustom py-jython-command "jython"
  "*Shell command used to start the Jython interpreter."
  :type 'string
  :group 'python
  :tag "Jython Command")

(defcustom py-default-interpreter 'cpython
  "*Which Python interpreter is used by default.
The value for this variable can be either `cpython' or `jython'.

When the value is `cpython', the variables `py-python-command' and
`py-python-command-args' are consulted to determine the interpreter
and arguments to use.

When the value is `jython', the variables `py-jython-command' and
`py-jython-command-args' are consulted to determine the interpreter
and arguments to use.

Note that this variable is consulted only the first time that a Python
mode buffer is visited during an Emacs session.  After that, use
\\[py-toggle-shells] to change the interpreter shell."
  :type '(choice (const :tag "Python (a.k.a. CPython)" cpython)
		 (const :tag "Jython" jython))
  :group 'python)

(defcustom py-python-command-args '("-i")
  "*List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python)

(make-obsolete-variable 'py-jpython-command-args 'py-jython-command-args)
(defcustom py-jython-command-args '("-i")
  "*List of string arguments to be used when starting a Jython shell."
  :type '(repeat string)
  :group 'python
  :tag "Jython Command Args")

(defcustom py-indent-offset 4
  "*Amount of offset per level of indentation.
`\\[py-guess-indent-offset]' can usually guess a good value when
you're editing someone else's Python code."
  :type 'integer
  :group 'python)

(defcustom py-continuation-offset 4
  "*Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a backslash
terminated line.  Only those continuation lines for a block opening
statement are given this extra offset."
  :type 'integer
  :group 'python)

(defcustom py-smart-indentation t
  "*Should `python-mode' try to automagically set some indentation variables?
When this variable is non-nil, two things happen when a buffer is set
to `python-mode':

    1. `py-indent-offset' is guessed from existing code in the buffer.
       Only guessed values between 2 and 8 are considered.  If a valid
       guess can't be made (perhaps because you are visiting a new
       file), then the value in `py-indent-offset' is used.

    2. `indent-tabs-mode' is turned off if `py-indent-offset' does not
       equal `tab-width' (`indent-tabs-mode' is never turned on by
       Python mode).  This means that for newly written code, tabs are
       only inserted in indentation if one tab is one indentation
       level, otherwise only spaces are used.

Note that both these settings occur *after* `python-mode-hook' is run,
so if you want to defeat the automagic configuration, you must also
set `py-smart-indentation' to nil in your `python-mode-hook'."
  :type 'boolean
  :group 'python)

(defcustom py-align-multiline-strings-p t
  "*Flag describing how multi-line triple quoted strings are aligned.
When this flag is non-nil, continuation lines are lined up under the
preceding line's indentation.  When this flag is nil, continuation
lines are aligned to column zero."
  :type '(choice (const :tag "Align under preceding line" t)
		 (const :tag "Align to column zero" nil))
  :group 'python)

(defcustom py-block-comment-prefix "##"
  "*String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
`...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'python)

(defcustom py-honor-comment-indentation t
  "*Controls how comment lines influence subsequent indentation.

When nil, all comment lines are skipped for indentation purposes, and
if possible, a faster algorithm is used (i.e. X/Emacs 19 and beyond).

When t, lines that begin with a single `#' are a hint to subsequent
line indentation.  If the previous line is such a comment line (as
opposed to one that starts with `py-block-comment-prefix'), then its
indentation is used as a hint for this line's indentation.  Lines that
begin with `py-block-comment-prefix' are ignored for indentation
purposes.

When not nil or t, comment lines that begin with a single `#' are used
as indentation hints, unless the comment character is in column zero."
  :type '(choice
	  (const :tag "Skip all comment lines (fast)" nil)
	  (const :tag "Single # `sets' indentation for next line" t)
	  (const :tag "Single # `sets' indentation except at column zero"
		 other)
	  )
  :group 'python)

(defcustom py-temp-directory
  (let ((ok '(lambda (x)
	       (and x
		    (setq x (expand-file-name x)) ; always true
		    (file-directory-p x)
		    (file-writable-p x)
		    x))))
    (or (funcall ok (getenv "TMPDIR"))
	(funcall ok "/usr/tmp")
	(funcall ok "/tmp")
	(funcall ok "/var/tmp")
	(funcall ok  ".")
	(error
	 "Couldn't find a usable temp directory -- set `py-temp-directory'")))
  "*Directory used for temporary files created by a *Python* process.
By default, the first directory from this list that exists and that you
can write into: the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, /var/tmp, or the current directory."
  :type 'string
  :group 'python)

(defcustom py-beep-if-tab-change t
  "*Ring the bell if `tab-width' is changed.
If a comment of the form

  \t# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `py-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning."
  :type 'boolean
  :group 'python)

(defcustom py-jump-on-exception t
  "*Jump to innermost exception frame in *Python Output* buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame."
  :type 'boolean
  :group 'python)

(defcustom py-ask-about-save t
  "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking."
  :type 'boolean
  :group 'python)

(defcustom py-backspace-function 'backward-delete-char-untabify
  "*Function called by `py-electric-backspace' when deleting backwards."
  :type 'function
  :group 'python)

(defcustom py-delete-function 'delete-char
  "*Function called by `py-electric-delete' when deleting forwards."
  :type 'function
  :group 'python)

(defcustom py-imenu-show-method-args-p nil
  "*Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed."
  :type 'boolean
  :group 'python)
(make-variable-buffer-local 'py-indent-offset)

(defcustom py-pdbtrack-do-tracking-p t
  "*Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the *Python* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb."
  :type 'boolean
  :group 'python)
(make-variable-buffer-local 'py-pdbtrack-do-tracking-p)

(defcustom py-pdbtrack-minor-mode-string " PDB"
  "*String to use in the minor mode list when pdbtrack is enabled."
  :type 'string
  :group 'python)

(defcustom py-import-check-point-max
  20000
  "Maximum number of characters to search for a Java-ish import statement.
When `python-mode' tries to calculate the shell to use (either a
CPython or a Jython shell), it looks at the so-called `shebang' line
-- i.e. #! line.  If that's not available, it looks at some of the
file heading imports to see if they look Java-like."
  :type 'integer
  :group 'python
  )

(make-obsolete-variable 'py-jpython-packages 'py-jython-packages)
(defcustom py-jython-packages
  '("java" "javax" "org" "com")
  "Imported packages that imply `jython-mode'."
  :type '(repeat string)
  :group 'python)

;; Not customizable
(defvar py-master-file nil
  "If non-nil, execute the named file instead of the buffer's file.
The intent is to allow you to set this variable in the file's local
variable section, e.g.:

    # Local Variables:
    # py-master-file: \"master.py\"
    # End:

so that typing \\[py-execute-buffer] in that buffer executes the named
master file instead of the buffer's file.  If the file name has a
relative path, the value of variable `default-directory' for the
buffer is prepended to come up with a file name.")
(make-variable-buffer-local 'py-master-file)

(defcustom py-pychecker-command "pychecker"
  "*Shell command used to run Pychecker."
  :type 'string
  :group 'python
  :tag "Pychecker Command")

(defcustom py-pychecker-command-args '("--stdlib")
  "*List of string arguments to be passed to pychecker."
  :type '(repeat string)
  :group 'python
  :tag "Pychecker Command Args")

(defvar py-shell-alist
  '(("jython" . 'jython)
    ("python" . 'cpython))
  "*Alist of interpreters and python shells. Used by `py-choose-shell'
to select the appropriate python interpreter mode for a file.")

(defcustom py-shell-input-prompt-1-regexp "^>>> "
  "*A regular expression to match the input prompt of the shell."
  :type 'string
  :group 'python)

(defcustom py-shell-input-prompt-2-regexp "^[.][.][.] "
  "*A regular expression to match the input prompt of the shell after the
  first line of input."
  :type 'string
  :group 'python)

(defcustom py-shell-switch-buffers-on-execute t
  "*Controls switching to the Python buffer where commands are
  executed.  When non-nil the buffer switches to the Python buffer, if
  not no switching occurs."
  :type 'boolean
  :group 'python)


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defvar py-line-number-offset 0
  "When an exception occurs as a result of py-execute-region, a
subsequent py-up-exception needs the line number where the region
started, in order to jump to the correct file line.  This variable is
set in py-execute-region and used in py-jump-to-exception.")

(defconst py-emacs-features
  (let (features)
   features)
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, with different levels of
support for features needed by `python-mode'.")

;; Face for None, True, False, self, and Ellipsis
(defvar py-pseudo-keyword-face 'py-pseudo-keyword-face
  "Face for pseudo keywords in Python mode, like self, True, False, Ellipsis.")
(make-face 'py-pseudo-keyword-face)

;; PEP 318 decorators
(defvar py-decorators-face 'py-decorators-face
  "Face method decorators.")
(make-face 'py-decorators-face)

;; Face for builtins
(defvar py-builtins-face 'py-builtins-face
  "Face for builtins like TypeError, object, open, and exec.")
(make-face 'py-builtins-face)

;; XXX, TODO, and FIXME comments and such
(defvar py-XXX-tag-face 'py-XXX-tag-face
  "Face for XXX, TODO, and FIXME tags")
(make-face 'py-XXX-tag-face)

(defun py-font-lock-mode-hook ()
  (or (face-differs-from-default-p 'py-pseudo-keyword-face)
      (copy-face 'font-lock-keyword-face 'py-pseudo-keyword-face))
  (or (face-differs-from-default-p 'py-builtins-face)
      (copy-face 'font-lock-keyword-face 'py-builtins-face))
  (or (face-differs-from-default-p 'py-decorators-face)
      (copy-face 'py-pseudo-keyword-face 'py-decorators-face))
  (or (face-differs-from-default-p 'py-XXX-tag-face)
      (copy-face 'font-lock-comment-face 'py-XXX-tag-face))
  )
(add-hook 'font-lock-mode-hook 'py-font-lock-mode-hook)

(defvar python-font-lock-keywords
  (let ((kw1 (mapconcat 'identity
			'("and"      "assert"   "break"   "class"
			  "continue" "def"      "del"     "elif"
			  "else"     "except"   "exec"    "for"
			  "from"     "global"   "if"      "import"
			  "in"       "is"       "lambda"  "not"
			  "or"       "pass"     "print"   "raise"
			  "return"   "while"    "yield"
			  )
			"\\|"))
	(kw2 (mapconcat 'identity
			'("else:" "except:" "finally:" "try:")
			"\\|"))
	(kw3 (mapconcat 'identity
			;; Don't include True, False, None, or
			;; Ellipsis in this list, since they are
			;; already defined as pseudo keywords.
			'("__debug__"
			  "__import__" "__name__" "abs" "apply" "basestring"
			  "bool" "buffer" "callable" "chr" "classmethod"
			  "cmp" "coerce" "compile" "complex" "copyright"
			  "delattr" "dict" "dir" "divmod"
			  "enumerate" "eval" "execfile" "exit" "file"
			  "filter" "float" "getattr" "globals" "hasattr"
			  "hash" "hex" "id" "input" "int" "intern"
			  "isinstance" "issubclass" "iter" "len" "license"
			  "list" "locals" "long" "map" "max" "min" "object"
			  "oct" "open" "ord" "pow" "property" "range"
			  "raw_input" "reduce" "reload" "repr" "round"
			  "setattr" "slice" "staticmethod" "str" "sum"
			  "super" "tuple" "type" "unichr" "unicode" "vars"
			  "xrange" "zip")
			"\\|"))
	(kw4 (mapconcat 'identity
			;; Exceptions and warnings
			'("ArithmeticError" "AssertionError"
			  "AttributeError" "DeprecationWarning" "EOFError"
			  "EnvironmentError" "Exception"
			  "FloatingPointError" "FutureWarning" "IOError"
			  "ImportError" "IndentationError" "IndexError"
			  "KeyError" "KeyboardInterrupt" "LookupError"
			  "MemoryError" "NameError" "NotImplemented"
			  "NotImplementedError" "OSError" "OverflowError"
			  "OverflowWarning" "PendingDeprecationWarning"
			  "ReferenceError" "RuntimeError" "RuntimeWarning"
			  "StandardError" "StopIteration" "SyntaxError"
			  "SyntaxWarning" "SystemError" "SystemExit"
			  "TabError" "TypeError" "UnboundLocalError"
			  "UnicodeDecodeError" "UnicodeEncodeError"
			  "UnicodeError" "UnicodeTranslateError"
			  "UserWarning" "ValueError" "Warning"
			  "ZeroDivisionError")
			"\\|"))
	)
    (list
     '("^[ \t]*\\(@.+\\)" 1 'py-decorators-face)
     ;; keywords
     (cons (concat "\\<\\(" kw1 "\\)\\>[ \n\t(]") 1)
     ;; builtins when they don't appear as object attributes
     (list (concat "\\([^. \t]\\|^\\)[ \t]*\\<\\(" kw3 "\\)\\>[ \n\t(]") 2
	   'py-builtins-face)
     ;; block introducing keywords with immediately following colons.
     ;; Yes "except" is in both lists.
     (cons (concat "\\<\\(" kw2 "\\)[ \n\t(]") 1)
     ;; Exceptions
     (list (concat "\\<\\(" kw4 "\\)[ \n\t:,(]") 1 'py-builtins-face)
     ;; `as' but only in "import foo as bar"
     '("[ \t]*\\(\\<from\\>.*\\)?\\<import\\>.*\\<\\(as\\)\\>" . 2)

     ;; classes
     '("\\<class[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)" 1 font-lock-type-face)
     ;; functions
     '("\\<def[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     ;; pseudo-keywords
     '("\\<\\(self\\|None\\|True\\|False\\|Ellipsis\\)\\>"
       1 py-pseudo-keyword-face)
     ;; XXX, TODO, and FIXME tags
     '("XXX\\|TODO\\|FIXME" 0 py-XXX-tag-face t)
     ))
  "Additional expressions to highlight in Python mode.")
(put 'python-mode 'font-lock-defaults '(python-font-lock-keywords))

;; have to bind py-file-queue before installing the kill-emacs-hook
(defvar py-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar py-pdbtrack-is-tracking-p nil)

(defvar py-pychecker-history nil)



;; Constants

(defconst py-stringlit-re
  (concat
   ;; These fail if backslash-quote ends the string (not worth
   ;; fixing?).  They precede the short versions so that the first two
   ;; quotes don't look like an empty short string.
   ;;
   ;; (maybe raw), long single quoted triple quoted strings (SQTQ),
   ;; with potential embedded single quotes
   "[rR]?'''[^']*\\(\\('[^']\\|''[^']\\)[^']*\\)*'''"
   "\\|"
   ;; (maybe raw), long double quoted triple quoted strings (DQTQ),
   ;; with potential embedded double quotes
   "[rR]?\"\"\"[^\"]*\\(\\(\"[^\"]\\|\"\"[^\"]\\)[^\"]*\\)*\"\"\""
   "\\|"
   "[rR]?'\\([^'\n\\]\\|\\\\.\\)*'"	; single-quoted
   "\\|"				; or
   "[rR]?\"\\([^\"\n\\]\\|\\\\.\\)*\""	; double-quoted
   )
  "Regular expression matching a Python string literal.")

(defconst py-continued-re
  ;; This is tricky because a trailing backslash does not mean
  ;; continuation if it's in a comment
  (concat
   "\\(" "[^#'\"\n\\]" "\\|" py-stringlit-re "\\)*"
   "\\\\$")
  "Regular expression matching Python backslash continuation lines.")

(defconst py-blank-or-comment-re "[ \t]*\\($\\|#\\)"
  "Regular expression matching a blank or comment line.")

(defconst py-outdent-re
  (concat "\\(" (mapconcat 'identity
			   '("else:"
			     "except\\(\\s +.*\\)?:"
			     "finally:"
			     "elif\\s +.*:")
			   "\\|")
	  "\\)")
  "Regular expression matching statements to be dedented one level.")

(defconst py-block-closing-keywords-re
  "\\(return\\|raise\\|break\\|continue\\|pass\\)"
  "Regular expression matching keywords which typically close a block.")

(defconst py-no-outdent-re
  (concat
   "\\("
   (mapconcat 'identity
	      (list "try:"
		    "except\\(\\s +.*\\)?:"
		    "while\\s +.*:"
		    "for\\s +.*:"
		    "if\\s +.*:"
		    "elif\\s +.*:"
		    (concat py-block-closing-keywords-re "[ \t\n]")
		    )
	      "\\|")
	  "\\)")
  "Regular expression matching lines not to dedent after.")

(defvar py-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")

;; pdbtrack constants
(defconst py-pdbtrack-stack-entry-regexp
;  "^> \\([^(]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_]+\\)()"
  "^> \\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_]+\\)()"
  "Regular expression pdbtrack uses to find a stack trace entry.")

(defconst py-pdbtrack-input-prompt "\n[(<]*[Pp]db[>)]+ "
  "Regular expression pdbtrack uses to recognize a pdb prompt.")

(defconst py-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")



;; Major mode boilerplate

;; define a mode-specific abbrev table for those who use such things
(defvar python-mode-abbrev-table nil
  "Abbrev table in use in `python-mode' buffers.")
(define-abbrev-table 'python-mode-abbrev-table nil)

(defvar python-mode-hook nil
  "*Hook called by `python-mode'.")

(make-obsolete-variable 'jpython-mode-hook 'jython-mode-hook)
(defvar jython-mode-hook nil
  "*Hook called by `jython-mode'. `jython-mode' also calls
`python-mode-hook'.")

(defvar py-shell-hook nil
  "*Hook called by `py-shell'.")

;; In previous version of python-mode.el, the hook was incorrectly
;; called py-mode-hook, and was not defvar'd.  Deprecate its use.
(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'py-mode-hook 'python-mode-hook))

(defvar py-mode-map ()
  "Keymap used in `python-mode' buffers.")
(if py-mode-map
    nil
  (setq py-mode-map (make-sparse-keymap))
  ;; electric keys
  (define-key py-mode-map ":" 'py-electric-colon)
  ;; indentation level modifiers
  (define-key py-mode-map "\C-c\C-l"  'py-shift-region-left)
  (define-key py-mode-map "\C-c\C-r"  'py-shift-region-right)
  (define-key py-mode-map "\C-c<"     'py-shift-region-left)
  (define-key py-mode-map "\C-c>"     'py-shift-region-right)
  ;; subprocess commands
  (define-key py-mode-map "\C-c\C-c"  'py-execute-buffer)
  (define-key py-mode-map "\C-c\C-m"  'py-execute-import-or-reload)
  (define-key py-mode-map "\C-c\C-s"  'py-execute-string)
  (define-key py-mode-map "\C-c|"     'py-execute-region)
  (define-key py-mode-map "\e\C-x"    'py-execute-def-or-class)
  (define-key py-mode-map "\C-c!"     'py-shell)
  (define-key py-mode-map "\C-c\C-t"  'py-toggle-shells)
  ;; Caution!  Enter here at your own risk.  We are trying to support
  ;; several behaviors and it gets disgusting. :-( This logic ripped
  ;; largely from CC Mode.
  ;;
  ;; In XEmacs 19, Emacs 19, and Emacs 20, we use this to bind
  ;; backwards deletion behavior to DEL, which both Delete and
  ;; Backspace get translated to.  There's no way to separate this
  ;; behavior in a clean way, so deal with it!  Besides, it's been
  ;; this way since the dawn of time.
  (if (not (boundp 'delete-key-deletes-forward))
      (define-key py-mode-map "\177" 'py-electric-backspace)
    ;; However, XEmacs 20 actually achieved enlightenment.  It is
    ;; possible to sanely define both backward and forward deletion
    ;; behavior under X separately (TTYs are forever beyond hope, but
    ;; who cares?  XEmacs 20 does the right thing with these too).
    (define-key py-mode-map [delete]    'py-electric-delete)
    (define-key py-mode-map [backspace] 'py-electric-backspace))
  ;; Separate M-BS from C-M-h.  The former should remain
  ;; backward-kill-word.
  (define-key py-mode-map [(control meta h)] 'py-mark-def-or-class)
  (define-key py-mode-map "\C-c\C-k"  'py-mark-block)
  ;; Miscellaneous
  (define-key py-mode-map "\C-c:"     'py-guess-indent-offset)
  (define-key py-mode-map "\C-c\t"    'py-indent-region)
  (define-key py-mode-map "\C-c\C-d"  'py-pdbtrack-toggle-stack-tracking)
  (define-key py-mode-map "\C-c\C-n"  'py-next-statement)
  (define-key py-mode-map "\C-c\C-p"  'py-previous-statement)
  (define-key py-mode-map "\C-c\C-u"  'py-goto-block-up)
  (define-key py-mode-map "\C-c#"     'py-comment-region)
  (define-key py-mode-map "\C-c?"     'py-describe-mode)
  (define-key py-mode-map "\C-c\C-h"  'py-help-at-point)
  (define-key py-mode-map "\e\C-a"    'py-beginning-of-def-or-class)
  (define-key py-mode-map "\e\C-e"    'py-end-of-def-or-class)
  (define-key py-mode-map "\C-c-"     'py-up-exception)
  (define-key py-mode-map "\C-c="     'py-down-exception)
  ;; stuff that is `standard' but doesn't interface well with
  ;; python-mode, which forces us to rebind to special commands
  (define-key py-mode-map "\C-xnd"    'py-narrow-to-defun)
  ;; information
  (define-key py-mode-map "\C-c\C-b" 'py-submit-bug-report)
  (define-key py-mode-map "\C-c\C-v" 'py-version)
  (define-key py-mode-map "\C-c\C-w" 'py-pychecker-run)
  ;; shadow global bindings for newline-and-indent w/ the py- version.
  ;; BAW - this is extremely bad form, but I'm not going to change it
  ;; for now.
  (mapcar #'(lambda (key)
	      (define-key py-mode-map key 'py-newline-and-indent))
	  (where-is-internal 'newline-and-indent))
  ;; Force RET to be py-newline-and-indent even if it didn't get
  ;; mapped by the above code.  motivation: Emacs' default binding for
  ;; RET is `newline' and C-j is `newline-and-indent'.  Most Pythoneers
  ;; expect RET to do a `py-newline-and-indent' and any Emacsers who
  ;; dislike this are probably knowledgeable enough to do a rebind.
  ;; However, we do *not* change C-j since many Emacsers have already
  ;; swapped RET and C-j and they don't want C-j bound to `newline' to
  ;; change.
  (define-key py-mode-map "\C-m" 'py-newline-and-indent)
  )

(defvar py-mode-output-map nil
  "Keymap used in *Python Output* buffers.")
(if py-mode-output-map
    nil
  (setq py-mode-output-map (make-sparse-keymap))
  (define-key py-mode-output-map [button2]  'py-mouseto-exception)
  (define-key py-mode-output-map "\C-c\C-c" 'py-goto-exception)
  ;; TBD: Disable all self-inserting keys.  This is bogus, we should
  ;; really implement this as *Python Output* buffer being read-only
  (mapcar #' (lambda (key)
	       (define-key py-mode-output-map key
		 #'(lambda () (interactive) (beep))))
	     (where-is-internal 'self-insert-command))
  )

(defvar py-shell-map nil
  "Keymap used in *Python* shell buffers.")
(if py-shell-map
    nil
  (setq py-shell-map (copy-keymap comint-mode-map))
  (define-key py-shell-map [tab]   'tab-to-tab-stop)
  (define-key py-shell-map "\C-c-" 'py-up-exception)
  (define-key py-shell-map "\C-c=" 'py-down-exception)
  )

(defvar py-mode-syntax-table nil
  "Syntax table used in `python-mode' buffers.")
(when (not py-mode-syntax-table)
  (setq py-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" py-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" py-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" py-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" py-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" py-mode-syntax-table)
  (modify-syntax-entry ?\} "){" py-mode-syntax-table)
  ;; Add operator symbols misassigned in the std table
  (modify-syntax-entry ?\$ "."  py-mode-syntax-table)
  (modify-syntax-entry ?\% "."  py-mode-syntax-table)
  (modify-syntax-entry ?\& "."  py-mode-syntax-table)
  (modify-syntax-entry ?\* "."  py-mode-syntax-table)
  (modify-syntax-entry ?\+ "."  py-mode-syntax-table)
  (modify-syntax-entry ?\- "."  py-mode-syntax-table)
  (modify-syntax-entry ?\/ "."  py-mode-syntax-table)
  (modify-syntax-entry ?\< "."  py-mode-syntax-table)
  (modify-syntax-entry ?\= "."  py-mode-syntax-table)
  (modify-syntax-entry ?\> "."  py-mode-syntax-table)
  (modify-syntax-entry ?\| "."  py-mode-syntax-table)
  ;; For historical reasons, underscore is word class instead of
  ;; symbol class.  GNU conventions say it should be symbol class, but
  ;; there's a natural conflict between what major mode authors want
  ;; and what users expect from `forward-word' and `backward-word'.
  ;; Guido and I have hashed this out and have decided to keep
  ;; underscore in word class.  If you're tempted to change it, try
  ;; binding M-f and M-b to py-forward-into-nomenclature and
  ;; py-backward-into-nomenclature instead.  This doesn't help in all
  ;; situations where you'd want the different behavior
  ;; (e.g. backward-kill-word).
  (modify-syntax-entry ?\_ "w"  py-mode-syntax-table)
  ;; Both single quote and double quote are string delimiters
  (modify-syntax-entry ?\' "\"" py-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" py-mode-syntax-table)
  ;; backquote is open and close paren
  (modify-syntax-entry ?\` "$"  py-mode-syntax-table)
  ;; comment delimiters
  (modify-syntax-entry ?\# "<"  py-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  py-mode-syntax-table)
  )

;; An auxiliary syntax table which places underscore and dot in the
;; symbol class for simplicity
(defvar py-dotted-expression-syntax-table nil
  "Syntax table used to identify Python dotted expressions.")
(when (not py-dotted-expression-syntax-table)
  (setq py-dotted-expression-syntax-table
	(copy-syntax-table py-mode-syntax-table))
  (modify-syntax-entry ?_ "_" py-dotted-expression-syntax-table)
  (modify-syntax-entry ?. "_" py-dotted-expression-syntax-table))



;; Utilities
(defmacro py-safe (&rest body)
  "Safely execute BODY, return nil if an error occurred."
  (` (condition-case nil
	 (progn (,@ body))
       (error nil))))

(defsubst py-keep-region-active ()
  "Keep the region active in XEmacs."
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs 19 does it differently; its policy doesn't require us
  ;; to take explicit action.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defsubst py-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol  -- beginning of line
  eol  -- end of line
  bod  -- beginning of def or class
  eod  -- end of def or class
  bob  -- beginning of buffer
  eob  -- end of buffer
  boi  -- back to indentation
  bos  -- beginning of statement

This function does not modify point or mark."
  (let ((here (point)))
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'eol) (end-of-line))
     ((eq position 'bod) (py-beginning-of-def-or-class 'either))
     ((eq position 'eod) (py-end-of-def-or-class 'either))
     ;; Kind of funny, I know, but useful for py-up-exception.
     ((eq position 'bob) (beginning-of-buffer))
     ((eq position 'eob) (end-of-buffer))
     ((eq position 'boi) (back-to-indentation))
     ((eq position 'bos) (py-goto-initial-line))
     (t (error "Unknown buffer position requested: %s" position))
     )
    (prog1
	(point)
      (goto-char here))))

(defsubst py-highlight-line (from to file line)
  (cond
   ((fboundp 'make-extent)
    ;; XEmacs
    (let ((e (make-extent from to)))
      (set-extent-property e 'mouse-face 'highlight)
      (set-extent-property e 'py-exc-info (cons file line))
      (set-extent-property e 'keymap py-mode-output-map)))
   (t
    ;; Emacs -- Please port this!
    )
   ))

(defun py-in-literal (&optional lim)
  "Return non-nil if point is in a Python literal (a comment or string).
Optional argument LIM indicates the beginning of the containing form,
i.e. the limit on how far back to scan."
  ;; This is the version used for non-XEmacs, which has a nicer
  ;; interface.
  ;;
  ;; WARNING: Watch out for infinite recursion.
  (let* ((lim (or lim (py-point 'bod)))
	 (state (parse-partial-sexp lim (point))))
    (cond
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment)
     (t nil))))

;; XEmacs has a built-in function that should make this much quicker.
;; In this case, lim is ignored
(defun py-fast-in-literal (&optional lim)
  "Fast version of `py-in-literal', used only by XEmacs.
Optional LIM is ignored."
  ;; don't have to worry about context == 'block-comment
  (buffer-syntactic-context))

(if (fboundp 'buffer-syntactic-context)
    (defalias 'py-in-literal 'py-fast-in-literal))



;; Menu definitions, only relevent if you have the easymenu.el package
;; (standard in the latest Emacs 19 and XEmacs 19 distributions).
(defvar py-menu nil
  "Menu for Python Mode.
This menu will get created automatically if you have the `easymenu'
package.  Note that the latest X/Emacs releases contain this package.")

(and (py-safe (require 'easymenu) t)
     (easy-menu-define
      py-menu py-mode-map "Python Mode menu"
      '("Python"
	["Comment Out Region"   py-comment-region  (mark)]
	["Uncomment Region"     (py-comment-region (point) (mark) '(4)) (mark)]
	"-"
	["Mark current block"   py-mark-block t]
	["Mark current def"     py-mark-def-or-class t]
	["Mark current class"   (py-mark-def-or-class t) t]
	"-"
	["Shift region left"    py-shift-region-left (mark)]
	["Shift region right"   py-shift-region-right (mark)]
	"-"
	["Import/reload file"   py-execute-import-or-reload t]
	["Execute buffer"       py-execute-buffer t]
	["Execute region"       py-execute-region (mark)]
	["Execute def or class" py-execute-def-or-class (mark)]
	["Execute string"       py-execute-string t]
	["Start interpreter..." py-shell t]
	"-"
	["Go to start of block" py-goto-block-up t]
	["Go to start of class" (py-beginning-of-def-or-class t) t]
	["Move to end of class" (py-end-of-def-or-class t) t]
	["Move to start of def" py-beginning-of-def-or-class t]
	["Move to end of def"   py-end-of-def-or-class t]
	"-"
	["Describe mode"        py-describe-mode t]
	)))



;; Imenu definitions
(defvar py-imenu-class-regexp
  (concat				; <<classes>>
   "\\("				;
   "^[ \t]*"				; newline and maybe whitespace
   "\\(class[ \t]+[a-zA-Z0-9_]+\\)"	; class name
					; possibly multiple superclasses
   "\\([ \t]*\\((\\([a-zA-Z0-9_,. \t\n]\\)*)\\)?\\)"
   "[ \t]*:"				; and the final :
   "\\)"				; >>classes<<
   )
  "Regexp for Python classes for use with the Imenu package."
  )

(defvar py-imenu-method-regexp
  (concat                               ; <<methods and functions>>
   "\\("                                ;
   "^[ \t]*"                            ; new line and maybe whitespace
   "\\(def[ \t]+"                       ; function definitions start with def
   "\\([a-zA-Z0-9_]+\\)"                ;   name is here
					;   function arguments...
;;   "[ \t]*(\\([-+/a-zA-Z0-9_=,\* \t\n.()\"'#]*\\))"
   "[ \t]*(\\([^:#]*\\))"
   "\\)"                                ; end of def
   "[ \t]*:"                            ; and then the :
   "\\)"                                ; >>methods and functions<<
   )
  "Regexp for Python methods/functions for use with the Imenu package."
  )

(defvar py-imenu-method-no-arg-parens '(2 8)
  "Indices into groups of the Python regexp for use with Imenu.

Using these values will result in smaller Imenu lists, as arguments to
functions are not listed.

See the variable `py-imenu-show-method-args-p' for more
information.")

(defvar py-imenu-method-arg-parens '(2 7)
  "Indices into groups of the Python regexp for use with imenu.
Using these values will result in large Imenu lists, as arguments to
functions are listed.

See the variable `py-imenu-show-method-args-p' for more
information.")

;; Note that in this format, this variable can still be used with the
;; imenu--generic-function. Otherwise, there is no real reason to have
;; it.
(defvar py-imenu-generic-expression
  (cons
   (concat
    py-imenu-class-regexp
    "\\|"				; or...
    py-imenu-method-regexp
    )
   py-imenu-method-no-arg-parens)
  "Generic Python expression which may be used directly with Imenu.
Used by setting the variable `imenu-generic-expression' to this value.
Also, see the function \\[py-imenu-create-index] for a better
alternative for finding the index.")

;; These next two variables are used when searching for the Python
;; class/definitions. Just saving some time in accessing the
;; generic-python-expression, really.
(defvar py-imenu-generic-regexp nil)
(defvar py-imenu-generic-parens nil)


(defun py-imenu-create-index-function ()
  "Python interface function for the Imenu package.
Finds all Python classes and functions/methods. Calls function
\\[py-imenu-create-index-engine].  See that function for the details
of how this works."
  (setq py-imenu-generic-regexp (car py-imenu-generic-expression)
	py-imenu-generic-parens (if py-imenu-show-method-args-p
				    py-imenu-method-arg-parens
				  py-imenu-method-no-arg-parens))
  (goto-char (point-min))
  ;; Warning: When the buffer has no classes or functions, this will
  ;; return nil, which seems proper according to the Imenu API, but
  ;; causes an error in the XEmacs port of Imenu.  Sigh.
  (py-imenu-create-index-engine nil))

(defun py-imenu-create-index-engine (&optional start-indent)
  "Function for finding Imenu definitions in Python.

Finds all definitions (classes, methods, or functions) in a Python
file for the Imenu package.

Returns a possibly nested alist of the form

	(INDEX-NAME . INDEX-POSITION)

The second element of the alist may be an alist, producing a nested
list as in

	(INDEX-NAME . INDEX-ALIST)

This function should not be called directly, as it calls itself
recursively and requires some setup.  Rather this is the engine for
the function \\[py-imenu-create-index-function].

It works recursively by looking for all definitions at the current
indention level.  When it finds one, it adds it to the alist.  If it
finds a definition at a greater indentation level, it removes the
previous definition from the alist. In its place it adds all
definitions found at the next indentation level.  When it finds a
definition that is less indented then the current level, it returns
the alist it has created thus far.

The optional argument START-INDENT indicates the starting indentation
at which to continue looking for Python classes, methods, or
functions.  If this is not supplied, the function uses the indentation
of the first definition found."
  (let (index-alist
	sub-method-alist
	looking-p
	def-name prev-name
	cur-indent def-pos
	(class-paren (first  py-imenu-generic-parens))
	(def-paren   (second py-imenu-generic-parens)))
    (setq looking-p
	  (re-search-forward py-imenu-generic-regexp (point-max) t))
    (while looking-p
      (save-excursion
	;; used to set def-name to this value but generic-extract-name
	;; is new to imenu-1.14. this way it still works with
	;; imenu-1.11
	;;(imenu--generic-extract-name py-imenu-generic-parens))
	(let ((cur-paren (if (match-beginning class-paren)
			     class-paren def-paren)))
	  (setq def-name
		(buffer-substring-no-properties (match-beginning cur-paren)
						(match-end cur-paren))))
	(save-match-data
	  (py-beginning-of-def-or-class 'either))
	(beginning-of-line)
	(setq cur-indent (current-indentation)))
      ;; HACK: want to go to the next correct definition location.  We
      ;; explicitly list them here but it would be better to have them
      ;; in a list.
      (setq def-pos
	    (or (match-beginning class-paren)
		(match-beginning def-paren)))
      ;; if we don't have a starting indent level, take this one
      (or start-indent
	  (setq start-indent cur-indent))
      ;; if we don't have class name yet, take this one
      (or prev-name
	  (setq prev-name def-name))
      ;; what level is the next definition on?  must be same, deeper
      ;; or shallower indentation
      (cond
       ;; Skip code in comments and strings
       ((py-in-literal))
       ;; at the same indent level, add it to the list...
       ((= start-indent cur-indent)
	(push (cons def-name def-pos) index-alist))
       ;; deeper indented expression, recurse
       ((< start-indent cur-indent)
	;; the point is currently on the expression we're supposed to
	;; start on, so go back to the last expression. The recursive
	;; call will find this place again and add it to the correct
	;; list
	(re-search-backward py-imenu-generic-regexp (point-min) 'move)
	(setq sub-method-alist (py-imenu-create-index-engine cur-indent))
	(if sub-method-alist
	    ;; we put the last element on the index-alist on the start
	    ;; of the submethod alist so the user can still get to it.
	    (let ((save-elmt (pop index-alist)))
	      (push (cons prev-name
			  (cons save-elmt sub-method-alist))
		    index-alist))))
       ;; found less indented expression, we're done.
       (t
	(setq looking-p nil)
	(re-search-backward py-imenu-generic-regexp (point-min) t)))
      ;; end-cond
      (setq prev-name def-name)
      (and looking-p
	   (setq looking-p
		 (re-search-forward py-imenu-generic-regexp
				    (point-max) 'move))))
    (nreverse index-alist)))



(defun py-choose-shell-by-shebang ()
  "Choose CPython or Jython mode by looking at #! on the first line.
Returns the appropriate mode function.
Used by `py-choose-shell', and similar to but distinct from
`set-auto-mode', though it uses `auto-mode-interpreter-regexp' (if available)."
  ;; look for an interpreter specified in the first line
  ;; similar to set-auto-mode (files.el)
  (let* ((re (if (boundp 'auto-mode-interpreter-regexp)
		 auto-mode-interpreter-regexp
	       ;; stolen from Emacs 21.2
	       "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)"))
	 (interpreter (save-excursion
			(goto-char (point-min))
			(if (looking-at re)
			    (match-string 2)
			  "")))
	 elt)
    ;; Map interpreter name to a mode.
    (setq elt (assoc (file-name-nondirectory interpreter)
		     py-shell-alist))
    (and elt (caddr elt))))



(defun py-choose-shell-by-import ()
  "Choose CPython or Jython mode based imports.
If a file imports any packages in `py-jython-packages', within
`py-import-check-point-max' characters from the start of the file,
return `jython', otherwise return nil."
  (let (mode)
    (save-excursion
      (goto-char (point-min))
      (while (and (not mode)
		  (search-forward-regexp
		   "^\\(\\(from\\)\\|\\(import\\)\\) \\([^ \t\n.]+\\)"
		   py-import-check-point-max t))
	(setq mode (and (member (match-string 4) py-jython-packages)
			'jython
			))))
    mode))


(defun py-choose-shell ()
  "Choose CPython or Jython mode. Returns the appropriate mode function.
This does the following:
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py-choose-shell-by-import'
 - default to the variable `py-default-interpreter'"
  (interactive)
  (or (py-choose-shell-by-shebang)
      (py-choose-shell-by-import)
      py-default-interpreter
;      'cpython ;; don't use to py-default-interpreter, because default
;               ;; is only way to choose CPython
      ))


;;;###autoload
(defun python-mode ()
  "Major mode for editing Python files.
To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
\\{py-mode-map}
VARIABLES

py-indent-offset\t\tindentation increment
py-block-comment-prefix\t\tcomment string used by `comment-region'
py-python-command\t\tshell command to invoke Python interpreter
py-temp-directory\t\tdirectory used for temp files (if needed)
py-beep-if-tab-change\t\tring the bell if `tab-width' is changed"
  (interactive)
  ;; set up local variables
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'add-log-current-defun-function)
  (make-local-variable 'fill-paragraph-function)
  ;;
  (set-syntax-table py-mode-syntax-table)
  (setq major-mode              'python-mode
	mode-name               "Python"
	local-abbrev-table      python-mode-abbrev-table
	font-lock-defaults      '(python-font-lock-keywords)
	paragraph-separate      "^[ \t]*$"
	paragraph-start         "^[ \t]*$"
	require-final-newline   t
	comment-start           "# "
	comment-end             ""
	comment-start-skip      "# *"
	comment-column          40
	comment-indent-function 'py-comment-indent-function
	indent-region-function  'py-indent-region
	indent-line-function    'py-indent-line
	;; tell add-log.el how to find the current function/method/variable
	add-log-current-defun-function 'py-current-defun

	fill-paragraph-function 'py-fill-paragraph
	)
  (use-local-map py-mode-map)
  ;; add the menu
  (if py-menu
      (easy-menu-add py-menu))
  ;; Emacs 19 requires this
  (if (boundp 'comment-multi-line)
      (setq comment-multi-line nil))
  ;; Install Imenu if available
  (when (py-safe (require 'imenu))
    (setq imenu-create-index-function #'py-imenu-create-index-function)
    (setq imenu-generic-expression py-imenu-generic-expression)
    (if (fboundp 'imenu-add-to-menubar)
	(imenu-add-to-menubar (format "%s-%s" "IM" mode-name)))
    )
  ;; Run the mode hook.  Note that py-mode-hook is deprecated.
  (if python-mode-hook
      (run-hooks 'python-mode-hook)
    (run-hooks 'py-mode-hook))
  ;; Now do the automagical guessing
  (if py-smart-indentation
    (let ((offset py-indent-offset))
      ;; It's okay if this fails to guess a good value
      (if (and (py-safe (py-guess-indent-offset))
	       (<= py-indent-offset 8)
	       (>= py-indent-offset 2))
	  (setq offset py-indent-offset))
      (setq py-indent-offset offset)
      ;; Only turn indent-tabs-mode off if tab-width !=
      ;; py-indent-offset.  Never turn it on, because the user must
      ;; have explicitly turned it off.
      (if (/= tab-width py-indent-offset)
	  (setq indent-tabs-mode nil))
      ))
  ;; Set the default shell if not already set
  (when (null py-which-shell)
    (py-toggle-shells (py-choose-shell))))


(make-obsolete 'jpython-mode 'jython-mode)
(defun jython-mode ()
  "Major mode for editing Jython/Jython files.
This is a simple wrapper around `python-mode'.
It runs `jython-mode-hook' then calls `python-mode.'
It is added to `interpreter-mode-alist' and `py-choose-shell'.
"
  (interactive)
  (python-mode)
  (py-toggle-shells 'jython)
  (when jython-mode-hook
      (run-hooks 'jython-mode-hook)))


;; It's handy to add recognition of Python files to the
;; interpreter-mode-alist and to auto-mode-alist.  With the former, we
;; can specify different `derived-modes' based on the #! line, but
;; with the latter, we can't.  So we just won't add them if they're
;; already added.
;;;###autoload
(let ((modes '(("jython" . jython-mode)
	       ("python" . python-mode))))
  (while modes
    (when (not (assoc (car modes) interpreter-mode-alist))
      (push (car modes) interpreter-mode-alist))
    (setq modes (cdr modes))))
;;;###autoload
(when (not (or (rassq 'python-mode auto-mode-alist)
	       (rassq 'jython-mode auto-mode-alist)))
  (push '("\\.py$" . python-mode) auto-mode-alist))



;; electric characters
(defun py-outdent-p ()
  "Returns non-nil if the current line should dedent one level."
  (save-excursion
    (and (progn (back-to-indentation)
		(looking-at py-outdent-re))
	 ;; short circuit infloop on illegal construct
	 (not (bobp))
	 (progn (forward-line -1)
		(py-goto-initial-line)
		(back-to-indentation)
		(while (or (looking-at py-blank-or-comment-re)
			   (bobp))
		  (backward-to-indentation 1))
		(not (looking-at py-no-outdent-re)))
	 )))

(defun py-electric-colon (arg)
  "Insert a colon.
In certain cases the line is dedented appropriately.  If a numeric
argument ARG is provided, that many colons are inserted
non-electrically.  Electric behavior is inhibited inside a string or
comment."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  ;; are we in a string or comment?
  (if (save-excursion
	(let ((pps (parse-partial-sexp (save-excursion
					 (py-beginning-of-def-or-class)
					 (point))
				       (point))))
	  (not (or (nth 3 pps) (nth 4 pps)))))
      (save-excursion
	(let ((here (point))
	      (outdent 0)
	      (indent (py-compute-indentation t)))
	  (if (and (not arg)
		   (py-outdent-p)
		   (= indent (save-excursion
			       (py-next-statement -1)
			       (py-compute-indentation t)))
		   )
	      (setq outdent py-indent-offset))
	  ;; Don't indent, only dedent.  This assumes that any lines
	  ;; that are already dedented relative to
	  ;; py-compute-indentation were put there on purpose.  It's
	  ;; highly annoying to have `:' indent for you.  Use TAB, C-c
	  ;; C-l or C-c C-r to adjust.  TBD: Is there a better way to
	  ;; determine this???
	  (if (< (current-indentation) indent) nil
	    (goto-char here)
	    (beginning-of-line)
	    (delete-horizontal-space)
	    (indent-to (- indent outdent))
	    )))))


;; Python subprocess utilities and filters
(defun py-execute-file (proc filename)
  "Send to Python interpreter process PROC \"execfile('FILENAME')\".
Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing."
  (let ((curbuf (current-buffer))
	(procbuf (process-buffer proc))
;	(comint-scroll-to-bottom-on-output t)
	(msg (format "## working on region in file %s...\n" filename))
        ;; add some comment, so that we can filter it out of history
	(cmd (format "execfile(r'%s') # PYTHON-MODE\n" filename)))
    (unwind-protect
	(save-excursion
	  (set-buffer procbuf)
	  (goto-char (point-max))
	  (move-marker (process-mark proc) (point))
	  (funcall (process-filter proc) proc msg))
      (set-buffer curbuf))
    (process-send-string proc cmd)))

(defun py-comint-output-filter-function (string)
  "Watch output for Python prompt and exec next file waiting in queue.
This function is appropriate for `comint-output-filter-functions'."
  ;;remove ansi terminal escape sequences from string, not sure why they are
  ;;still around...
  (setq string (ansi-color-filter-apply string))
  (when (and (string-match py-shell-input-prompt-1-regexp string)
                   py-file-queue)
    (if py-shell-switch-buffers-on-execute
      (pop-to-buffer (current-buffer)))
    (py-safe (delete-file (car py-file-queue)))
    (setq py-file-queue (cdr py-file-queue))
    (if py-file-queue
	(let ((pyproc (get-buffer-process (current-buffer))))
	  (py-execute-file pyproc (car py-file-queue))))
    ))

(defun py-pdbtrack-overlay-arrow (activation)
  "Activate or de arrow at beginning-of-line in current buffer."
  ;; This was derived/simplified from edebug-overlay-arrow
  (cond (activation
	 (setq overlay-arrow-position (make-marker))
	 (setq overlay-arrow-string "=>")
	 (set-marker overlay-arrow-position (py-point 'bol) (current-buffer))
	 (setq py-pdbtrack-is-tracking-p t))
	(overlay-arrow-position
	 (setq overlay-arrow-position nil)
	 (setq py-pdbtrack-is-tracking-p nil))
	))

(defun py-pdbtrack-track-stack-file (text)
  "Show the file indicated by the pdb stack entry line, in a separate window.

Activity is disabled if the buffer-local variable
`py-pdbtrack-do-tracking-p' is nil.

We depend on the pdb input prompt matching `py-pdbtrack-input-prompt'
at the beginning of the line.

If the traceback target file path is invalid, we look for the most
recently visited python-mode buffer which either has the name of the
current function \(or class) or which defines the function \(or
class).  This is to provide for remote scripts, eg, Zope's 'Script
(Python)' - put a _copy_ of the script in a buffer named for the
script, and set to python-mode, and pdbtrack will find it.)"
  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next pdb prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  ;;
  ;; Also, we're very conservative about clearing the overlay arrow,
  ;; to minimize residue.  This means, for instance, that executing
  ;; other pdb commands wipe out the highlight.  You can always do a
  ;; 'where' (aka 'w') command to reveal the overlay arrow.
  (let* ((origbuf (current-buffer))
	 (currproc (get-buffer-process origbuf)))

    (if (not (and currproc py-pdbtrack-do-tracking-p))
        (py-pdbtrack-overlay-arrow nil)

      (let* ((procmark (process-mark currproc))
             (block (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              py-pdbtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match (concat py-pdbtrack-input-prompt "$") block))
            (py-pdbtrack-overlay-arrow nil)

          (setq target (py-pdbtrack-get-source-buffer block))

          (if (stringp target)
              (message "pdbtrack: %s" target)

            (setq target_lineno (car target))
            (setq target_buffer (cadr target))
            (setq target_fname (buffer-file-name target_buffer))
            (switch-to-buffer-other-window target_buffer)
            (goto-line target_lineno)
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (py-pdbtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)

            )))))
  )

(defun py-pdbtrack-get-source-buffer (block)
  "Return line number and buffer of code indicated by block's traceback text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited python-mode buffer
with the same name or having the named function.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (not (string-match py-pdbtrack-stack-entry-regexp block))

      "Traceback cue not found"

    (let* ((filename (match-string 1 block))
           (lineno (string-to-int (match-string 2 block)))
           (funcname (match-string 3 block))
           funcbuffer)

      (cond ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((setq funcbuffer (py-pdbtrack-grub-for-buffer funcname lineno))
             (if (string-match "/Script (Python)$" filename)
                 ;; Add in number of lines for leading '##' comments:
                 (setq lineno
                       (+ lineno
                          (save-excursion
                            (set-buffer funcbuffer)
                            (count-lines
                             (point-min)
                             (max (point-min)
                                  (string-match "^\\([^#]\\|#[^#]\\|#$\\)"
                                                (buffer-substring (point-min)
                                                                  (point-max)))
                                  ))))))
             (list lineno funcbuffer))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s(), %s" funcname filename)))
      )
    )
  )

(defun py-pdbtrack-grub-for-buffer (funcname lineno)
  "Find most recent buffer itself named or having function funcname.

We walk the buffer-list history for python-mode buffers that are
named for funcname or define a function funcname."
  (let ((buffers (buffer-list))
        buf
        got)
    (while (and buffers (not got))
      (setq buf (car buffers)
            buffers (cdr buffers))
      (if (and (save-excursion (set-buffer buf)
                               (string= major-mode "python-mode"))
               (or (string-match funcname (buffer-name buf))
                   (string-match (concat "^\\s-*\\(def\\|class\\)\\s-+"
                                         funcname "\\s-*(")
                                 (save-excursion
                                   (set-buffer buf)
                                   (buffer-substring (point-min)
                                                     (point-max))))))
          (setq got buf)))
    got))

(defun py-postprocess-output-buffer (buf)
  "Highlight exceptions found in BUF.
If an exception occurred return t, otherwise return nil.  BUF must exist."
  (let (line file bol err-p)
    (save-excursion
      (set-buffer buf)
      (beginning-of-buffer)
      (while (re-search-forward py-traceback-line-re nil t)
	(setq file (match-string 1)
	      line (string-to-int (match-string 2))
	      bol (py-point 'bol))
	(py-highlight-line bol (py-point 'eol) file line)))
    (when (and py-jump-on-exception line)
      (beep)
      (py-jump-to-exception file line)
      (setq err-p t))
    err-p))



;;; Subprocess commands

;; only used when (memq 'broken-temp-names py-emacs-features)
(defvar py-serial-number 0)
(defvar py-exception-buffer nil)
(defconst py-output-buffer "*Python Output*")
(make-variable-buffer-local 'py-output-buffer)

;; for toggling between CPython and Jython
(defvar py-which-shell nil)
(defvar py-which-args  py-python-command-args)
(defvar py-which-bufname "Python")
(make-variable-buffer-local 'py-which-shell)
(make-variable-buffer-local 'py-which-args)
(make-variable-buffer-local 'py-which-bufname)

(defun py-toggle-shells (arg)
  "Toggles between the CPython and Jython shells.

With positive argument ARG (interactively \\[universal-argument]),
uses the CPython shell, with negative ARG uses the Jython shell, and
with a zero argument, toggles the shell.

Programmatically, ARG can also be one of the symbols `cpython' or
`jython', equivalent to positive arg and negative arg respectively."
  (interactive "P")
  ;; default is to toggle
  (if (null arg)
      (setq arg 0))
  ;; preprocess arg
  (cond
   ((equal arg 0)
    ;; toggle
    (if (string-equal py-which-bufname "Python")
	(setq arg -1)
      (setq arg 1)))
   ((equal arg 'cpython) (setq arg 1))
   ((equal arg 'jython) (setq arg -1)))
  (let (msg)
    (cond
     ((< 0 arg)
      ;; set to CPython
      (setq py-which-shell py-python-command
	    py-which-args py-python-command-args
	    py-which-bufname "Python"
	    msg "CPython")
      (if (string-equal py-which-bufname "Jython")
	  (setq mode-name "Python")))
     ((> 0 arg)
      (setq py-which-shell py-jython-command
	    py-which-args py-jython-command-args
	    py-which-bufname "Jython"
	    msg "Jython")
      (if (string-equal py-which-bufname "Python")
	  (setq mode-name "Jython")))
     )
    (message "Using the %s shell" msg)
    (setq py-output-buffer (format "*%s Output*" py-which-bufname))))

;;;###autoload
(defun py-shell (&optional argprompt)
  "Start an interactive Python interpreter in another window.
This is like Shell mode, except that Python is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Python*' buffer.

With optional \\[universal-argument], the user is prompted for the
flags to pass to the Python interpreter.  This has no effect when this
command is used to switch to an existing process, only when a new
process is started.  If you use this, you will probably want to ensure
that the current arguments are retained (they will be included in the
prompt).  This argument is ignored when this function is called
programmatically, or when running in Emacs 19.34 or older.

Note: You can toggle between using the CPython interpreter and the
Jython interpreter by hitting \\[py-toggle-shells].  This toggles
buffer local variables which control whether all your subshell
interactions happen to the `*Jython*' or `*Python*' buffers (the
latter is the name used for the CPython buffer).

Warning: Don't use an interactive Python if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `python-mode' can't
distinguish your output from Python's output, and assumes that `>>> '
at the start of a line is a prompt from Python.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Python prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Python, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Python process buffers using the default (Emacs-supplied) process
filter."
  (interactive "P")
  ;; Set the default shell if not already set
  (when (null py-which-shell)
    (py-toggle-shells py-default-interpreter))
  (let ((args py-which-args))
    (when (and argprompt
	       (interactive-p)
	       (fboundp 'split-string))
      ;; TBD: Perhaps force "-i" in the final list?
      (setq args (split-string
		  (read-string (concat py-which-bufname
				       " arguments: ")
			       (concat
				(mapconcat 'identity py-which-args " ") " ")
			       ))))
    (if (not (equal (buffer-name) "*Python*"))
        (switch-to-buffer-other-window
         (apply 'make-comint py-which-bufname py-which-shell nil args))
      (apply 'make-comint py-which-bufname py-which-shell nil args))
    (make-local-variable 'comint-prompt-regexp)
    (setq comint-prompt-regexp (concat py-shell-input-prompt-1-regexp "\\|"
                                       py-shell-input-prompt-2-regexp "\\|"
                                       "^([Pp]db) "))
    (add-hook 'comint-output-filter-functions
	      'py-comint-output-filter-function)
    ;; pdbtrack
    (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)
    (setq py-pdbtrack-do-tracking-p t)
    (set-syntax-table py-mode-syntax-table)
    (use-local-map py-shell-map)
    (run-hooks 'py-shell-hook)
    ))

(defun py-clear-queue ()
  "Clear the queue of temporary files waiting to execute."
  (interactive)
  (let ((n (length py-file-queue)))
    (mapcar 'delete-file py-file-queue)
    (setq py-file-queue nil)
    (message "%d pending files de-queued." n)))


(defun py-execute-region (start end &optional async)
  "Execute the region in a Python interpreter.

The region is first copied into a temporary file (in the directory
`py-temp-directory').  If there is no Python interpreter shell
running, this file is executed synchronously using
`shell-command-on-region'.  If the program is long running, use
\\[universal-argument] to run the command asynchronously in its own
buffer.

When this function is used programmatically, arguments START and END
specify the region to execute, and optional third argument ASYNC, if
non-nil, specifies to run the command asynchronously in its own
buffer.

If the Python interpreter shell is running, the region is execfile()'d
in that shell.  If you try to execute regions too quickly,
`python-mode' will queue them up and execute them one at a time when
it sees a `>>> ' prompt from Python.  Each time this happens, the
process buffer is popped into a window (if it's not already in some
window) so you can see it, and a comment of the form

    \t## working on region in file <name>...

is inserted at the end.  See also the command `py-clear-queue'."
  (interactive "r\nP")
  ;; Skip ahead to the first non-blank line
  (let* ((proc (get-process py-which-bufname))
	 (temp (if (memq 'broken-temp-names py-emacs-features)
		   (let
		       ((sn py-serial-number)
			(pid (and (fboundp 'emacs-pid) (emacs-pid))))
		     (setq py-serial-number (1+ py-serial-number))
		     (if pid
			 (format "python-%d-%d" sn pid)
		       (format "python-%d" sn)))
		 (make-temp-name "python-")))
	 (file (concat (expand-file-name temp py-temp-directory) ".py"))
     ;;(message "plik tymczasowy: %s" file)
	 (cur (current-buffer))
	 (buf (get-buffer-create file))
	 shell)
    ;; Write the contents of the buffer, watching out for indented regions.
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (while (and (looking-at "\\s *$")
		  (< (point) end))
	(forward-line 1))
      (setq start (point))
      (or (< start end)
	  (error "Region is empty"))
      (setq py-line-number-offset (count-lines 1 start))
      (let ((needs-if (/= (py-point 'bol) (py-point 'boi))))
	(set-buffer buf)
	(python-mode)
	(when needs-if
	  (insert "if 1:\n")
	  (setq py-line-number-offset (- py-line-number-offset 1)))
	(insert-buffer-substring cur start end)
	;; Set the shell either to the #! line command, or to the
	;; py-which-shell buffer local variable.
	(setq shell (or (py-choose-shell-by-shebang)
			(py-choose-shell-by-import)
			py-which-shell))))
    (cond
     ;; always run the code in its own asynchronous subprocess
     (async
      ;; User explicitly wants this to run in its own async subprocess
      (save-excursion
	(set-buffer buf)
	(write-region (point-min) (point-max) file nil 'nomsg))
      (let* ((buf (generate-new-buffer-name py-output-buffer))
	     ;; TBD: a horrible hack, but why create new Custom variables?
	     (arg (if (string-equal py-which-bufname "Python")
		      "-u" "")))
	(start-process py-which-bufname buf shell arg file)
	(pop-to-buffer buf)
	(py-postprocess-output-buffer buf)
	;; TBD: clean up the temporary file!
	))
     ;; if the Python interpreter shell is running, queue it up for
     ;; execution there.
     (proc
      ;; use the existing python shell
      (save-excursion
        (set-buffer buf)
        (write-region (point-min) (point-max) file nil 'nomsg))
      (if (not py-file-queue)
          (py-execute-file proc file)
        (message "File %s queued for execution" file))
      (setq py-file-queue (append py-file-queue (list file)))
      (setq py-exception-buffer (cons file (current-buffer))))
     (t
      ;; TBD: a horrible hack, but why create new Custom variables?
      (let ((cmd (concat py-which-shell (if (string-equal py-which-bufname
							  "Jython")
					    " -" ""))))
	;; otherwise either run it synchronously in a subprocess
	(save-excursion
	  (set-buffer buf)
	  (shell-command-on-region (point-min) (point-max)
				   cmd py-output-buffer))
	;; shell-command-on-region kills the output buffer if it never
	;; existed and there's no output from the command
	(if (not (get-buffer py-output-buffer))
	    (message "No output.")
	  (setq py-exception-buffer (current-buffer))
	  (let ((err-p (py-postprocess-output-buffer py-output-buffer)))
	    (pop-to-buffer py-output-buffer)
	    (if err-p
		(pop-to-buffer py-exception-buffer)))
	  ))
      ))
    ;; Clean up after ourselves.
    (kill-buffer buf)))


;; Code execution commands
(defun py-execute-buffer (&optional async)
  "Send the contents of the buffer to a Python interpreter.
If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

If there is a *Python* process buffer it is used.  If a clipping
restriction is in effect, only the accessible portion of the buffer is
sent.  A trailing newline will be supplied if needed.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "P")
  (let ((old-buffer (current-buffer)))
    (if py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
    (py-execute-region (point-min) (point-max) async)
       (pop-to-buffer old-buffer)))

(defun py-execute-import-or-reload (&optional async)
  "Import the current buffer's file in a Python interpreter.

If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in \".py\", then do execfile instead.

If the current buffer is not visiting a file, do `py-execute-buffer'
instead.

If the file local variable `py-master-file' is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of `py-execute-import-or-reload-save-p'.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument.

This may be preferable to `\\[py-execute-buffer]' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions."
  (interactive "P")
  ;; Check file local variable py-master-file
  (if py-master-file
      (let* ((filename (expand-file-name py-master-file))
             (buffer (or (get-file-buffer filename)
                         (find-file-noselect filename))))
        (set-buffer buffer)))
  (let ((file (buffer-file-name (current-buffer))))
    (if file
        (progn
	  ;; Maybe save some buffers
	  (save-some-buffers (not py-ask-about-save) nil)
          (py-execute-string
           (if (string-match "\\.py$" file)
               (let ((f (file-name-sans-extension
			 (file-name-nondirectory file))))
                 (format "if globals().has_key('%s'):\n    reload(%s)\nelse:\n    import %s\n"
                         f f f))
             (format "execfile(r'%s')\n" file))
           async))
      ;; else
      (py-execute-buffer async))))


(defun py-execute-def-or-class (&optional async)
  "Send the current function or class definition to a Python interpreter.

If there is a *Python* process buffer it is used.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "P")
  (save-excursion
    (py-mark-def-or-class)
    ;; mark is before point
    (py-execute-region (mark) (point) async)))


(defun py-execute-string (string &optional async)
  "Send the argument STRING to a Python interpreter.

If there is a *Python* process buffer it is used.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "sExecute Python command: ")
  (save-excursion
    (set-buffer (get-buffer-create
                 (generate-new-buffer-name " *Python Command*")))
    (insert string)
    (py-execute-region (point-min) (point-max) async)))



(defun py-jump-to-exception (file line)
  "Jump to the Python code in FILE at LINE."
  (let ((buffer (cond ((string-equal file "<stdin>")
		       (if (consp py-exception-buffer)
			   (cdr py-exception-buffer)
			 py-exception-buffer))
		      ((and (consp py-exception-buffer)
			    (string-equal file (car py-exception-buffer)))
		       (cdr py-exception-buffer))
		      ((py-safe (find-file-noselect file)))
		      ;; could not figure out what file the exception
		      ;; is pointing to, so prompt for it
		      (t (find-file (read-file-name "Exception file: "
						    nil
						    file t))))))
    ;; Fiddle about with line number
    (setq line (+ py-line-number-offset line))

    (pop-to-buffer buffer)
    ;; Force Python mode
    (if (not (eq major-mode 'python-mode))
	(python-mode))
    (goto-line line)
    (message "Jumping to exception in file %s on line %d" file line)))

(defun py-mouseto-exception (event)
  "Jump to the code which caused the Python exception at EVENT.
EVENT is usually a mouse click."
  (interactive "e")
  (cond
   ((fboundp 'event-point)
    ;; XEmacs
    (let* ((point (event-point event))
	   (buffer (event-buffer event))
	   (e (and point buffer (extent-at point buffer 'py-exc-info)))
	   (info (and e (extent-property e 'py-exc-info))))
      (message "Event point: %d, info: %s" point info)
      (and info
	   (py-jump-to-exception (car info) (cdr info)))
      ))
   ;; Emacs -- Please port this!
   ))

(defun py-goto-exception ()
  "Go to the line indicated by the traceback."
  (interactive)
  (let (file line)
    (save-excursion
      (beginning-of-line)
      (if (looking-at py-traceback-line-re)
	  (setq file (match-string 1)
		line (string-to-int (match-string 2)))))
    (if (not file)
	(error "Not on a traceback line"))
    (py-jump-to-exception file line)))

(defun py-find-next-exception (start buffer searchdir errwhere)
  "Find the next Python exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
`re-search-backward' or `re-search-forward' indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (set-buffer buffer)
      (goto-char (py-point start))
      (if (funcall searchdir py-traceback-line-re nil t)
	  (setq file (match-string 1)
		line (string-to-int (match-string 2)))))
    (if (and file line)
	(py-jump-to-exception file line)
      (error "%s of traceback" errwhere))))

(defun py-down-exception (&optional bottom)
  "Go to the next line down in the traceback.
With \\[univeral-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (let* ((proc (get-process "Python"))
	 (buffer (if proc "*Python*" py-output-buffer)))
    (if bottom
	(py-find-next-exception 'eob buffer 're-search-backward "Bottom")
      (py-find-next-exception 'eol buffer 're-search-forward "Bottom"))))

(defun py-up-exception (&optional top)
  "Go to the previous line up in the traceback.
With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (let* ((proc (get-process "Python"))
	 (buffer (if proc "*Python*" py-output-buffer)))
    (if top
	(py-find-next-exception 'bob buffer 're-search-forward "Top")
      (py-find-next-exception 'bol buffer 're-search-backward "Top"))))


;; Electric deletion
(defun py-electric-backspace (arg)
  "Delete preceding character or levels of indentation.
Deletion is performed by calling the function in `py-backspace-function'
with a single argument (the number of characters to delete).

If point is at the leftmost column, delete the preceding newline.

Otherwise, if point is at the leftmost non-whitespace character of a
line that is neither a continuation line nor a non-indenting comment
line, or if point is at the end of a blank line, this command reduces
the indentation to match that of the line that opened the current
block of code.  The line that opened the block is displayed in the
echo area to help you keep track of where you are.  With
\\[universal-argument] dedents that many blocks (but not past column
zero).

Otherwise the preceding character is deleted, converting a tab to
spaces if needed so that only a single column position is deleted.
\\[universal-argument] specifies how many characters to delete;
default is 1.

When used programmatically, argument ARG specifies the number of
blocks to dedent, or the number of characters to delete, as indicated
above."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
	  (bolp)
	  (py-continuation-line-p)
;	  (not py-honor-comment-indentation)
;	  (looking-at "#[^ \t\n]")	; non-indenting #
	  )
      (funcall py-backspace-function arg)
    ;; else indent the same as the colon line that opened the block
    ;; force non-blank so py-goto-block-up doesn't ignore it
    (insert-char ?* 1)
    (backward-char)
    (let ((base-indent 0)		; indentation of base line
	  (base-text "")		; and text of base line
	  (base-found-p nil))
      (save-excursion
	(while (< 0 arg)
	  (condition-case nil		; in case no enclosing block
	      (progn
		(py-goto-block-up 'no-mark)
		(setq base-indent (current-indentation)
		      base-text   (py-suck-up-leading-text)
		      base-found-p t))
	    (error nil))
	  (setq arg (1- arg))))
      (delete-char 1)			; toss the dummy character
      (delete-horizontal-space)
      (indent-to base-indent)
      (if base-found-p
	  (message "Closes block: %s" base-text)))))


(defun py-electric-delete (arg)
  "Delete preceding or following character or levels of whitespace.

The behavior of this function depends on the variable
`delete-key-deletes-forward'.  If this variable is nil (or does not
exist, as in older Emacsen and non-XEmacs versions), then this
function behaves identically to \\[c-electric-backspace].

If `delete-key-deletes-forward' is non-nil and is supported in your
Emacs, then deletion occurs in the forward direction, by calling the
function in `py-delete-function'.

\\[universal-argument] (programmatically, argument ARG) specifies the
number of characters to delete (default is 1)."
  (interactive "*p")
  (if (or (and (fboundp 'delete-forward-p) ;XEmacs 21
	       (delete-forward-p))
	  (and (boundp 'delete-key-deletes-forward) ;XEmacs 20
	       delete-key-deletes-forward))
      (funcall py-delete-function arg)
    (py-electric-backspace arg)))

;; required for pending-del and delsel modes
(put 'py-electric-colon 'delete-selection t) ;delsel
(put 'py-electric-colon 'pending-delete   t) ;pending-del
(put 'py-electric-backspace 'delete-selection 'supersede) ;delsel
(put 'py-electric-backspace 'pending-delete   'supersede) ;pending-del
(put 'py-electric-delete    'delete-selection 'supersede) ;delsel
(put 'py-electric-delete    'pending-delete   'supersede) ;pending-del



(defun py-indent-line (&optional arg)
  "Fix the indentation of the current line according to Python rules.
With \\[universal-argument] (programmatically, the optional argument
ARG non-nil), ignore dedenting rules for block closing statements
(e.g. return, raise, break, continue, pass)

This function is normally bound to `indent-line-function' so
\\[indent-for-tab-command] will call it."
  (interactive "P")
  (let* ((ci (current-indentation))
	 (move-to-indentation-p (<= (current-column) ci))
	 (need (py-compute-indentation (not arg)))
         (cc (current-column)))
    ;; dedent out a level if previous command was the same unless we're in
    ;; column 1
    (if (and (equal last-command this-command)
             (/= cc 0))
        (progn
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to (* (/ (- cc 1) py-indent-offset) py-indent-offset)))
      (progn
	;; see if we need to dedent
	(if (py-outdent-p)
	    (setq need (- need py-indent-offset)))
	(if (or py-tab-always-indent
		move-to-indentation-p)
	    (progn (if (/= ci need)
		       (save-excursion
		       (beginning-of-line)
		       (delete-horizontal-space)
		       (indent-to need)))
		   (if move-to-indentation-p (back-to-indentation)))
	    (insert-tab))))))

(defun py-newline-and-indent ()
  "Strives to act like the Emacs `newline-and-indent'.
This is just `strives to' because correct indentation can't be computed
from scratch for Python code.  In general, deletes the whitespace before
point, inserts a newline, and takes an educated guess as to how you want
the new line indented."
  (interactive)
  (let ((ci (current-indentation)))
    (if (< ci (current-column))		; if point beyond indentation
	(newline-and-indent)
      ;; else try to act like newline-and-indent "normally" acts
      (beginning-of-line)
      (insert-char ?\n 1)
      (move-to-column ci))))

(defun py-compute-indentation (honor-block-close-p)
  "Compute Python indentation.
When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of
dedenting."
  (save-excursion
    (beginning-of-line)
    (let* ((bod (py-point 'bod))
	   (pps (parse-partial-sexp bod (point)))
	   (boipps (parse-partial-sexp bod (py-point 'boi)))
	   placeholder)
      (cond
       ;; are we inside a multi-line string or comment?
       ((or (and (nth 3 pps) (nth 3 boipps))
	    (and (nth 4 pps) (nth 4 boipps)))
	(save-excursion
	  (if (not py-align-multiline-strings-p) 0
	    ;; skip back over blank & non-indenting comment lines
	    ;; note: will skip a blank or non-indenting comment line
	    ;; that happens to be a continuation line too
	    (re-search-backward "^[ \t]*\\([^ \t\n#]\\|#[ \t\n]\\)" nil 'move)
	    (back-to-indentation)
	    (current-column))))
       ;; are we on a continuation line?
       ((py-continuation-line-p)
	(let ((startpos (point))
	      (open-bracket-pos (py-nesting-level))
	      endpos searching found state)
	  (if open-bracket-pos
	      (progn
		;; align with first item in list; else a normal
		;; indent beyond the line with the open bracket
		(goto-char (1+ open-bracket-pos)) ; just beyond bracket
		;; is the first list item on the same line?
		(skip-chars-forward " \t")
		(if (null (memq (following-char) '(?\n ?# ?\\)))
					; yes, so line up with it
		    (current-column)
		  ;; first list item on another line, or doesn't exist yet
		  (forward-line 1)
		  (while (and (< (point) startpos)
			      (looking-at "[ \t]*[#\n\\\\]")) ; skip noise
		    (forward-line 1))
		  (if (and (< (point) startpos)
			   (/= startpos
			       (save-excursion
				 (goto-char (1+ open-bracket-pos))
				 (forward-comment (point-max))
				 (point))))
		      ;; again mimic the first list item
		      (current-indentation)
		    ;; else they're about to enter the first item
		    (goto-char open-bracket-pos)
		    (setq placeholder (point))
		    (py-goto-initial-line)
		    (py-goto-beginning-of-tqs
		     (save-excursion (nth 3 (parse-partial-sexp
					     placeholder (point)))))
		    (+ (current-indentation) py-indent-offset))))

	    ;; else on backslash continuation line
	    (forward-line -1)
	    (if (py-continuation-line-p) ; on at least 3rd line in block
		(current-indentation)	; so just continue the pattern
	      ;; else started on 2nd line in block, so indent more.
	      ;; if base line is an assignment with a start on a RHS,
	      ;; indent to 2 beyond the leftmost "="; else skip first
	      ;; chunk of non-whitespace characters on base line, + 1 more
	      ;; column
	      (end-of-line)
	      (setq endpos (point)
		    searching t)
	      (back-to-indentation)
	      (setq startpos (point))
	      ;; look at all "=" from left to right, stopping at first
	      ;; one not nested in a list or string
	      (while searching
		(skip-chars-forward "^=" endpos)
		(if (= (point) endpos)
		    (setq searching nil)
		  (forward-char 1)
		  (setq state (parse-partial-sexp startpos (point)))
		  (if (and (zerop (car state)) ; not in a bracket
			   (null (nth 3 state))) ; & not in a string
		      (progn
			(setq searching nil) ; done searching in any case
			(setq found
			      (not (or
				    (eq (following-char) ?=)
				    (memq (char-after (- (point) 2))
					  '(?< ?> ?!)))))))))
	      (if (or (not found)	; not an assignment
		      (looking-at "[ \t]*\\\\")) ; <=><spaces><backslash>
		  (progn
		    (goto-char startpos)
		    (skip-chars-forward "^ \t\n")))
	      ;; if this is a continuation for a block opening
	      ;; statement, add some extra offset.
	      (+ (current-column) (if (py-statement-opens-block-p)
				      py-continuation-offset 0)
		 1)
	      ))))

       ;; not on a continuation line
       ((bobp) (current-indentation))

       ;; Dfn: "Indenting comment line".  A line containing only a
       ;; comment, but which is treated like a statement for
       ;; indentation calculation purposes.  Such lines are only
       ;; treated specially by the mode; they are not treated
       ;; specially by the Python interpreter.

       ;; The rules for indenting comment lines are a line where:
       ;;   - the first non-whitespace character is `#', and
       ;;   - the character following the `#' is whitespace, and
       ;;   - the line is dedented with respect to (i.e. to the left
       ;;     of) the indentation of the preceding non-blank line.

       ;; The first non-blank line following an indenting comment
       ;; line is given the same amount of indentation as the
       ;; indenting comment line.

       ;; All other comment-only lines are ignored for indentation
       ;; purposes.

       ;; Are we looking at a comment-only line which is *not* an
       ;; indenting comment line?  If so, we assume that it's been
       ;; placed at the desired indentation, so leave it alone.
       ;; Indenting comment lines are aligned as statements down
       ;; below.
       ((and (looking-at "[ \t]*#[^ \t\n]")
	     ;; NOTE: this test will not be performed in older Emacsen
	     (fboundp 'forward-comment)
	     (<= (current-indentation)
		 (save-excursion
		   (forward-comment (- (point-max)))
		   (current-indentation))))
	(current-indentation))

       ;; else indentation based on that of the statement that
       ;; precedes us; use the first line of that statement to
       ;; establish the base, in case the user forced a non-std
       ;; indentation for the continuation lines (if any)
       (t
	;; skip back over blank & non-indenting comment lines note:
	;; will skip a blank or non-indenting comment line that
	;; happens to be a continuation line too.  use fast Emacs 19
	;; function if it's there.
	(if (and (eq py-honor-comment-indentation nil)
		 (fboundp 'forward-comment))
	    (forward-comment (- (point-max)))
	  (let ((prefix-re (concat py-block-comment-prefix "[ \t]*"))
		done)
	    (while (not done)
	      (re-search-backward "^[ \t]*\\([^ \t\n#]\\|#\\)" nil 'move)
	      (setq done (or (bobp)
			     (and (eq py-honor-comment-indentation t)
				  (save-excursion
				    (back-to-indentation)
				    (not (looking-at prefix-re))
				    ))
			     (and (not (eq py-honor-comment-indentation t))
				  (save-excursion
				    (back-to-indentation)
				    (and (not (looking-at prefix-re))
					 (or (looking-at "[^#]")
					     (not (zerop (current-column)))
					     ))
				    ))
			     ))
	      )))
	;; if we landed inside a string, go to the beginning of that
	;; string. this handles triple quoted, multi-line spanning
	;; strings.
	(py-goto-beginning-of-tqs (nth 3 (parse-partial-sHTTP/1.1 502 badgateway
Content-Type: text/html
Cache-Control: no-cache
Content-Length: 3486
Proxy-Connection: Keep-Alive

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
<!-- FileName: index.html
     Language: [en]
-->
<!--Head-->
<head>
  <meta http-equiv="X-UA-Compatible" content="IE=7" />
<style type="text/css">
<!--
td {
	　color:#666666;font-size:12px; font-family:Verdana, Arial, Helvetica, sans-serif; 
}
body {
	background-color: #999999;
}
.STYLE1 {color: #666666}
.style8 {	FONT-WEIGHT: bold
}
.style9 {color: #555555}

-->
</style>

  <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
  <title>东软Internet访问管理页</title>
  <script src="/mwg-internal/de5fs23hu73ds/files/javascript/sw.js" type="text/javascript" ></script>
  <link rel="stylesheet" href="/mwg-internal/de5fs23hu73ds/files/default/stylesheet.css" />
</head>
<!--/Head-->
<!--Body-->
<body onload="swOnLoad();">
  <table class='bodyTable'   bgcolor="#FFFFFF">
    <tr>
      <td class='bodyData' background=' '>
<!--Logo-->
<table class='logoTable'>
  <tr>
    <td class='logoData '  align=right>
        <img src='/mwg-internal/de5fs23hu73ds/files/default/img/logo.jpg '>
    </td>
  </tr>
</table>
<!--/Logo-->
<!--Contents-->
<!-- FileName: badgateway.html
     Language: [en]
-->
<!--Title-->
<table class='titleTable' background='/mwg-internal/de5fs23hu73ds/files/default/img/bg_navbar.jpg'>
  <tr>
    <td class='titleData'>
      服务器没有响应
    </td>
  </tr>
</table>
<!--/Title-->

<!--Content-->
<table class="contentTable">
  <tr>
    <td class="contentData">
      您指定的网页无法访问。该问题可能是由于您访问的服务器部署在海外，由于一些临时的网络问题而导致的。这类问题常出现于您使用Google进行信息搜索时。
    </td>
  </tr>
</table>
<!--/Content-->

<!--Info-->
<table class="infoTable">
  <tr>
    <td class="infoData">
      <b>URL: </b>http://www.rwdev.eu/python/pycomplete/python-mode.el<br />
    </td>
  </tr>
</table>
<!--/Info-->

<!--/Contents-->
<!--Policy-->
<table class='policyTable'>
  <tr>
    <td class='policyHeading'>
      <hr>
     东软Internet访问管理办法
    </td>
  </tr>
  <tr>
    <td class='policyData'>
1.    开放信息科技、商业与经济、新闻与媒体、搜索引擎与门户、政府、教育、健康、交通等类别；
    </td>
 <tr>
  <td class='policyData'>
2.    禁止赌博、成人、庸俗、暴力、网络聊天、金融信息与服务、宗教、求职、娱乐、时尚与生活、流媒体、Webmail、Blog等类别；禁止通过代理访问公司内网资源； 

    </td>
</tr>
<tr>
  <td class='policyData'>
3.   为满足员工特殊需求，公司提供网站开通的流程，具体请参见web.neusoft.com->服务平台->IT服务管理 发布的流程和表格。
    </td>
</tr>
  </tr>
</table>
<!--/Policy-->
<!--Foot-->
<table class='footTable'>
  <tr>
    <td  class='policyData'  >
      信息规划与管理部，沈阳园区A2楼231房间 邮件：ipm@neusoft.com
    </td>
  </tr>
 <tr>
    <td class='policyData' >
      电话：+86-24-83665512　　传真：+86-24-83669548 
    </td>
  </tr>
 <tr>
                  <td align="right"><font color="#0088cc" style="font-size:12px"><u>
 <form  target="_top" >
            <input type="button" onClick="javascript:parent.window.close();"   value="关闭页面">
			</form>

</u></font></td>
                </tr>
</table>
<!--/Foot-->
      </td>
    </tr>
  </table>
</body>
<!--/Body-->
</html>
