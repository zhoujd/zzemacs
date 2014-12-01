;;;python programme setting
;;https://wiki.python.org/moin/EmacsEditor

(zz-load-path "site-lisp/python-mode")

(require 'pymacs)
(autoload 'pymacs-apply "pymacs") 
(autoload 'pymacs-call "pymacs") 
(autoload 'pymacs-eval "pymacs" nil t) 
(autoload 'pymacs-exec "pymacs" nil t) 
(autoload 'pymacs-load "pymacs" nil t) 

(pymacs-load "ropemacs" "rope-") 
(setq ropemacs-enable-autoimport t)

(require 'pycomplete)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(setq interpreter-mode-alist(cons '("python" . python-mode)
                           interpreter-mode-alist))

;;ac for python-mode
(require 'auto-complete-pycomplete)
;;highlight indent
(require 'highlight-indentation)

;;sudo apt-get install ipython
;;http://archive.ipython.org/release/
;;git clone https://github.com/ipython/ipython.git
;;pyreadline https://pypi.python.org/pypi/pyreadline/2.0
;;ipython http://archive.ipython.org/release/1.0.0/
(unless-ms-windows  
 (setq ipython-command "/usr/bin/ipython"))
(setq-default py-python-command-args '("--colors=Linux"))
(require 'ipython)
(add-hook 'py-shell-hook
          (lambda ()
            (define-key py-shell-map (kbd "C-c M-o") 'clear-input)))

;;pdb setup, note the python version
;;run pdb.py (like this): python -i -m pdb <file-name.py>
(when-ms-windows
 (setq pdb-path 'c:/python27/Lib/pdb.py
       gud-pdb-command-name (symbol-name pdb-path))
 (defadvice pdb (before gud-query-cmdline activate)
   "Provide a better default command line when called interactively."
   (interactive
    (list (gud-query-cmdline pdb-path
                             (file-name-nondirectory buffer-file-name))))))

;;pdb setup on linux
(unless-ms-windows
 (setq gud-pdb-command-name "python -i -m pdb"))

;;sudo apt-get install pydb
;;http://sourceforge.net/projects/bashdb/files/pydb/
;;http://bashdb.sourceforge.net/pydb/
;;https://github.com/rocky/pydb.git
(zz-load-path "site-lisp/pydb")
(require 'pydb)
(autoload 'pydb "pydb" "Python Debugger mode via GUD and pydb" t)
(setq pydb-pydbtrack-minor-mode-string "")

;;scons file setting
(setq auto-mode-alist (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SConscript" . python-mode) auto-mode-alist))


(provide 'python-setting)

;;; python-setting.el end here
