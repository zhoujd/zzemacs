;;;python programme setting

;;python-mode settings
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist(cons '("python" . python-mode)
                             interpreter-mode-alist))
;;path to the python interpreter, e.g.: ~rw/python27/bin/python2.7

;;https://launchpad.net/python-mode/
(setq py-python-command "python")
(zz-load-file "site-lisp/python-mode.el")

(require 'pymacs)
(autoload 'pymacs-apply "pymacs") 
(autoload 'pymacs-call "pymacs") 
(autoload 'pymacs-eval "pymacs" nil t) 
(autoload 'pymacs-exec "pymacs" nil t) 
(autoload 'pymacs-load "pymacs" nil t) 

(pymacs-load "ropemacs" "rope-") 
(setq ropemacs-enable-autoimport t)

(require 'pycomplete)

;;sudo apt-get install ipython
;;http://archive.ipython.org/release/
;;git clone https://github.com/ipython/ipython.git
;;pyreadline https://pypi.python.org/pypi/pyreadline/2.0
;;ipython http://archive.ipython.org/release/1.0.0/
(unless-ms-windows  
 (setq ipython-command "/usr/bin/ipython"))
(setq-default py-python-command-args '("--colors=Linux"))
(require 'ipython)

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

;;sudo apt-get install pydb
;;http://sourceforge.net/projects/bashdb/
;;http://bashdb.sourceforge.net/pydb/
;;https://github.com/rocky/pydb.git
(zz-load-path "site-lisp/pydb")
(require 'pydb)
(autoload 'pydb "pydb" "Python Debugger mode via GUD and pydb" t)

;;scons file setting
(setq auto-mode-alist (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SConscript" . python-mode) auto-mode-alist))

(provide 'python-setting)

;;; python-setting.el end here
