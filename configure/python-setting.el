;;;python programme setting

;; python-mode settings
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist(cons '("python" . python-mode)
                             interpreter-mode-alist))
;; path to the python interpreter, e.g.: ~rw/python27/bin/python2.7
(setq py-python-command "python")
(autoload 'python-mode "python-mode" "Python editing mode." t)


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
;;git clone https://github.com/ipython/ipython.git
(unless (or (eq window-system 'w32)
            (eq window-system 'win32))
  (setq ipython-command "/usr/bin/ipython"))
(setq-default py-python-command-args '("--colors=Linux"))
(require 'ipython)


;;sudo apt-get install pydb
;;http://sourceforge.net/projects/bashdb/
;;https://github.com/rocky/pydb.git
(zz-load-path "site-lisp/pydb")
(require 'pydb)
(autoload 'pydb "pydb" "Python Debugger mode via GUD and pydb" t)

;;scons file setting
(setq auto-mode-alist (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SConscript" . python-mode) auto-mode-alist))

(provide 'python-setting)

;;; python-setting.el end here
