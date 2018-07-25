;;;python programme setting
;;https://wiki.python.org/moin/EmacsEditor

(zz-load-path "site-lisp/python-mode")

;;pymacs with auto complete
(ac-ropemacs-initialize)
(ac-ropemacs-setup)

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-ropemacs)))

(require 'pycomplete)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))

(autoload 'python-mode "python-mode" "Python editing mode." t)
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))

;;ac for python-mode
(require 'auto-complete-pycomplete)
;;highlight indent
(require 'highlight-indentation)

;;pdb setup, note the python version
;;run pdb.py (like this): python -i -m pdb <file-name.py>
;;M-x pdb
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

;;scons file setting
(setq auto-mode-alist (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SConscript" . python-mode) auto-mode-alist))


(provide 'python-setting)

;;; python-setting.el end here
