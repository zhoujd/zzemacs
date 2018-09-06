;;;python programme setting
;;https://wiki.python.org/moin/EmacsEditor

(zz-load-path "site-lisp/python-mode")

;;pymacs & ropemacs
(require 'pymacs)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call  "pymacs")
(autoload 'pymacs-eval  "pymacs" nil t)
(autoload 'pymacs-exec  "pymacs" nil t)
(autoload 'pymacs-load  "pymacs" nil t)

(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

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

;;remove warning
(setq python-shell-completion-native-enable nil)

;;ac for python-mode
(require 'auto-complete-pycomplete)
;;highlight indent
(require 'highlight-indentation)

;;pdb setup, note the python version
;;run pdb.py (like this): python -i -m pdb <file-name.py>
;;M-x pdb
(setq gud-pdb-command-name "python -i -m pdb")

;;scons file setting
(setq auto-mode-alist (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SConscript" . python-mode) auto-mode-alist))


(provide 'python-setting)

;;; python-setting.el end here
