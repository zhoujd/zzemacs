;;;python programme setting
;;https://wiki.python.org/moin/EmacsEditor

;;elpy pkg deps
;;pip install rope jedi flake8 importmagic
;;pip3 install rope jedi flake8 importmagic
(zz-load-path "site-lisp/python-mode")
(zz-load-path "site-lisp/elpy")
(load "elpy")
(load "elpy-rpc")
(load "elpy-shell")
(load "elpy-profile")
(load "elpy-refactor")
(load "elpy-django")
(elpy-enable)

;;pyenv-mode
;(require 'pyenv-mode)

(zz-load-path "site-lisp/python-environment")
(zz-load-path "site-lisp/epc")
(zz-load-path "site-lisp/jedi-core")
(require 'company-jedi)
(add-to-list 'company-backends 'company-jedi)
(add-hook 'python-mode-hook 'jedi:setup)

;;remove warning
(setq python-shell-completion-native-enable nil)
;;remove warning: "Can't guess python-indent-offset"
(setq python-indent-guess-indent-offset-verbose nil)

;;pdb setup, note the python version
;;run pdb.py (like this): python -i -m pdb <file-name.py>
;;M-x pdb
(setq gud-pdb-command-name "python -i -m pdb")

;;scons file setting
(setq auto-mode-alist (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SConscript" . python-mode) auto-mode-alist))

;;python3
(setq py-python-command "python3")
(setq python-shell-interpreter "python3")
(setq elpy-rpc-python-command "python3")

(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent 2)
            ))


(provide 'python-setting)

;;; python-setting.el end here
