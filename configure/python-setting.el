;;;python programme setting
;;https://wiki.python.org/moin/EmacsEditor
;;https://elpy.readthedocs.io/en/latest/

;;elpy pkg deps
;;pip2 install rope jedi flake8 importmagic
;;pip3 install rope jedi flake8 importmagic
;;git clone https://github.com/jorgenschaefer/elpy
;;M-x elpy-config
(zz:load-path "site-lisp/python-mode")
(zz:load-path "site-lisp/elpy")
(load "elpy")
(load "elpy-rpc")
(load "elpy-shell")
(load "elpy-profile")
(load "elpy-refactor")
(load "elpy-django")
(elpy-enable)

;;disable highlight-indentation-mode
(add-hook 'elpy-mode-hook
          (lambda ()
            (highlight-indentation-mode -1)))

(zz:load-path "site-lisp/python-environment")
(zz:load-path "site-lisp/epc")
(zz:load-path "site-lisp/jedi-core")
(require 'company-jedi)
(add-to-list 'company-backends 'company-jedi)
(add-hook 'python-mode-hook 'jedi:setup)

;;remove warning
(setq python-shell-completion-native-enable nil)
;;remove warning: "Can't guess python-indent-offset"
(setq python-indent-guess-indent-offset-verbose nil)

;;pdb setup, note the python version
;;run pdb.py (like this): python3 -i -m pdb <file-name.py>
;;M-x pdb
(setq gud-pdb-command-name "python3 -i -m pdb")

;;scons file setting
(setq auto-mode-alist (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SConscript" . python-mode) auto-mode-alist))

;;python3
(setq py-python-command "python3")

;;ipython or python3
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

(setq jedi:environment-root "jedi"
      jedi:environment-virtualenv (append python-environment-virtualenv
                                          '("--python" "python3")))

;;flycheck
(setq flycheck-python-pycompile-executable "python3"
      flycheck-python-pylint-executable "python3"
      flycheck-python-flake8-executable "python3")

;;virtualenv
;;python3 -m venv ~/.venv/emacs
;;vim ~/.venv/emacs/pyvenv.cfg
;;include-system-site-packages = true
(setq elpy-rpc-python-command "python3")
(setq elpy-rpc-virtualenv-path "~/.venv/emacs")

(defun zz:venv ()
  (interactive)
  (let ((venv (expand-file-name elpy-rpc-virtualenv-path)))
    (when (file-exists-p venv)
      (pyvenv-activate venv))))

(defun zz:py-indent-4 ()
  (setq tab-width 4)
  (setq python-shift-right 4)
  (setq python-shift-left 4)
  (setq python-indent 4))

(defun zz:py-indent-2 ()
  (setq tab-width 2)
  (setq python-shift-right 2)
  (setq python-shift-left 2)
  (setq python-indent 2))

(defun zz:python-mode-hook ()
  (zz:py-indent-4))

(add-hook 'python-mode-hook
          'zz:python-mode-hook)


(provide 'python-setting)

;;; python-setting.el end here
