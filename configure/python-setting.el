;;;python programme setting
;;https://wiki.python.org/moin/EmacsEditor
;;https://elpy.readthedocs.io/en/latest/

;;elpy pkg deps
;;sudo apt install python3-jedi black python3-autopep8 yapf3 python3-yapf
;;pip3 install rope jedi flake8 importmagic
;;git clone https://github.com/jorgenschaefer/elpy
;;M-x elpy-config
(zz/load-path "site-lisp/python-mode")

;;python3 interpreter
(setq python-interpreter "python3")
(setq python-shell-interpreter "python3")
(setq python-shell-interpreter-args "-i")

;;pdb setup, note the python version
;;run pdb.py (like this): python3 -i -m pdb <file-name.py>
;;M-x pdb
(setq gud-pdb-command-name "python3 -i -m pdb")

;;python3
(setq py-python-command "python3")

;;Warning: Your ‘python-shell-interpreter’ doesn’t seem to support readline
(setq python-shell-completion-native-enable nil)

;;custom indent
(defun zz/python-indent (num)
  (interactive "nIndent: ")
  (eval-expression
   '(progn
      (setq tab-width num)
      (setq python-shift-right num)
      (setq python-shift-left num)
      (setq python-indent num)
      ))
  (message "Select and press TAB to indent: %d" num))

(defun zz/py-indent-4 ()
  (setq tab-width 4)
  (setq python-shift-right 4)
  (setq python-shift-left 4)
  (setq python-indent 4))

(defun zz/py-indent-2 ()
  (setq tab-width 2)
  (setq python-shift-right 2)
  (setq python-shift-left 2)
  (setq python-indent 2))

(defun zz/python-mode-hook ()
  (defkeys-map python-mode-map
    ((kbd "C-c m") 'eassist-list-methods))
  (zz/py-indent-4))

(add-hook 'python-mode-hook 'zz/python-mode-hook)

;;helm-pydoc
(zz/load-path "site-lisp/helm-pydoc")
(require 'helm-pydoc)

;;lsp-mode or eglog
;;lsp-deferred or eglot-ensure
;;https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
;;pip3 install python-lsp-server
;;~/.venv/emacs/bin/pylsp
(defvar zz/default-pyls "pylsp-wrapper"
  "default pyls to be used")
(defun zz/python-eglot-enable ()
  "set variables and hook for eglot python IDE"
  (interactive)
  (setq company-backends
        (cons 'company-capf
              (remove 'company-capf company-backends)))
  (add-to-list 'eglot-server-programs
               `(python-mode ,zz/default-pyls))
  (add-hook 'python-mode-hook 'eglot-ensure))
(defun zz/python-eglot-disable ()
  "remove hook for eglot python"
  (interactive)
  (remove-hook 'python-mode-hook 'eglot-ensure))
(zz/python-eglot-enable)


(provide 'python-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; python-setting.el end here
