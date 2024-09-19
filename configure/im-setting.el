;;;; im-setting.el --- im setting file
;;;

;;https://github.com/tumashu/pyim
;;https://github.com/tumashu/pyim-basedict
;;M-x toggle-input-method"  "C-\\"
(zz/load-path "site-lisp/pyim")
(zz/load-path "site-lisp/pyim-basedict")
(require 'pyim)
(require 'pyim-basedict)
(pyim-basedict-enable)
(setq default-input-method "pyim")
(setq pyim-page-length 5)

(defun pyim-add-unread-command-events (key &optional reset)
  (when reset
    (setq unread-command-events nil))
  (setq unread-command-events
        (if (characterp key)
            (cons key unread-command-events)
          (append (mapcan (lambda (e) (list e))
                          (append key nil))
                  unread-command-events))))

;;Search with Chinese
;;M-x: pyim-forward-word
;;M-x: pyim-backward-word
(require 'pyim-cstring-utils)


(provide 'im-setting)

;;; im-setting.el ends here
