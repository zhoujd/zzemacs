;;;; cedet-setting.el --- cedet common file


(zz/load-path "site-lisp")

(setq semantic-idle-work-update-headers-flag  t)
(setq semantic-idle-scheduler-idle-time       10)
(setq semantic-idle-scheduler-work-idle-time  60)

(setq semantic-default-submodes
      '(global-semanticdb-minor-mode
        global-semantic-idle-scheduler-mode
        global-semantic-idle-summary-mode
        global-semantic-idle-breadcrumbs-mode
        global-semantic-mru-bookmark-mode))

(setq semantic-idle-scheduler-function (lambda () t))

(semantic-mode t)

(mapc
 (lambda (mode)
   (add-hook mode 'semantic-mode))
 '(c-mode-hook
   c++-mode-hook
   python-mode-hook
   ))

;;list methold in current buffer
;;c-mode and (or) python-mode and for lisp
(require 'eassist)
(defkeys-map eassist-mode-map
  ((kbd "TAB") 'eassist-jump-to-method)
  ((kbd "C-b") 'eassist-backspace-pressed)
  ((kbd "C-q") 'eassist-escape)
  ((kbd "C-g") 'eassist-escape))

;;Nothing to complete
(defun zz/semantic-remove-hooks ()
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-notc-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-nolongprefix-completion-at-point-function))

(add-hook 'semantic-mode-hook #'zz/semantic-remove-hooks)


(provide 'cedet-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; cedet-setting.el ends here
