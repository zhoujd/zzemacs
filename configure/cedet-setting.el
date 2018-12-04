;;;; cedet-setting.el --- cedet common file


(zz-load-path "site-lisp")

(setq semantic-idle-work-update-headers-flag  t)
(setq semantic-idle-scheduler-idle-time       10)
(setq semantic-idle-scheduler-work-idle-time  60)

(setq semantic-default-submodes
      '(global-semanticdb-minor-mode
        global-semantic-idle-scheduler-mode
        global-semantic-idle-summary-mode
        global-semantic-idle-breadcrumbs-mode
        global-semantic-mru-bookmark-mode))

(semantic-mode t)

(setq semantic-idle-scheduler-function (lambda () t))

(provide 'cedet-setting)

;;; cedet-setting.el ends here
