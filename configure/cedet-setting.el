;;;; cedet-setting.el --- cedet common file


(zz-load-path "site-lisp")

;; cedet
(mapc (lambda (mode) (add-to-list 'semantic-default-submodes mode))
      '(global-semantic-idle-scheduler-mode
        global-semanticdb-minor-mode
        global-semantic-idle-summary-mode
        global-semantic-mru-bookmark-mode))

(global-ede-mode t)
(semantic-mode t)

(defun my:add-semantic-to-autocomplete() 
  (add-to-list 'ac-sources 'ac-source-semantic))

(mapc
 (lambda (mode)
   (add-hook mode 'my:add-semantic-to-autocomplete))
 '(
   c-mode-common-hook
   ))


(provide 'cedet-setting)

;;; cedet-setting.el ends here
