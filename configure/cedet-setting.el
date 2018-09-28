;;;; cedet-setting.el --- cedet common file


(zz-load-path "site-lisp")

(setq semantic-default-submodes
      '(;; Perform semantic actions during idle time
        global-semantic-idle-scheduler-mode
        ;; Use a database of parsed tags
        global-semanticdb-minor-mode
        ;; Decorate buffers with additional semantic information
        global-semantic-decoration-mode
        ;; Highlight the name of the function you're currently in
        global-semantic-highlight-func-mode
        ;; show the name of the function at the top in a sticky
        global-semantic-stickyfunc-mode
        ;; Generate a summary of the current tag when idle
        global-semantic-idle-summary-mode
        ;; Show a breadcrumb of location during idle time
        global-semantic-idle-breadcrumbs-mode
        ;; Switch to recently changed tags with `semantic-mrub-switch-tags',
        ;; or `C-x B'
        global-semantic-mru-bookmark-mode))

(semantic-mode t)
(global-ede-mode t)
(global-semantic-idle-completions-mode)

(defun my:add-semantic-to-autocomplete() 
  (add-to-list 'ac-sources 'ac-source-semantic))

(mapc
 (lambda (mode)
   (add-hook mode 'my:add-semantic-to-autocomplete))
 '(c-mode-common-hook
   ))


(provide 'cedet-setting)

;;; cedet-setting.el ends here
