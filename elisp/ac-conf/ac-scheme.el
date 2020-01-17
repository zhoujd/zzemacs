;;; Auto complete for scheme

;;auto-complete setting
(defvar ac-source-scheme
  '((candidates . (lambda ()
                    (require 'scheme-complete)
                    (all-completions ac-target
                                     (car (scheme-current-env))))))
  "Source for scheme keywords.")

;;Auto-complete-mode config
(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'ac-sources)
            (setq ac-sources (append ac-sources '(ac-source-scheme)))))

