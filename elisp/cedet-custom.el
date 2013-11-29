;;;; cedet-custom.el --- cedet setting file
;;;

;;cedet version flag t for inside
(setq use-cedet-inside-flag (if-emacs24-3 t nil))
(if use-cedet-inside-flag
    (progn
      ;;auto complete
      (require 'cedet)
      ;; speed bar
      (require 'semantic/sb)

      ;; Helper tools.
      (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                        global-semanticdb-minor-mode
                                        global-semantic-idle-summary-mode
                                        global-semantic-mru-bookmark-mode))

      ;; smart complitions
      (require 'semantic/ia)
      (setq-mode-local c-mode semanticdb-find-default-throttle
                       '(project unloaded system recursive))
      (setq-mode-local c++-mode semanticdb-find-default-throttle
                       '(project unloaded system recursive))

      (global-ede-mode t)
      (semantic-mode t)
      )
    (progn
      ;; Disable cedet inside emacs
      (if-ms-windows
       (progn
         (setq load-path (remove (format "%s/lisp/cedet" (getenv "EMACS_DIR")) load-path)))
       (progn
         (setq load-path (remove "/usr/share/emacs/cedet" load-path))
         (setq load-path (remove (format "/usr/share/emacs/%s.%s/lisp/cedet"
                                         emacs-major-version emacs-minor-version)
                                 load-path))))
      
      (zz-load-path "site-lisp/cedet/common")
      
      ;; Load CEDET.
      ;; See cedet/common/cedet.info for configuration details.
      ;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
      ;; CEDET component (including EIEIO) gets activated by another 
      ;; package (Gnus, auth-source, ...).
      (require 'cedet)
      (global-ede-mode t)      
      (semantic-load-enable-minimum-features)
      (semantic-load-enable-code-helpers)

      ;; Enable source code folding
      (when window-system
          (global-semantic-tag-folding-mode 1))
      ))

(provide 'cedet-custom)

;;; cedet-custom.el ends here

