;;;java programme setting

(zz-load-path "site-lisp/ajc")

(require 'ajc-java-complete-config)
(setq ajc-tag-file  (concat zzemacs-path  "site-lisp/ajc/java_base.tag"))

(defun my-java-mode-hook ()
  (ajc-java-complete-mode)
  (hide-ifdef-mode -1))

(add-hook 'java-mode-hook 'my-java-mode-hook)
(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

(provide 'java-setting)

;;; java-setting.el end here
