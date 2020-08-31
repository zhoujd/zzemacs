;;;java programme setting

(zz:load-path "site-lisp/ajc")

(require 'ajc-java-complete-config)
(setq ajc-tag-file  (concat zzemacs-path  "/site-lisp/ajc/java_base.tag"))

(defun zz:java-mode-hook ()
  (ajc-java-complete-mode)
  (hide-ifdef-mode -1))

(add-hook 'java-mode-hook 'zz:java-mode-hook)
(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

;;android-mode
(require 'emdroid)
(require 'android-mode)
(setq android-mode-sdk-dir "~/work/android/android")
(add-hook 'gud-mode-hook
          (lambda ()
            (add-to-list 'gud-jdb-classpath
                         "~/work/android-sdk-linux_86/platforms/android-7/android.jar")
            ))

(provide 'java-setting)

;;; java-setting.el end here
