;;;this wiki setting

(zz-load-path "site-lisp/emacs-wiki")
(require 'emacs-wiki)

(setq emacs-wiki-projects
      '(("ProgrammingWiki" .
         ((emacs-wiki-directories . ("~/wiki/programming"))
          (emacs-wiki-project-server-prefix . "../programming")
          (emacs-wiki-publishing-directory
           . "~/personal-site/programming")))
        ("SystemWiki" .
         ((emacs-wiki-directories . ("~/wiki/system"))
          (emacs-wiki-project-server-prefix . "../system")
          (emacs-wiki-publishing-directory
           . "~/personal-site/system")))
        ))

(add-hook 'emacs-wiki-mode-hook
          (lambda ()
            (define-key emacs-wiki-mode-map (kbd "C-c C-h") 'emacs-wiki-preview-html)
            (define-key emacs-wiki-mode-map (kbd "C-c C-c") 'emacs-wiki-preview-source)))

(defun emacs-wiki-preview-source ()
  (interactive)
  (emacs-wiki-publish-this-page)
  (find-file (emacs-wiki-published-file)))

(defun emacs-wiki-preview-html ()
  (interactive)
  (emacs-wiki-publish-this-page)
  (browse-url (emacs-wiki-published-file)))



(provide 'wiki-setting)

;;; vm-setting.el end here
