;;;; w3m-setting.el --- w3m setting file
;;;; git clone https://github.com/doitian/emacs-w3m.git

(zz-load-path "site-lisp/emacs-w3m")
(require 'w3m-load)

(defun w3m-build-in-emacs ()
  "compile emacs-w3m"
  (interactive)
  (apply
   'start-process
   "my-w3m-cmp"
   nil
   (split-string "emacs -batch -q -no-site-file -l w3mhack.el NONE -f w3mhack-nonunix-install"))
  (message "build emacs-w3m ok"))

;;(setq exec-path (cons "~/bin" exec-path))
;;(setq w3m-command (concat "~/bin/w3m.exe" ""))
;;(setq w3m-browse-url (concat "~/bin/w3m.exe" ""))
;;(setq w3m-find-file (concat "~/bin/w3m.exe" ""))

(setq browse-url-browser-function 'w3m-browse-url)
;;(setq w3m-local-find-file-function nil)

;;; Allow browsing of local files:
(setq w3m-dirlist-cgi-program "~/site-lisp/emacs-w3m/dirlist.cgi")
;;; causes the return key to submit a form
(setq w3m-use-form t)
(setq w3m-use-mule-ucs t)
(setq w3m-use-toolbar t)  
(setq w3m-use-cookies t)
(setq w3m-display-inline-image t)

;;(setq w3m-bookmark-file-coding-system 'chinese-iso-8bit)
;;(setq w3m-coding-system 'chinese-iso-8bit)
;;(setq w3m-default-coding-system 'chinese-iso-8bit)
;;(setq w3m-file-coding-system 'chinese-iso-8bit)
;;(setq w3m-file-name-coding-system 'chinese-iso-8bit)
;;(setq w3m-terminal-coding-system 'chinese-iso-8bit)
;;(setq w3m-input-coding-system 'chinese-iso-8bit)
;;(setq w3m-output-coding-system 'chinese-iso-8bit)

(setq w3m-tab-width 4)
(setq w3m-fill-column 120);;
(setq w3m-home-page "http://www.google.com/")
(setq w3m-view-this-url-new-session-in-background t)

(add-hook 'w3m-fontify-after-hook 'remove-w3m-output-garbages)
(defun remove-w3m-output-garbages ()
      (interactive)
      (let ((buffer-read-only))
        (setf (point) (point-min))
        (while (re-search-forward "[\200-\240]" nil t)
          (replace-match " "))
        (set-buffer-multibyte t))
      (set-buffer-modified-p nil)) 

;;(setq w3m-command-arguments
;;              (nconc w3m-command-arguments
;;                     '("-o" "http_proxy=http://proxy.hogege.com:8000/")))

;;(setq w3m-no-proxy-domains '("local.com" "neighbor.com"))
;;

(provide 'w3m-setting)

;;; w3m-setting.el ends here
