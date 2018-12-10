;;;; w3m-setting.el --- w3m setting file
;;;http://emacs-w3m.namazu.org/index-en.html
;;;% cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot login
;;;CVS password: # No password is set.  Just hit Enter/Return key.
;;;% cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m

(zz-load-path "site-lisp/emacs-w3m")
(require 'w3m-load)

(defun zz:w3m-build-in-emacs ()
  "compile emacs-w3m"
  (interactive)
  (apply
   'start-process
   "my w3m compile"
   nil
   (split-string
    (format "%s -batch -q -no-site-file -l w3mhack.el NONE -f w3mhack-nonunix-install"
            (if-ms-windows (concat (getenv "EMACS_DIR") "/bin/emacs") "emacs"))))
  (message "build emacs-w3m ok"))

;;;browser selecting
(setq browse-url-browser-function
      (list (cons (concat zzemacs-path "/doc/hyperspec/") 'w3m-browse-url)
            (cons "." 'browse-url-default-browser)))

;;;allow browsing of local files:
(setq w3m-dirlist-cgi-program (concat zzemacs-path "/site-lisp/emacs-w3m/dirlist.cgi"))
;;;causes the return key to submit a form
(setq w3m-use-form t)
(setq w3m-use-mule-ucs t)
(setq w3m-use-toolbar t)  
(setq w3m-use-cookies t)

;;;show images
(setq w3m-toggle-inline-image t)

(setq w3m-tab-width 4)
(setq w3m-fill-column 120);;
(setq w3m-home-page "http://www.google.com/")
(setq w3m-view-this-url-new-session-in-background t)

(defun zz:remove-w3m-output-garbages ()
      (interactive)
      (let ((buffer-read-only))
        (setf (point) (point-min))
        (while (re-search-forward "[\200-\240]" nil t)
          (replace-match " "))
        (set-buffer-multibyte t))
      (set-buffer-modified-p nil)) 

(add-hook 'w3m-fontify-after-hook 'zz:remove-w3m-output-garbages)

;;(setq w3m-command-arguments
;;              (nconc w3m-command-arguments
;;                     '("-o" "http_proxy=http://proxy-prc.*****.com:911/")))
;;(setq w3m-no-proxy-domains '("local.com" "neighbor.com"))


(provide 'w3m-setting)

;;;; w3m-setting.el ends here
