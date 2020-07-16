;;; server-sample.el


(defun zz:use-server-mode ()
  ;;server-mode
  ;;emacsclientw.exe -f "~\.emacs.d\server\server" -n -a "runemacs.exe" path\to\file
  ;;emacsclientw.exe --server-file ~\.emacs.d\server\server -n -a runemacs.exe path\to\file
  (defvar server-directory-name "~/.emacs.d/server")
  (when-ms-windows
   (when (not (file-directory-p server-directory-name))
     (make-directory server-directory-name t)))

  (require 'server)
  (when-ms-windows
   ;;suppress error "directory
   ;;~/.emacs.d/server is unsafe on windows.
   (defun server-ensure-safe-dir (dir) "Noop" t))
  (unless (server-running-p)
    (server-start))

  (add-hook 'kill-emacs-hook
            (lambda ()
              (if (file-exists-p  (concat server-directory-name "/server"))
                  (delete-file (concat server-directory-name "/server")))))

  (message "start emacs server..."))

(when-ms-windows
 (defun zz:use-gnusvr ()
   ;; start gnuserv on Windows
   (progn
     (require 'gnuserv)
     (setq server-done-function 'bury-buffer gnuserv-frame (car (frame-list)))
     (gnuserv-start)
      ;;; open buffer in existing frame instead of creating new one...
     (setq gnuserv-frame (selected-frame))
     (message "gnuserv started."))))

(if-ms-windows
 (let ((use-gnusvr-flag nil))
   (if use-gnusvr-flag
       (zz:use-gnusvr)
       (zz:use-server-mode)))
 (zz:use-server-mode))


(provide 'server-sample)
