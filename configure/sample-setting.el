;;;; sample-setting.el --- sample config file
;;;

;;my unicad enable/disable switch
(defun my-unicad-switch ()
  "unicad enable/disable switch"
  (interactive)
  (if (eq t unicad-global-enable)
    (progn (setq unicad-global-enable nil)
           (message "unicad is disabled"))
    (progn (setq unicad-global-enable t)
           (message "unicad is enabled"))))

;;using spaces repalce tabs
(defun untabify-buffer ()
  "untabify whole buffer"
  (interactive)
  (untabify (point-min) (point-max))
  (save-buffer))

;;using tabs repalce spaces
(defun tabify-buffer ()
  "tabify whole buffer"
  (interactive)
  (tabify (point-min) (point-max))
  (save-buffer))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;;File coding for Unix Dos Mac switcher
(defvar my-os-flag 0)
(defun my-os-file-switch ()
    "for unix dos max"
    (interactive)
    (if (> my-os-flag 2)
      (setq my-os-flag 0))
    (cond ((eq my-os-flag 0)
           (set-buffer-file-coding-system 'unix 't)
           (message "end line with LF"))
          ((eq my-os-flag 1)
           (set-buffer-file-coding-system 'dos 't)
           (message "end line with CRLF"))
          ((eq my-os-flag 2)
           (set-buffer-file-coding-system 'mac 't)
           (message "end line with CR")))
    (setq my-os-flag (+ my-os-flag 1)))

;;; Maximum Windows Frame
(defvar my-fullscreen-p t "Check if fullscreen is on or off")
(defun my-sub-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil
                           'fullboth)))

(defun my-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND restore #xf120
	  (w32-send-sys-command 61728)
      (run-with-idle-timer 0.1 nil 'my-sub-fullscreen)))

(defun my-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND maximaze #xf030
	  (w32-send-sys-command 61488)
      (run-with-idle-timer 0.1 nil 'my-sub-fullscreen)))

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (my-non-fullscreen)
	(my-fullscreen)))

(defvar my-maxframe-p t "Check if maxframe is on or off")
(defun my-toggle-maxframe ()
  (interactive)
  (setq my-maxframe-p (not my-maxframe-p))
  (if my-maxframe-p
	  (restore-frame)
	(maximize-frame)))

;;display current buffer name
(defun display-buffer-name ()
  (interactive)
  (message (buffer-file-name (current-buffer))))

;;set file to utf-8
(defun my-utf-8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer)
  (message "this file to utf-8 ok!"))

(defun switch-to-scratch ()
  "switch to *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*")
  (message "switch to *scratch*"))

(defun switch-to-compilation ()
  "switch to *compilation* buffer"
  (interactive)
  (switch-to-buffer "*compilation*")
  (message "switch to *compilation*"))

(defun edit-with-gvim()
  "Edit current buffer file with gvim"
  (interactive)
  (start-process  "gvim"
                  nil
                  "gvim"
                  (display-buffer-name)))


(defun open-with-terminal()
  "Open terminal to current dired"
  (interactive)
  (start-process  "gnome-terminal"
                  nil
                  "gnome-terminal"
                  ""
                  ))

(defun open-with-nautilus()
  "Open nautilus on current dired"
  (interactive)
  (start-process  "nautilus"
                  nil
                  "nautilus"
                  "--no-desktop"
                  ""
                  ))

;;go to last buffer
(defun my-last-buffer-go ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun line-to-top-of-window ()
    (interactive)
    (recenter 0))

;; used in my ion C-F2 key binding. run before shutting down X!
(defun delete-all-x-frames ()
  (mapcar (lambda (frame) (if (eq 'x (frame-live-p frame))
                              (delete-frame frame)))
          (frame-list)))

(defun make-my-frames ()
  (interactive)
  (make-frame '((name . "emacs main")))
  (make-frame '((name . "emacs secondary")))
  )

;;file transform
(defun my-dos2unix (buffer)
  "Automate M-% C-q C-m RET RET !" 
  (interactive "*b") 
  (save-excursion 
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

(defun dos2unix ()
  "Automate M-% C-q C-m RET RET !"
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun unix2dos ()
  "Automate M-% C-q C-j RET C-q C-m C-q C-j RET !"
  (interactive)
  (set-buffer-file-coding-system 'dos))

;;occur setting
(defun my-occur (&optional arg)
  "Switch to *Occur* buffer, or run `occur'.
   Without a prefix argument, switch to the buffer.
   With a universal prefix argument, run occur again.
   With a numeric prefix argument, run occur with NLINES
   set to that number."
  (interactive "P")
  (if (and (not arg) (get-buffer "*Occur*"))
      (switch-to-buffer "*Occur*")
      (occur (completing-read "Search Term: " nil nil nil (thing-at-point 'word))
             (if (listp arg) 0 arg))))

;;emacs with sudo
(defun zz-sudo-find-file (file dir)
  (find-file (concat "/sudo:localhost:" (expand-file-name file dir))))

;;open special info file
(defun zz-info-open-file (dir-name)
  "Create tags file."
  (interactive "FInfo file: ")
  (info dir-name))

(defun my-insert-numbers (arg)
  "insert number into a file, starting with 1   -mdf"
  (interactive "NHow many numbers to insert: ")
  (setq i 0)
  (while (< i arg)
    (setq i (1+ i))
    (insert (int-to-string i))
	(backward-char 1)
    (next-line 2)
    (beginning-of-line 0)
    )
  )

(defun my-trans-path-sep (path origin-sep target-sep)
  (let ((tmp ""))
    (dolist (item (split-string path origin-sep))
      (setq tmp (concat tmp item target-sep)))
    (setq tmp (substring tmp 0 -2))
    tmp))

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

;;multi key setting
(defun apply-keys-to-map (map key-pairs)
  "apply multi key defines"
  (let ((i 0))
    (while (< i (length key-pairs))
      (let ((key (nth i key-pairs))
            (fn (nth (1+ i) key-pairs)))
        (when fn
          (define-key map key fn)))
      (setq i (+ i 2)))))

;;group define fn key
(defun define-fn-key (fn-name
                      fn-sym        fn
                      s-fn-sym      s-fn
                      c-fn-sym      c-fn
                      m-fn-sym      m-fn
                      ctrl-x-fn-sym ctrl-x-fn
                      ctrl-c-fn-sym ctrl-c-fn
                      &optional doc)
  (when (and fn-sym fn) 
    (define-key global-map       fn-sym         fn)
    (define-key zz/fn-map        fn-name        fn))
  (when (and s-fn-sym s-fn)
    (define-key global-map       s-fn-sym       s-fn)
    (define-key zz/shift-fn-map  fn-name        s-fn))
  (when (and c-fn-sym c-fn)
    (define-key global-map       c-fn-sym       c-fn)
    (define-key zz/ctrl-fn-map   fn-name        c-fn))
  (when (and m-fn-sym m-fn)
    (define-key global-map       m-fn-sym       m-fn)
    (define-key zz/alt-fn-map    fn-name        m-fn))
  (when (and ctrl-x-fn-sym ctrl-x-fn)
    (define-key global-map       ctrl-x-fn-sym  ctrl-x-fn)
    (define-key zz/ctrl-x-fn-map fn-name        ctrl-x-fn))
  (when (and ctrl-c-fn-sym ctrl-c-fn)
    (define-key global-map       ctrl-c-fn-sym  ctrl-c-fn)
    (define-key zz/ctrl-c-fn-map fn-name        ctrl-c-fn)))


(provide 'sample-setting)

;;; sample-setting.el ends here
