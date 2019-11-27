;;;; complete-setting.el --- complete common file


(zz-load-path "site-lisp")

;; list methold in current buffer
;; switch buffer in h & cpp file
(require 'eassist)
(defkeys-map eassist-mode-map
  ((kbd "TAB") 'eassist-jump-to-method)
  ((kbd "C-b") 'eassist-backspace-pressed)
  ((kbd "C-q") 'eassist-escape))

;; Semantic DataBase
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.d/semanticdb"))

;; hippie-try-expand settings
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs))

(defun zz:expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially
                                            try-complete-file-name)))
    (call-interactively 'hippie-expand)))

;;; https://www.emacswiki.org/emacs/InsertFileName
;;Key sequence	Inserted text
;;C-c C-i ~/.emacs RET	~/.emacs
;;C-u C-c C-i ~/.emacs RET	/home/user/.emacs
;;C-- C-c C-i ~/.emacs RET	.emacs
(defun zz:insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point."
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (expand-file-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (file-relative-name filename)))))

;; bind it
(defkeys-map global-map
  ((kbd "C-c C-i") 'zz:insert-file-name)
  ((kbd "C-M-/")   'zz:expand-file-name-at-point))

(defun zz:indent-or-complete ()
   "complete if point is at end of a word, otherwise indent line"
   (interactive)
   (if (looking-at "\\>")
       (hippie-expand nil)
       (indent-for-tab-command)))

;;YASNIPPET
;;https://github.com/capitaomorte/yasnippet
(zz-load-path "site-lisp/yasnippet")
(zz-load-path "site-lisp/yasnippet-snippets")
(require 'yasnippet-snippets)
(yas-global-mode t)


(provide 'complete-setting)

;;; complete-setting.el ends here
