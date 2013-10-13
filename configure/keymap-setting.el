;;;; keymap-setting.el --- key function config file
;;;

;;;;function key setting on console
;;Ctl-z map
(defvar ctl-z-map        (make-sparse-keymap) "ctl-z-map for self functions.")
(defvar zz/ctrl-map      (make-sparse-keymap) "f1-f5 => C-1/+")
(defvar zz/meta-map      (make-sparse-keymap) "f1-f6 => M-1/+")
(defvar zz/ctrl-x-fn-map (make-sparse-keymap) "f1-f7 => ctrl-x f1/f12")
(defvar zz/ctrl-c-fn-map (make-sparse-keymap) "f1-f8 => ctrl-c f1/f12")
(defvar zz/fn-map        (make-sparse-keymap) "f1-f9 => f1/f12")
(defvar zz/shift-fn-map  (make-sparse-keymap) "f1-f10 => S-f1/f12")
(defvar zz/ctrl-fn-map   (make-sparse-keymap) "f1-f11 => C-f1/f12")
(defvar zz/meta-fn-map   (make-sparse-keymap) "f1-f12 => M-f1/f12")

(defvar f4-map           (make-sparse-keymap) "f4 map for self functions.")
(defvar f4-backquote-map (make-sparse-keymap) "f4-backquote for self help function.")
(defvar f4-e-map         (make-sparse-keymap) "f4-e for execute functions.")
(defvar f4-p-map         (make-sparse-keymap) "f4-p for execute functions, in temp-setting.el.")
(defvar f1-backquote-map (make-sparse-keymap) "f1-backquote for self help function.")

;;keymap setting
(apply-keys-to-map
 global-map
 (list
  (kbd "\C-z") ctl-z-map
  [f4]         f4-map  
  ))

;;fn-key entry access
(apply-keys-to-map
 help-map
 (list
  (kbd "4")  f4-map
  (kbd "`")  f1-backquote-map
  
  [f5]  zz/ctrl-map
  [f6]  zz/meta-map  
  [f7]  zz/ctrl-x-fn-map
  [f8]  zz/ctrl-c-fn-map
  
  [f9]  zz/fn-map
  [f10] zz/shift-fn-map
  [f11] zz/ctrl-fn-map
  [f12] zz/meta-fn-map 
  ))

(apply-keys-to-map
 ctl-z-map
 (list
  (kbd "\C-x") zz/ctrl-x-fn-map
  (kbd "\C-c") zz/ctrl-c-fn-map
  
  (kbd "f")    zz/fn-map
  (kbd "s")    zz/shift-fn-map
  (kbd "c")    zz/ctrl-fn-map
  (kbd "m")    zz/meta-fn-map

  (kbd "4")    f4-map
  ))

(apply-keys-to-map
 f4-map
 (list
  (kbd "`") f4-backquote-map
  (kbd "e") f4-e-map
  (kbd "p") f4-p-map
  ))

;;fn-key-table
(defvar fn-key-table
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "f1"  "1" hash)
    (puthash "f2"  "2" hash)
    (puthash "f3"  "3" hash)
    (puthash "f4"  "4" hash)
    (puthash "f5"  "5" hash)
    (puthash "f6"  "6" hash)
    (puthash "f7"  "7" hash)
    (puthash "f8"  "8" hash)
    (puthash "f9"  "9" hash)
    (puthash "f10" "0" hash)
    (puthash "f11" "-" hash)
    (puthash "f12" "=" hash)
    hash)
  "fn-key-table")

;;group define fn key
(defun define-fn-key
    (fn-name fn-sym fn s-fn-sym s-fn c-fn-sym c-fn m-fn-sym m-fn
     ctrl-x-fn-sym ctrl-x-fn ctrl-c-fn-sym ctrl-c-fn &optional doc)
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
    (define-key zz/meta-fn-map   fn-name        m-fn))
  (when (and ctrl-x-fn-sym ctrl-x-fn)
    (define-key global-map       ctrl-x-fn-sym  ctrl-x-fn)
    (define-key zz/ctrl-x-fn-map fn-name        ctrl-x-fn))
  (when (and ctrl-c-fn-sym ctrl-c-fn)
    (define-key global-map       ctrl-c-fn-sym  ctrl-c-fn)
    (define-key zz/ctrl-c-fn-map fn-name        ctrl-c-fn)))


(provide 'keymap-setting)

;;; keymap-setting.el ends here
