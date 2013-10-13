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
  
  (kbd "`")  zz/ctrl-map
  [escape]   zz/meta-map
  
  [f7]       zz/ctrl-x-fn-map
  [f8]       zz/ctrl-c-fn-map
  
  [f9]       zz/fn-map
  [f10]      zz/shift-fn-map
  [f11]      zz/ctrl-fn-map
  [f12]      zz/meta-fn-map 
  ))

(apply-keys-to-map
 ctl-z-map
 (list

  (kbd "`")    zz/ctrl-map
  [escape]     zz/meta-map
  
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


(provide 'keymap-setting)

;;; keymap-setting.el ends here
