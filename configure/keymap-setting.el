;;;; keymap-setting.el --- key function config file
;;;

;;;;some keymap define by emacs default
;;;esc-map           --- <ESC>
;;;help-map          --- C-h 
;;;mode-specific-map --- C-c
;;;ctl-x-map         --- C-x
;;;mule-keymap       --- C-x <RET> 
;;;ctl-x-4-map       --- C-x 4 
;;;ctl-x-5-map       --- C-x 5 
;;;2C-mode-map       --- C-x 6 
;;;vc-prefix-map     --- C-x v
;;;goto-map          --- M-g
;;;search-map        --- M-s 
;;;facemenu-keymap   --- M-o
;;;C-x @, C-x a i, C-x <ESC> and <ESC> <ESC> without name 

;;;;function key setting on console
;;Ctl-z map
(defvar ctl-z-map        (make-sparse-keymap) "ctl-z-map for self functions.")
(defvar zz/ctrl-map      (make-sparse-keymap) "C-1/+")
(defvar zz/meta-map      (make-sparse-keymap) "M-1/+")
(defvar zz/ctrl-x-fn-map (make-sparse-keymap) "ctrl-x f1/f12")
(defvar zz/ctrl-c-fn-map (make-sparse-keymap) "ctrl-c f1/f12")
(defvar zz/fn-map        (make-sparse-keymap) "f1/f12")
(defvar zz/shift-fn-map  (make-sparse-keymap) "S-f1/f12")
(defvar zz/ctrl-fn-map   (make-sparse-keymap) "C-f1/f12")
(defvar zz/meta-fn-map   (make-sparse-keymap) "M-f1/f12")

(defvar f4-map           (make-sparse-keymap) "f4 map for self functions.")
(defvar f4-backquote-map (make-sparse-keymap) "f4-backquote for self help function.")
(defvar f4-e-map         (make-sparse-keymap) "f4-e for execute functions.")
(defvar f4-p-map         (make-sparse-keymap) "f4-p for execute functions, in temp-setting.el.")

;;;;emacs default setting
;;C-x @ S        event-apply-shift-modifier
;;C-x @ a        event-apply-alt-modifier
;;C-x @ c        event-apply-control-modifier
;;C-x @ h        event-apply-hyper-modifier
;;C-x @ m        event-apply-meta-modifier
;;C-x @ s        event-apply-super-modifier
(apply-keys-to-map
 key-translation-map
 (list
  [menu]   'event-apply-hyper-modifier
  [apps]   (lookup-key key-translation-map [menu])
  "\C-zh"  (lookup-key key-translation-map [menu])
  ))

;;keymap setting
(apply-keys-to-map
 global-map
 (list
  (kbd "C-z") ctl-z-map
  [f4]         f4-map  
  ))

;;fn-key entry access
(apply-keys-to-map
 help-map
 (list
  (kbd "4")    f4-map
  
  (kbd "SPC")  zz/ctrl-map
  (kbd "`")    zz/meta-map
  
  [f7]         zz/ctrl-x-fn-map
  [f8]         zz/ctrl-c-fn-map
  
  [f9]         zz/fn-map
  [f10]        zz/shift-fn-map
  [f11]        zz/ctrl-fn-map
  [f12]        zz/meta-fn-map 
  ))

(apply-keys-to-map
 ctl-z-map
 (list

  (kbd "SPC")  zz/ctrl-map
  (kbd "`")    zz/meta-map
  
  (kbd "C-x") zz/ctrl-x-fn-map
  (kbd "C-c") zz/ctrl-c-fn-map
  
  (kbd "f")    zz/fn-map
  (kbd "s")    zz/shift-fn-map
  (kbd "c")    zz/ctrl-fn-map
  (kbd "m")    zz/meta-fn-map

  (kbd "4")    f4-map
  ))

(apply-keys-to-map
 f4-map
 (list
  (kbd "`")    f4-backquote-map
  (kbd "e")    f4-e-map
  (kbd "p")    f4-p-map
  ))

(apply-keys-to-map
 global-map
 (list
  (kbd "H-s")   zz/shift-fn-map
  (kbd "H-f")   zz/fn-map
  (kbd "H-c")   zz/ctrl-fn-map
  (kbd "H-m")   zz/meta-fn-map
  (kbd "C-H-x") zz/ctrl-x-fn-map
  (kbd "C-H-c") zz/ctrl-c-fn-map
  ))


(provide 'keymap-setting)

;;; keymap-setting.el ends here
