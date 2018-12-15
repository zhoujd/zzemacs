;;;; keymap-setting.el --- key function config file
;;;

(zz-load-path "elisp")
(require 'apply-keys)

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
;;suspend-frame ctrl-x ctrl-z
(defvar ctl-z-map        (make-sparse-keymap) "ctl-z-map for self functions.")
(defvar ctrl-x-fn-map    (make-sparse-keymap) "ctrl-x f1/f12")
(defvar ctrl-c-fn-map    (make-sparse-keymap) "ctrl-c f1/f12")
(defvar fn-map           (make-sparse-keymap) "f1/f12")
(defvar shift-fn-map     (make-sparse-keymap) "S-f1/f12")
(defvar ctrl-fn-map      (make-sparse-keymap) "C-f1/f12")
(defvar meta-fn-map      (make-sparse-keymap) "M-f1/f12")

(defvar f4-map           (make-sparse-keymap) "f4 map for self functions.")
(defvar f4-e-map         (make-sparse-keymap) "f4-e for execute functions.")
(defvar f4-p-map         (make-sparse-keymap) "f4-p for execute functions, in temp-setting.el.")

;;keymap setting
(apply-keys-to-map
 global-map
 (list
  (kbd "C-z")   ctl-z-map
  (kbd "M-[")   ctl-z-map
  (kbd "M-]")   f4-map
  [f4]          f4-map
  ))

;;fn-key entry access
(apply-keys-to-map
 help-map
 (list
  ;;self f4-map
  (kbd "$")     f4-map

  ;;self f4-e-map/f4-p-map
  [f5]          f4-e-map
  [f6]          f4-p-map

  ;;self ctl-x/ctl-c
  [f7]          ctrl-x-fn-map
  [f8]          ctrl-c-fn-map

  ;;self fn relative
  [f9]          fn-map
  [f10]         shift-fn-map
  [f11]         ctrl-fn-map
  [f12]         meta-fn-map
  ))

(apply-keys-to-map
 ctl-z-map
 (list
  ;;self ctl-x/ctl-c
  (kbd "C-x")   ctrl-x-fn-map
  (kbd "C-c")   ctrl-c-fn-map

  ;;self fn relative
  (kbd "f")     fn-map
  (kbd "s")     shift-fn-map
  (kbd "c")     ctrl-fn-map
  (kbd "m")     meta-fn-map

  ;;self f4-map
  (kbd "$")     f4-map
  (kbd "e")     f4-e-map
  (kbd "p")     f4-p-map
  ))

(apply-keys-to-map
 f4-map
 (list
  (kbd "e")     f4-e-map
  (kbd "p")     f4-p-map
  ))

(provide 'keymap-setting)

;;; keymap-setting.el ends here
