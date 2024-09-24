;;;; spell-setting.el --- spell config file
;; https://www.emacswiki.org/emacs/FlySpell
;; sudo apt install ispell

(defun zz/flyspell-on-for-buffer-type ()
  (interactive)
  (if (not (symbol-value flyspell-mode))
      (progn
        (if (derived-mode-p 'prog-mode)
            (progn
              (message "Flyspell on (code)")
              (flyspell-prog-mode))
            (progn
              (message "Flyspell on (text)")
              (flyspell-mode 1))))))

(defun zz/flyspell-toggle ()
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn
        (message "Flyspell off")
        (flyspell-mode -1))
      (zz/flyspell-on-for-buffer-type)))

(defun zz/flyspell-hook ()
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined))
(add-hook 'flyspell-mode-hook 'zz/flyspell-hook)
(add-hook 'flyspell-prog-mode-hook 'zz/flyspell-hook)


(provide 'spell-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; spell-setting.el ends here
