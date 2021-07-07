;;; powerline-zz-theme.el
;;

(require 'powerline)

(defun powerline-zz-theme ()
  "Customisation of the default powerline theme"
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((powerline-default-separator 'box)
                          (active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left
                           (intern
                            (format "powerline-%s-%s"
                                    powerline-default-separator
                                    (car powerline-default-separator-dir))))
                          (separator-right
                           (intern (format "powerline-%s-%s"
                                           powerline-default-separator
                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face0 'l)
                                     (powerline-buffer-id face0 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face0 'l))
                                     (powerline-narrow face0 'l)
                                     (funcall separator-left face0 face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-raw " " face1 'r)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%l" face1)
                                     (powerline-raw ":" face1)
                                     (powerline-raw "%c" face1)
                                     (funcall separator-right face1 face0)
                                     (powerline-raw "%p" face0)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))


(provide 'powerline-zz-theme)

;;; end of powerline-zz-theme.el
