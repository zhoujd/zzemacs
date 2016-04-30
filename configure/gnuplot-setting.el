;;;; gnuplot-setting.el --- gnuplot config file
;;

;;sokoban setting
(zz-load-path "site-lisp/gnuplot-mode")

;;; gnuplot-mode
(autoload 'gnuplot-mode "gnuplot"
  "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot"
  "open a buffer in gnuplot mode" t)
(setq auto-mode-alist
      (append '(("\\.gp$" . gnuplot-mode))
	      auto-mode-alist))


(provide 'gnuplot-setting)

;;; gnuplot-setting.el ends here
