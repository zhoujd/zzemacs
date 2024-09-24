;;;; kube-setting.el

;;https://github.com/chrisbarrett/kubernetes-el
;;magit-style interface to the Kubernetes
;M-x kubernetes-overview
(zz/load-path "site-lisp/kubernetes-el")
(require 'kubernetes)

;;https://github.com/abrochard/kubel
;M-x kubel-set-namespace
;M-x kubel-set-context
;(require 'kubel)

;;kubernetes helm
(require 'kubernetes-helm)

;;kubernets tramp
(require 'kubernetes-tramp)


(provide 'kube-setting)

;; Local Variables:
;; coding: utf-8
;; End:
;;; kube-setting.el --- end here
