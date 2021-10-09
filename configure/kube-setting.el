;;;; kube-setting.el

;;https://github.com/chrisbarrett/kubernetes-el
;;magit-style interface to the Kubernetes
;M-x kubernetes-overview
(zz:load-path "site-lisp/kubernetes-el")
(require 'kubernetes)

;;https://github.com/abrochard/kubel
;M-x kubel-set-namespace
;M-x kubel-set-context
;(require 'kubel)

;;kubernetes helm
(require 'kubernetes-helm)


(provide 'kube-setting)

;;;; kube-setting.el --- end here
