;;;this muse setting

(zz-load-path "site-lisp/muse/lisp")

(require 'muse-mode)

(require 'muse-html)        
(require 'muse-latex)       
(require 'muse-texinfo)     
(require 'muse-docbook)     
(require 'muse-wiki nil t)   
(require 'muse-project)     

;;set code format to utf-8
(setq muse-html-meta-content-type (concat "text/html; charset=utf-8"))

;;html charset
(setq muse-html-charset-default "utf-8")

;;html encoding
(setq muse-html-encoding-default "utf8")

;;add my wiki project
(setq muse-project-alist
      '(("MyWiki"
         ("~/wiki" :default "index")
         (:base "html" :path "~/wiki/publish"))))


(provide 'muse-setting)

;;; muse-setting.el
