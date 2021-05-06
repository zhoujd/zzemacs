Quicklisp command list
=========================

;;set proxy if need
(setq ql:*proxy-url* "http://host:port")

;;upgrade quicklisp
(ql:update-client)

;;update system
(ql:update-dist "quicklisp")

;;search system
(ql:system-apropos "vector")

;;install software
(ql:quickload "quicklisp-slime-helper")

;;remove software
(ql-dist:uninstall (ql-dist:release "babel"))
(ql-dist:clean (ql-dist:dist "quicklisp"))

;;list installed software
(ql-dist:installed-releases (ql-dist:dist "quicklisp"))
(ql:where-is-system "cl-ppcre")
(ql:register-local-projects)

;;:dist-url can be used to specify the initial dist version to use at installation time.
;;Valid URLs can be obtained from an existing Quicklisp installation by
;;evaluating one of the new functions (ql:dist-url "quicklisp") or (ql:available-dist-versions "quicklisp")

;;:client-url can be used to specify the initial client version to use at installation time.
;;Valid URLs can be obtained from an existing Quicklisp installation by
;;evaluating one of the new functions (ql:client-url) or (ql:available-client-versions)

