

(require 'ert)
(require 'prf-tramp-friendly-parsing)




(ert-deftest prf-tramp-get-method-from-path-test ()
  "Ensure TRAMP method gets correctly extracted from pseudo-tramp path."
  (mapc (lambda (item)
          (let ((path (car item))
                (expected (cdr item)))
            (eval `(should (string= (prf/tramp/get-method-from-path ,path) ,expected)))))
        '(("/method:user@host" . "method")
          ("/method:host" . "method"))))

(ert-deftest prf-tramp-get-user-from-path-test ()
  "Ensure user gets correctly extracted from pseudo-tramp path."
  (mapc (lambda (item)
          (let ((path (car item))
                (expected (cdr item)))
            (eval `(should
                    (string= (prf/tramp/get-user-from-path ,path) ,expected)))))
        '(
          ;; ("/method:user%domain@host" . "user")
          ("/method:user@host" . "user")
          ("user@host" . "user"))))

(ert-deftest prf-tramp-get-host-from-path-test ()
  "Ensure host gets correctly extracted from pseudo-tramp path."
  (mapc (lambda (item)
          (let ((path (car item))
                (expected (cdr item)))
            (eval `(should (string= (prf/tramp/get-host-from-path ,path) ,expected)))))
        '(("/method:user%domain@host" . "host")
          ("/method:user@host" . "host")
          ("/method:user@host:" . "host")
          ("/method:user@host:/" . "host")
          ("user@host" . "host")
          ("user@host:" . "host")
          ("user@host/" . "host"))))

;; (ert-deftest prf-tramp-get-port-from-path-test ()
;;   "Ensure host gets correctly extracted from pseudo-tramp path."
;;   (mapc (lambda (item)
;;           (let ((path (car item))
;;                 (expected (cdr item)))
;;             (eval `(should (string= (prf/tramp/get-host-port-path ,path) ,expected)))))
;;         '(
;;           ("/method:user%domain@host" . nil)
;;           ("/method:user@host" . nil)
;;           ("/method:user@host:" . nil)
;;           ("/method:user@host:/" . nil)
;;           ("user@host" . nil)
;;           ("user@host:" . nil)
;;           ("user@host:/" . nil)
;;           )))

(ert-deftest prf-tramp-get-localname-from-path-test ()
  "Ensure localname gets correctly extracted from pseudo-tramp path."
  (mapc (lambda (item)
          (let ((path (car item))
                (expected (cdr item)))
            (eval `(should (string= (prf/tramp/get-localname-from-path ,path) ,expected)))))
        '(
          ("/method:user@host" . nil)
          ("/method:user@host:" . nil)
          ("/method:user@host:/" . "/")
          ("user@host" . nil)
          ("user@host:" . nil)
          ("user@host:/" . "/")
          ("host" . nil)
          ("host:" . nil)
          ("host:/" . "/")
          )))




;;; prf-tramp-test.el ends here
