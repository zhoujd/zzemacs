

# Extending & Modifying TRAMP Methods

#### Make Cygwin SSH play nice with emacs

```el
(use-package fakecygpty
  :quelpa (fakecygpty :fetcher github :repo "d5884/fakecygpty")
  :if (executable-find "fakecygpty")
  :after (tramp prf-tramp-method)
  :config
  (fakecygpty-activate)

  (defun tramp-cywgin-ssh--get-enriched-tramp-methods ()
    (-map-when
     (lambda (e) (member (car e) '("ssh" "sshx")))
     (lambda (e) (prf/tramp/method-def/with-login-exec e "fakecygpty ssh"))
     tramp-methods))

  (setq tramp-methods (tramp-cywgin-ssh--get-enriched-tramp-methods)))
```

#### Add certificate to putty methods

```el
(let ((cert-path "~/my-cert.ppk")
      (putty-methods '("pscp" "plink" "plinkx" "psftp")))
  (setq tramp-methods
        (-map-when
         (lambda (e) (member (car e) putty-methods))
         (lambda (e) (prf/tramp/method/def/with-cert-in-args e "-i" cert-path))
         tramp-methods)))
```

#### Add certificate to ssh methods

```el
(let ((cert-path "~/.ssh/id_dsa")
      (ssh-methods '("ssh" "sshx")))
  (setq tramp-methods
        (-map-when
         (lambda (e) (member (car e) ssh-methods))
         (lambda (e) (prf/tramp/method/def/with-cert-in-args e "-i" cert-path))
         tramp-methods)))
```
