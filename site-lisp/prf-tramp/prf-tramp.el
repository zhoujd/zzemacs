;;; prf-tramp.el --- Wrapper around tramp

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: tramp
;; URL: https://github.com/p3r7/prf-tramp
;; Package-Requires: ((s "1.11.0"))
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from
;; the use of this software.

;;; Commentary:
;;  -----------
;;
;; For detailed instructions, please look at the README.md


;;; Code:



;; REQUIRES

(require 'tramp)
(require 'tramp-sh)

(require 'cl-lib)
(require 's)



;; BETTER DEFAULTS

(defun prf/tramp-register-better-defaults ()
  (setq tramp-persistency-file-name "/tmp/.tramp")
  (setq tramp-default-user "root"))



;; UTILS: FILESYSTEM

(defun prf/sys/touch (&optional filename)
  "touch specified file or current buffer"
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (or filename (buffer-file-name)))))
  (clear-visited-file-modtime))



;; UTILS: EXEC-FIND

(defun prf/tramp/executable-find-remote (command &optional _args)
  "Drop in replacement for `executable-find' for remote *nix servers.
The unused ARGS param makes it also a replacement for `eshell-find-interpreter' which is slow with tramp connections."
  (let (errno
        ;; (get-path-cmd (concat "which "command))
        (get-path-cmd (concat "command -v "command)))
    (with-temp-buffer
      (setq errno (shell-command get-path-cmd (current-buffer)))
      (when (eq errno 0)
        ;; NB: removing trailing \n
        (substring (buffer-string) 0 -1)))))

(defun prf/tramp/executable-find (command &optional _args)
  "Drop in replacement for `executable-find' w/ support for remote *nix servers.
The unused ARGS param makes it also a replacement for `eshell-find-interpreter' which is slow with tramp connections."
  (if (file-remote-p default-directory)
      (prf/tramp/executable-find-remote command)
    (executable-find command)))



;; UTILS: SHELL INTERPRETER

(defun prf/tramp/get-interpreter-name (interpreter)
  (file-name-nondirectory interpreter))

(defun prf/tramp/get-interpreter-name-noExt (interpreter)
  (file-name-sans-extension (file-name-nondirectory interpreter)))



;; UTILS: PATH SANITIZE / NOMALIZE

(defun prf/tramp/sanitize-path (path)
  "Used by remote-shell"

  ;; trim
  (setq path (s-trim path))

  ;; remove protocol prefix
  (if (string-match (rx
                     line-start
                     (or "http" "https" "ftp") "://"
                     (group (one-or-more (any "a-z" "A-Z" "0-9" ".")))
                     (zero-or-more ":") (zero-or-more "/"))
                    path)
      (match-string 1 path)
    path))

(defun prf/tramp/path/normalize (path)
  "Normalize path, converting \\ into /."
  ;; REVIEW: shouldn't we just use instead `convert-standard-filename'
  ;; or even `executable-find'?
  (subst-char-in-string ?\\ ?/ path))



;; UTILS: PATH MODIFY / RECONSTRUCT

(defun prf/tramp/vec/update-localname (vec localname)
  (setcar (nthcdr 6 vec) localname))

(defun prf/tramp/vec/with-new-localname (vec localname)
  (let (vec2)
    (setq vec2 (copy-sequence vec))
    (prf/tramp/vec/update-localname vec2 localname)
    vec2))

(defun prf/tramp/path/with-new-localname (path new-localname)
  (let* ((vec (tramp-dissect-file-name default-directory))
         (new-vec (prf/tramp/vec/with-new-localname vec new-localname)))
    (prf/tramp/vec/undissect new-vec)))



;; UTILS: PATH / VEC

(defun prf/tramp/vec/undissect (vec)
  "Converts VEC back into path"
  (let ((method (tramp-file-name-method vec))
        (user (tramp-file-name-user vec))
        (domain (tramp-file-name-domain vec))
        (host (tramp-file-name-host vec))
        (port (tramp-file-name-port vec))
        (localname (tramp-file-name-localname vec)))

    ;; TODO: take into account hops (last optionnal arg)
    (if (>= emacs-major-version 26)
        (tramp-make-tramp-file-name method user domain host port localname)
      ;; REVIEW: how to handle domain & port?
      (tramp-make-tramp-file-name method user host localname))))

(defun prf/tramp/path/remote-p (path)
  "Tests that a path would be a valid remote file-name.
Might serve as amore lightweight version of `file-remote-p'."
  (let ((match (string-match (nth 0 tramp-file-name-structure) path)))
    (when match
      't)))

(defun prf/tramp/vec/vec-p (maybe-vec)
  (eq (car maybe-vec) 'tramp-file-name))



;; MAIN: KILLING SHELL

(defun prf/tramp/send-C-d ()
  "Cleanly kill the shell buffer.
Sends exit signal to buffer and if properly exited (no more process running) kill it wo/ prompting."
  (interactive)
  (let* ((prf/tramp/interpreterName (prf/tramp/get-interpreter-name shell-file-name))
         (comint-buffer-process (get-buffer-process (current-buffer)))
         (comint-buffer-process-name (process-name comint-buffer-process))
         (beg (point))
         (comint-buffer (current-buffer))
         (shell-exit-command))
    (goto-char (point-max))
    (if (or (and (boundp 'w32-system-shells)
                 (member prf/tramp/interpreterName w32-system-shells))
            (equal prf/tramp/interpreterName "cmdproxy.exe"))
        (setq shell-exit-command "exit")
      (setq shell-exit-command ""))

    (comint-simple-send comint-buffer-process (concat shell-exit-command "\n"))

    ;; NB: my first idea was to register a special hook in `comint-output-filter-functions' (takes 2 args) to search the end of (buffer-string) for (concat "Process " 'process-name prf/tramp/comint-buffer-process) "")
    ;; wasn't working as the last line wasn't getting caught by this hook

    ;; REVIEW: this seems dirty
    ;; at time of fn exec, the buffer-process is not yet empty
    ;; this might not wokr when using fakecygpty
    (run-at-time
     0.1 nil
     `(lambda ()
        (with-current-buffer ,comint-buffer
          (when (not (get-buffer-process (current-buffer)))
            (prf/kill-this-buffer)))))))




(provide 'prf-tramp)

;;; prf-tramp.el ends here.
