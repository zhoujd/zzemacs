(define-derived-mode sepia-regex-mode text-mode "RE"
  "Mode for looking at 'use re \"debug\"' output."
  (view-mode 1)
  (font-lock-add-keywords
   'sepia-regex-mode
   '(("^\\(Compiling REx\\|Guessing start of match\\|Matching REx\\|Freeing REx:\\)" . font-lock-keyword-face)
     ("^Match .*" . font-lock-warning-face)
     ;; ("^Matches word.*" . font-lock-keyword-face)
     ;; ("^Matching stclass.* against.*" . font-lock-keyword-face)
     ("^ +\\([0-9]+\\)[: ]" 1 font-lock-constant-face)
     ("^ *[0-9]+ <\\([^>]*\\)>" 1 font-lock-string-face)
     ("^ *[0-9]+ <[^>]*> <\\([^>]*\\)>" 1 font-lock-string-face)
     ("^ *[0-9]+.*?\\(|.*\\)" 1 font-lock-comment-face)
     )
   'set)
  (modify-syntax-entry ?\" "\"")
  (font-lock-fontify-buffer)
  (toggle-truncate-lines 1))

(defun sepia-test-regex (pat str)
  (interactive "Smatch pattern: \nSAgainst string: \n")
  "Show the debug output for compiling `pat' and matching against `str'.

XXX: this doesn't work, because I can't figure out how to
redirect the debug output to a file."
  (with-current-buffer (get-buffer-create "*sepia-regex*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (shell-command (format "perl -Mre=debug -e 'q#%s# =~ m/%s/'" str pat)
                     (current-buffer))
      ;; (insert (sepia-eval-raw
      ;;          (format "do { use re 'debug'; qq#%s# =~ m#%s# }" str pat)))
      )
    (sepia-regex-mode)
    (pop-to-buffer (current-buffer))))
