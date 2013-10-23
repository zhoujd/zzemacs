;;;; proj-helper.el --- help utils common file
;;http://www.tuicool.com/articles/QFZfq2

;;inc-num-region
(defun inc-num-region (p m)
  "Increments the numbers in a given region"
  (interactive "r")
  (save-restriction
    (save-excursion
      (narrow-to-region p m)
      (goto-char (point-min))
      (forward-line)
      (let ((counter 1))
        (while (not (eq (point)
                        (point-max)))
          (goto-char (point-at-eol))
          (search-backward-regexp "[0-9]+" (point-at-bol) t)
          (let* ((this-num (string-to-number (match-string 0)))
                 (new-num-str (number-to-string (+ this-num
                                                   counter))))
            (replace-match new-num-str)
            (incf counter)
            (forward-line)))))))

;;add code review note
(defun add-code-review-note ()
  "Add note for current file and line number"
  (interactive)
  (let ((file-name (buffer-file-name))
        (file-line (line-number-at-pos)))
    (switch-to-buffer-other-window (get-buffer-create "NOTES"))
    (goto-char (point-min))
    (when (not (search-forward "-*- mode:compilation-shell-minor"
                               nil t))
      (compilation-shell-minor-mode 1)
      (insert "-*- mode:compilation-shell-minor -*-\n\n"))
    (goto-char (point-max))
    (if (/= (current-column) 0)
        (newline))
    (insert file-name ":" (number-to-string file-line) ": ")))

;;automatic include guards
(defun get-include-guard ()
  "Return a string suitable for use in a C/C++ include guard"
  (let* ((fname (buffer-file-name (current-buffer)))
         (fbasename (replace-regexp-in-string ".*/" "" fname))
         (inc-guard-base (replace-regexp-in-string "[.-]"
                                                   "_"
                                                   fbasename)))
    (concat (upcase inc-guard-base) "_")))

(add-hook 'find-file-not-found-hooks
          '(lambda ()
             (let ((file-name (buffer-file-name (current-buffer))))
               (when (string= ".h" (substring file-name -2))
                 (let ((include-guard (get-include-guard)))
                   (insert "#ifndef " include-guard)
                   (newline)
                   (insert "#define " include-guard)
                   (newline 4)
                   (insert "#endif")
                   (newline)
                   (previous-line 3)
                   (set-buffer-modified-p nil))))))

;;jump between files in the same directory with the same basename. 
(defun next-file-with-basename ()
  "Cycles between files with the same basename as the given file.
   Usefull for cycling between header .h/.cpp/.hpp files etc."
  (interactive)
  (let* ((buf-file-name (replace-regexp-in-string
                         "^.*/" ""
                         (buffer-file-name)))
         (current-dir (replace-regexp-in-string
                       "[a-zA-Z0-9._-]+$" ""
                       (buffer-file-name)))
         (no-basename (equal ?. (aref buf-file-name 0)))
         (has-extension (find ?. buf-file-name)))
    ;; If the file is a .dot-file or it doesn't have an
    ;; extension, then there's nothing to do here.
    (unless (or no-basename (not has-extension))
      (let* ((basename (replace-regexp-in-string
                        "\\..*" ""
                        buf-file-name))
             (files-with-basename (directory-files
                                   current-dir t
                                   (concat "^" basename "\\."))))
        ;; If there's only 1 file with this basename, nothing to
        ;; do
        (unless (= (length files-with-basename) 1)
          ;; By making the list circular, we're guaranteed that
          ;; there will always be a next list element (ie. no
          ;; need for special case when file is at the end of
          ;; the list).
          (setf (cdr (last files-with-basename))
                files-with-basename)
          (find-file (cadr (member (buffer-file-name)
                                   files-with-basename))))))))

;;small snippets of boilerplate code
(defun j-newline-and-indent ()
  "Same as \"newline-and-indent\" except it also expands 
   c-macros if it sees one."
  (interactive)
  (if (and (equal (char-before) ?\))
           (macro-function (car (preceding-sexp))))
      ;; This is a c-macro
      (expand-c-macro-in-place)
    (newline-and-indent)))

(defun macro-function (name)
  "Given a name, returns the c-macro-name symbol if it 
   exists as a function"
  (let ((macro-sym (intern (concat "c-macro-"
                                   (symbol-name name)))))
    (if (fboundp macro-sym)
        macro-sym
      nil)))

(defun expand-c-macro-in-place ()
  "Given that point is at the end of a c-macro, expands
   it in-place"
  (let* ((sexp (preceding-sexp))
         (macro-name (car sexp))
         (replacement-text (apply (macro-function macro-name)
                                  (cdr sexp)))
         (jump-to (string-match "!!!BODY!!!;" replacement-text)))
    ;; Delete macro invocation
    (backward-list)
    (let ((start-del (point)))
      (forward-list)
      (kill-region start-del (point))

      ;; Insert macro expansion and indent appropriately
      (insert replacement-text)
      (indent-region start-del (point))
      (when jump-to
        (search-backward "!!!BODY!!!;")
        (kill-line))))
  (c-indent-command))

;;(doit std::vector<int> myContainer)
;;at end of the previous line, hit C-j and this expands to:
(defun c-macro-doit (container-type arg1 &optional arg2)
  "Emits code for iterating over an stl (or stl-like) structure"
  (let ((iterator-name  (if arg2 arg1 "it"))
        (container-name (if arg2 arg2 arg1)))
    (format (concat "for (%s::iterator %s = %s.begin();\n"
                    "     %s != %s.end();\n"
                    "     ++%s)\n"
                    "{\n"
                    "   !!!BODY!!!;\n"
                    "}\n")
            container-type
            iterator-name
            container-name
            iterator-name
            container-name
            iterator-name)))

;;(api-fn)
;;expands to
(defun c-macro-api-fn ()
  "Emits code for wrapping an api function in a try/catch block"
  (concat "try\n"
          "{\n"
          "   !!!BODY!!!;\n"
          "}\n"
          "catch(const std::exception& e)\n"
          "{\n"
          "   TRACE(\"Unhandled exception in function %s: %s\\n\",\n"
          "         __func__, e.what());\n"
          "   return -1;\n"
          "}\n"))



(provide 'proj-helper)

;;; proj-helper.el ends here
