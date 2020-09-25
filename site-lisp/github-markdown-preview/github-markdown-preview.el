;;; github-markdown-preview.el --- Preview Github markdowned text with browser

;; Copyright (C) 2015  alpha22jp <alpha22jp@gmail.com>

;; Author: alpha22jp <alpha22jp@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((request-deferred "0.2.0"))
;; Keywords: github markdown
;; URL: https://github.com/alpha22jp/github-markdown-preview.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Preview markdowned text with browser using Github markdown render API.

;;; Code:

(require 'request-deferred)

(defconst github-markdown-preview-lib-dir
  (concat (expand-file-name (file-name-directory load-file-name)) "etc/"))

(defvar github-markdown-preview-cache-dir
  (expand-file-name "~/.emacs.d/cache/"))

;;;###autoload
(defun github-markdown-preview-setup ()
  "Setup github-markdown-preview to work."
  (interactive)
  (unless (file-directory-p github-markdown-preview-cache-dir)
    (make-directory github-markdown-preview-cache-dir t)
    (copy-file (concat github-markdown-preview-lib-dir "github.css")
               (concat github-markdown-preview-cache-dir "github.css"))))

;;;###autoload
(defun github-markdown-preview ()
  "Preview current buffer with browser as Github markdowned text."
  (interactive)
  (github-markdown-preview-setup)
  (deferred:$
    (request-deferred
     "https://api.github.com/markdown/raw"
     :type "POST"
     :data (buffer-string)
     :headers '(("Content-Type" . "text/x-markdown"))
     :parser 'buffer-string)
    (deferred:nextc it
      (lambda (res)
        (if (request-response-error-thrown res)
            (message "Error: %s" res)
          (let ((data (request-response-data res))
                (file (expand-file-name
                       (concat github-markdown-preview-cache-dir "view.html")))
                (template (concat github-markdown-preview-lib-dir "template.html")))
            (write-region
             (with-temp-buffer
               (insert-file-contents template)
               (replace-string "%data%" data)
               (buffer-string))
             nil file nil)
            (browse-url file)))))))

(provide 'github-markdown-preview)

;;; github-markdown-preview.el ends here
