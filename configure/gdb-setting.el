;;;; gdb-setting.el --- gdb common file

;force gdb-mi to not dedicate any windows
(require 'nadvice)
(advice-add 'gdb-display-buffer
            :around (lambda (orig-fun &rest r)
                      (let ((window (apply orig-fun r)))
                        (set-window-dedicated-p window nil)
                        window)))

(advice-add 'gdb-set-window-buffer
            :around (lambda (orig-fun name &optional ignore-dedicated window)
                      (funcall orig-fun name ignore-dedicated window)
                      (set-window-dedicated-p window nil)))

(defun gud-break-remove ()
  "Set/clear breakpoin."
  (interactive)
  (save-excursion
   (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)
       (gud-remove nil)
       (gud-break nil))))

(defun gud-kill ()
  "Kill gdb process."
  (interactive)
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  (dolist (buffer '(gdba gdb-stack-buffer gdb-breakpoints-buffer
                         gdb-threads-buffer gdb-inferior-io
                         gdb-registers-buffer gdb-memory-buffer
                         gdb-locals-buffer gdb-assembler-buffer))
          (when (gdb-get-buffer buffer)
            (let ((proc (get-buffer-process (gdb-get-buffer buffer))))
              (when proc (set-process-query-on-exit-flag proc nil)))
            (kill-buffer (gdb-get-buffer buffer)))))

;;for cdb.exe debug on windows
;;Use kd -k <connection string> instead of cdb <your program> e.g. M-x cdb kd -k com:port=com1
;;http://www.microsoft.com/whdc/devtools/debugging/default.mspx
;;A word for WinDbg
;;http://mtaulty.com/communityserver/blogs/mike_taultys_blog/archive/2004/08/03/4656.aspx
;;http://www.windbg.org/
;;http://www.windbg.info/
;;command line: "windbg.exe" -y "Debug;for-windbg\Debug" -srcpath "for-windbg" Debug\for-windbg.exe args
;;start folder: D:\work\for-windbg
;;Symbol Server (Microsoft): srv*c:\mss*http://msdl.microsoft.com/download/symbols
(when-ms-windows  
 (load-library "cdb-gud.el")
 (defun my-cdb-mode-hook ()
   (gud-def cdb-bc  "bc * "  nil "Breakpoint clean all."))
 (add-hook 'cdb-mode-hook 'my-cdb-mode-hook))


(provide 'gdb-setting)

;;; gdb-setting.el ends here
