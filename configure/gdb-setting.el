;;;; gdb-setting.el --- gdb common file

;;GDB-MI: https://www.emacswiki.org/emacs/GDB-MI

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
  (kill-process (get-buffer-process gud-comint-buffer)))

(add-hook
 'gdb-mode-hook
 '(lambda ()
    (gud-def gud-break-main "break main" nil "Set breakpoint at main.")))

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
;(when-ms-windows  
; (load-library "cdb-gud.el")
; (defun my-cdb-mode-hook ()
;   (gud-def cdb-bc  "bc * "  nil "Breakpoint clean all."))
; (add-hook 'cdb-mode-hook 'my-cdb-mode-hook))


(provide 'gdb-setting)

;;; gdb-setting.el ends here
