扩展工具：
 Snippet: 可定义模板；
 AutoComplete：自动完成工具，可以弹出列表框以供选择；
 Rope and Ropemacs: 重构工具，如 extract method, goto difinition, show documents  等等
 pycomplete：智能提示工具，如，输入time.cl 按TAB，会列出time模块全部以cl开头的函数。调用函数时，可以通过mini buffer提示函数的参数类型。
 
1. Install YASnippet
 
download:
 https://github.com/capitaomorte/yasnippet
 
install:
 cp top-folder-of-yasinppet to your emcas load-path
 
config:
 (add-to-list'load-path "d:/tools/emacs-23.2/config/.emacs.d/capitaomorte-yasnippet-dad9612")
 (require 'yasnippet) 
 (yas/initialize) 
 (yas/load-directory "d:/tools/emacs-23.2/config/.emacs.d/capitaomorte-yasnippet-dad9612/snippets")
 
2. Install AutoComplete:
 
download:
 http://cx4a.org/software/auto-complete/#Latest_Stable
 
install:
 cp top-folder-of-autocomplete to your emcas load-path
 
config:
 (add-to-list'load-path "d:/tools/emacs-23.2/config/.emacs.d/auto-complete-1.3.1")
 (require 'auto-complete) 
 (require 'auto-complete-config) 
 ;(require 'auto-complete-settings) ; for test here.
 (global-auto-complete-mode t) 
 (add-to-list 'ac-dictionary-directories "d:/tools/emacs-23.2/config/.emacs.d/auto-complete-1.3.1/dict")
 ;(ac-config-default) ; for test here
 (setq-default ac-sources '(ac-source-words-in-same-mode-buffers)) 
 (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols))) 
 (add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename))) 
 (set-face-background 'ac-candidate-face "lightgray") 
 (set-face-underline 'ac-candidate-face "darkgray") 
 (set-face-background 'ac-selection-face "steelblue") 
 (define-key ac-completing-map "\M-n" 'ac-next) 
 (define-key ac-completing-map "\M-p" 'ac-previous) 
 (setq ac-auto-start 1) 
 (setq ac-dwim t) 
 (define-key ac-mode-map (kbd "M-TAB") 'auto-complete) 
 
3. Install rope, pymacs and ropemacs
 
download rope:
 http://pypi.python.org/pypi/rope
 
install rope:
 python setup.py install
 
config rope:
 no need
 
download pymacs:
 https://github.com/pinard/Pymacs/downloads
 
install pymacs:
 
for python part:
 python pppp -C ppppconfig.py pppp.rst.in pymacs.el.in pymacs.rst.in Pymacs.py.in contrib tests
 python setup.py install
 
for emacs part:
 cp top-folder-of-pymacs\pymacs.el your-eamcs-load-path(for me, is d:\tools\emacs-23.2\config\.emacs.d\pymacs\)
 
config pymacs:
 (add-to-list'load-path "d:/tools/emacs-23.2/config/.emacs.d/pymacs")
 (require 'pymacs)
 (autoload 'pymacs-apply "pymacs") 
 (autoload 'pymacs-call "pymacs") 
 (autoload 'pymacs-eval "pymacs" nil t) 
 (autoload 'pymacs-exec "pymacs" nil t) 
 (autoload 'pymacs-load "pymacs" nil t) 
 
download ropemacs:
 http://bitbucket.org/agr/ropemacs/get/tip.gz
 
install ropemacs:
 
python  setup.py install
 
install repemode:
 ( must install repemode for ropemacs, which can be download from http://bitbucket.org/agr/ropemode/get/tip.gz)
 python setup.py install
 


config ropemacs:
 (require 'pymacs)
 (pymacs-load "ropemacs" "rope-") 
 (setq ropemacs-enable-autoimport t)
 
To chech pymacs and ropemacs sussfully:
 
luanch eamcs, and within the *Pymacs* buffer, you can info such like this:
 
<23  (version "0.24-beta2")
 >45  eval pymacs_load_helper("ropemacs", "rope-")
 <5801  (eval (progn (defgroup ropemacs nil
   "ropemacs, an emacs plugin for rope."
   :link '(url-link "http://rope.sourceforge.net/ropemacs.html")
   :prefix "rope-")
 ......
 
4. Install pycomplete:
 
download:
 http://www.rwdev.eu/articles/emacspyeng
 
install and config:
 refer to http://www.rwdev.eu/articles/emacspyeng

 (1).if you choose original Pymacs install according to instructions
 (2).if you choose my Pymacs package: copy file pymacs.el on your Emacs load-path (e.g. /usr/share/emacs/site-lisp), 
       and directory Pymacs on PYTHONPATH (e.g. /usr/lib/python2.7/site-packages)
 (3).copy files python-mode.el and pycomplete.el on your Emacs load-path (e.g. /usr/share/emacs/site-lisp).
 (4).copy file pycomplete on your PYTHONPATH (e.g. /usr/lib/python2.7/site-packages)

 
5. config gdb
 
to continue.