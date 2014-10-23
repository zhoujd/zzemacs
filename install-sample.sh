#!/bin/sh

ZZEMACS_ROOT=`pwd`

Install_dot_emacs()
{
cat > ~/.emacs <<EOF
;;;this is .emacs for zhoujd.
(defvar zzemacs-path "${ZZEMACS_ROOT}")
(if (file-exists-p (concat zzemacs-path "/.emacs"))
    (load-file (concat zzemacs-path "/.emacs"))
    (message "zzemacs has not install"))
EOF
}

##setup font setting
#http://www.jinbuguo.com/gui/fonts.conf.html
Install_fonts_conf()
{
cat > ~/.fonts.conf <<EOF
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<!-- /etc/fonts/fonts.conf file to configure system font access -->
<fontconfig>
  <!-- Font directory list -->
  <dir>${ZZEMACS_ROOT}/font</dir>
</fontconfig>
EOF
}

Install_dot_emacs
Install_fonts_conf

