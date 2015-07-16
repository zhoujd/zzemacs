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
    FONT_TARGET=~/.fonts
    mkdir -p $FONT_TARGET
    ln -sf ${ZZEMACS_ROOT}/font/consola/*.ttf $FONT_TARGET
    ln -sf ${ZZEMACS_ROOT}/font/*.ttf         $FONT_TARGET
}

Install_others()
{
    ##create ~/.emacs.d folder
    mkdir -p ~/.emacs.d
}

Install_dot_emacs
Install_fonts_conf
Install_others
