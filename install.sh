#!/bin/sh

ZZEMACS_ROOT=`pwd`

##Import vars and functions
. $ZZEMACS_ROOT/bin/sample.sh

echo "install .emacs to HOME directory begin..."

##setup .emacs
Install_dot_emacs()
{
cat > ~/.emacs <<EOF
;;;This is .emacs for zhoujd.
(defvar zzemacs-path "${ZZEMACS_ROOT}")
(if (file-exists-p (concat zzemacs-path "/.emacs"))
    (load-file (concat zzemacs-path "/.emacs"))
    (message "zzemacs has not install"))
EOF
}

##setup font setting
#https://github.com/android/platform_frameworks_base/tree/master/data/fonts
Install_fonts_conf()
{
    FONT_TARGET=~/.fonts
    mkdir -p $FONT_TARGET

    ln -sf ${ZZEMACS_ROOT}/font/consola/*.ttf       $FONT_TARGET
    ln -sf ${ZZEMACS_ROOT}/font/AnonymousPro/*.ttf  $FONT_TARGET
    ln -sf ${ZZEMACS_ROOT}/font/*.ttf               $FONT_TARGET
}

##setup others
Install_others()
{
    ##create ~/.emacs.d folder
    mkdir -p ~/.emacs.d

    ##link zzemacs/etc/profile
    ln -sf ${ZZEMACS_ROOT}/etc/profile ~/.zzemacs_bash
}

##install thirdparty
Install_thirdparty()
{
    ##install pymacs
    cd ${ZZEMACS_ROOT}/third-party/python
    sh ./install.sh
    cd ${ZZEMACS_ROOT}

    ##install EPL
    cd ${ZZEMACS_ROOT}/third-party/perl
    sh ./install.sh
    cd ${ZZEMACS_ROOT}

    ##install connect
    cd ${ZZEMACS_ROOT}/third-party/proxy
    sh ./install.sh
    cd ${ZZEMACS_ROOT}
}

main()
{
    ##install configure file
    if [ -f ~/.emacs ] ; then
        confirm_execute "Do you wanna overwrite .emacs? [y/N]" try_command Install_dot_emacs
    else
        echo "install configure file"
        try_command Install_dot_emacs
    fi

    echo "install fonts"
    try_command Install_fonts_conf

    echo "install others"
    try_command Install_others

    ##install third-party
    confirm_execute "Do you wanna install third-party packages? (y/N): " try_command Install_thirdparty
}

main

echo "install .emacs to HOME directory end..."
