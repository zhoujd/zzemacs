#!/bin/sh

ZZEMACS_ROOT=`pwd`

##source vars and functions
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

##setup font
Install_fonts()
{
    TARGET_TYPE="system"
    
    echo "install font to $TARGET_TYPE"
    case "$TARGET_TYPE" in
        "system" )
            FONT_TARGET=/usr/share/fonts/zzemacs
            sudo mkdir -p $FONT_TARGET
            
            sudo ln -sf ${ZZEMACS_ROOT}/font/consola/*.ttf       $FONT_TARGET
            sudo ln -sf ${ZZEMACS_ROOT}/font/AnonymousPro/*.ttf  $FONT_TARGET
            sudo ln -sf ${ZZEMACS_ROOT}/font/*.ttf               $FONT_TARGET

            sudo fc-cache
            ;;
        "user")
            FONT_TARGET=~/.fonts
            mkdir -p $FONT_TARGET
            
            ln -sf ${ZZEMACS_ROOT}/font/consola/*.ttf       $FONT_TARGET
            ln -sf ${ZZEMACS_ROOT}/font/AnonymousPro/*.ttf  $FONT_TARGET
            ln -sf ${ZZEMACS_ROOT}/font/*.ttf               $FONT_TARGET

            fc-cache
            ;;
        * )
            echo "unknown $TARGET_TYPE"
            ;;
    esac
}

##setup others
Install_others()
{
    ##create ~/.emacs.d folder
    mkdir -p ~/.emacs.d

    ##setup zzemacs & zzvim & zztmux
    BIN_TARGET=/usr/bin
    sudo ln -sf ${ZZEMACS_ROOT}/bin/zzemacs $BIN_TARGET
    sudo ln -sf ${ZZEMACS_ROOT}/bin/zzvim   $BIN_TARGET
    sudo ln -sf ${ZZEMACS_ROOT}/bin/zztmux  $BIN_TARGET

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
    confirm_execute "Do you want to overwrite .emacs ? [y/N]" run_cmd Install_dot_emacs

    ##install fonts
    confirm_execute "Do you want to install fonts ? [y/N]" run_cmd Install_fonts

    ##install others
    confirm_execute "Do you want to install others ? [y/N]" run_cmd Install_others

    ##install third-party
    confirm_execute "Do you want to install third-party packages ? (y/N): " run_cmd Install_thirdparty
}

main

echo "install .emacs to HOME directory end..."
