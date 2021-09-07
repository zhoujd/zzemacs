#!/bin/bash

ZZEMACS_ROOT=`pwd`

## source vars and functions
. $ZZEMACS_ROOT/bin/sample.sh

echo "install .emacs to HOME directory begin..."

## setup .emacs
install_dot_emacs() {
    cat <<EOF > ~/.emacs
;;;This is .emacs for zhoujd.
(defvar zzemacs-path "${ZZEMACS_ROOT}")
(if (file-exists-p (concat zzemacs-path "/.emacs"))
    (load-file (concat zzemacs-path "/.emacs"))
    (message "zzemacs has not install"))
EOF
}

## setup font
install_fonts() {
    TARGET_TYPE="system"   ## system/user
    echo "install font to $TARGET_TYPE"
    case "$TARGET_TYPE" in
        "system" )
            FONT_TARGET=/usr/share/fonts
            if [ -d $FONT_TARGET ]; then
                sudo ln -sfvT ${ZZEMACS_ROOT}/font $FONT_TARGET/zach
                sudo fc-cache -f
            fi
            ;;
        "user" )
            FONT_TARGET=~/.fonts
            if [ -d $FONT_TARGET ]; then
                ln -sfvT ${ZZEMACS_ROOT}/font/* $FONT_TARGET
                fc-cache -f
            fi
            ;;
        * )
            echo "unknown $TARGET_TYPE"
            ;;
    esac
}

## setup others
install_others() {
    ## create ~/.emacs.d
    mkdir -p ~/.emacs.d

    ## link zzemacs/etc/terminfo
    ln -sfvT ${ZZEMACS_ROOT}/etc/terminfo ~/.terminfo

    ## link zzemacs/etc/gdbinit
    ln -sfvT ${ZZEMACS_ROOT}/etc/gdbinit ~/.gdbinit
}

## install thirdparty
install_thirdparty() {
    echo "install third party to $TARGET_TYPE"
    ## install pymacs
    ${ZZEMACS_ROOT}/third-party/python/install.sh py3
    ## install EPL
    ${ZZEMACS_ROOT}/third-party/perl/install.sh
}

main() {
    ## install configure file
    confirm_execute "Do you want to overwrite .emacs ? [y/N]" \
                    run_cmd install_dot_emacs
    ## install fonts
    confirm_execute "Do you want to install fonts ? [y/N]" \
                    run_cmd install_fonts
    ## install others
    confirm_execute "Do you want to install others ? [y/N]" \
                    run_cmd install_others
    ## install third-party
    confirm_execute "Do you want to install third-party packages ? (y/N): " \
                    run_cmd install_thirdparty
}

main

echo "install .emacs to HOME directory end..."
