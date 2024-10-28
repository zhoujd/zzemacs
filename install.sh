#!/bin/bash

ZZEMACS_ROOT=$(cd $(dirname $0) && pwd)

. $ZZEMACS_ROOT/bin/sample.sh

install_dotemacs() {
    mkdir -p ~/.emacs.d
    cat <<EOF > ~/.emacs
;;; The .emacs for zzemacs
(defvar zzemacs-path "${ZZEMACS_ROOT}")
(if (file-exists-p (concat zzemacs-path "/.emacs"))
    (load-file (concat zzemacs-path "/.emacs"))
    (message "zzemacs has not install"))
EOF
    echo "Install .emacs done"
}

install_fonts() {
    TYPE="${1:-user}"
    echo "Install font to $TYPE"
    case "$TYPE" in
        "system" )
            FONT_TARGET=/usr/share/fonts
            sudo mkdir -p $FONT_TARGET
            sudo ln -sfvT ${ZZEMACS_ROOT}/font $FONT_TARGET/zach
            sudo fc-cache -f
            ;;
        "user" )
            FONT_TARGET=~/.fonts
            mkdir -p $FONT_TARGET
            ln -sfvT ${ZZEMACS_ROOT}/font $FONT_TARGET/zach
            fc-cache -f
            ;;
        * )
            echo "Unknown $TYPE"
            ;;
    esac
    echo "Install fonts done"
}

install_others() {
    ${ZZEMACS_ROOT}/misc/term/install.sh
    ${ZZEMACS_ROOT}/misc/debug/install.sh
    ${ZZEMACS_ROOT}/misc/gitconfig.d/install-cfg.sh
    echo "Install others done"
}

install_thirdparty() {
    ${ZZEMACS_ROOT}/third-party/python/install.sh py3
    ${ZZEMACS_ROOT}/third-party/perl/install.sh
    echo "Install thirdparty done"
}

install_all() {
    confirm_execute "Do you want to overwrite .emacs? [y/N]" \
                    run_cmd install_dotemacs
    confirm_execute "Do you want to install fonts? [y/N]" \
                    run_cmd install_fonts user
    confirm_execute "Do you want to install others? [y/N]" \
                    run_cmd install_others
    confirm_execute "Do you want to install third-party packages? [y/N] " \
                    run_cmd install_thirdparty
    echo "Install all done"
}

usage() {
    local app=$(basename $0)
    echo "$app {dotemacs|-d|fonts|-f|others|-o|thirdparty|-t|all|-a}"
}

case $1 in
    dotemacs|-d )
        confirm_execute "Do you want to overwrite .emacs? [y/N]" \
                        run_cmd install_dotemacs
        ;;
    fonts|-f )
        confirm_execute "Do you want to install fonts? [y/N]" \
                        run_cmd install_fonts user
        ;;
    others|-o )
        confirm_execute "Do you want to install others? [y/N]" \
                        run_cmd install_others
        ;;
    thirdparty|-t )
        confirm_execute "Do you want to install third-party packages? [y/N] " \
                        run_cmd install_thirdparty
        ;;
    all|-a )
        confirm_execute "Do you want to install all configure? [y/N] " \
                        run_cmd install_all
        ;;
    * )
        usage
        ;;
esac
