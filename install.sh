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
    echo "[dotemacs] Install .emacs done"
}

install_font() {
    TYPE="${1:-user}"
    echo "[font] Install to $TYPE"
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
            echo "[font] Unknown $TYPE"
            ;;
    esac
    echo "[font] Install done"
}

install_other() {
    echo "[other] Install term rc files"
    ${ZZEMACS_ROOT}/misc/term/install.sh
    echo "[other] Install debug rc files"
    ${ZZEMACS_ROOT}/misc/debug/install.sh gdb
    ${ZZEMACS_ROOT}/misc/debug/install.sh cgdb
    echo "[other] Install git rc files"
    ${ZZEMACS_ROOT}/misc/gitconfig.d/install-cfg.sh
    echo "[other] Install done"
}

install_thirdparty() {
    echo "[thirdparty] Install python files"
    ${ZZEMACS_ROOT}/third-party/python/install.sh py3
    echo "[thirdparty] Install done"
}

install_all() {
    confirm_execute "Do you want to overwrite .emacs? [y/N]" \
                    run_cmd install_dotemacs
    confirm_execute "Do you want to install fonts? [y/N]" \
                    run_cmd install_font user
    confirm_execute "Do you want to install others? [y/N]" \
                    run_cmd install_other
    confirm_execute "Do you want to install third-party packages? [y/N] " \
                    run_cmd install_thirdparty
    echo "Install all done"
}

usage() {
    local app=$(basename $0)
    echo "$app {dotemacs|-d|font|-f|other|-o|thirdparty|-t|all|-a}"
}

case $1 in
    dotemacs|-d )
        confirm_execute "Do you want to overwrite .emacs? [y/N]" \
                        run_cmd install_dotemacs
        ;;
    font|-f )
        confirm_execute "Do you want to install font? [y/N]" \
                        run_cmd install_font user
        ;;
    other|-o )
        confirm_execute "Do you want to install other? [y/N]" \
                        run_cmd install_other
        ;;
    thirdparty|-t )
        confirm_execute "Do you want to install third-party? [y/N] " \
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
