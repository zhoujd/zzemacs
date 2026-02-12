#!/bin/bash

ZZEMACS_ROOT=$(cd $(dirname $0) && pwd)

. $ZZEMACS_ROOT/bin/sample.sh

dep() {
    case "$OS_DISTRO" in
        "Ubuntu" | "LinuxMint" | "Debian" )
            sudo apt install -y python3-pip
            sudo apt install -y curl
            ;;
        "VoidLinux" )
            sudo xbps-install -Sy python3-pip
            sudo xbps-install -Sy curl
            ;;
        * )
            echo "[dep] $OS_DISTRO is not supported."
            ;;
    esac
    echo "[dep] install done"
}

emacs() {
    mkdir -p ~/.emacs.d
    tee ~/.emacs <<EOF
;;; The .emacs for zzemacs
(defvar zzemacs-path "${ZZEMACS_ROOT}")
(if (file-exists-p (concat zzemacs-path "/.emacs"))
    (load-file (concat zzemacs-path "/.emacs"))
    (message "zzemacs has not install"))
EOF
    echo "[emacs] install .emacs done"
}

font() {
    TYPE="${1:-system}"
    echo "[font] install to $TYPE"
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
            echo "[font] unknown $TYPE"
            ;;
    esac
    echo "[font] install done"
}

other() {
    echo "[other] install term rc files"
    ${ZZEMACS_ROOT}/misc/term/install.sh
    echo "[other] install debug rc files"
    ${ZZEMACS_ROOT}/misc/debug/install.sh all
    echo "[other] install git rc files"
    ${ZZEMACS_ROOT}/misc/gitconfig.d/install.sh
    echo "[other] install done"
}

thirdparty() {
    echo "[thirdparty] install python support libraries"
    ${ZZEMACS_ROOT}/third-party/python/install-deps.sh
    ${ZZEMACS_ROOT}/third-party/python/install-venv.sh all
    ${ZZEMACS_ROOT}/third-party/python/install.sh all
    echo "[thirdparty] install done"
}

all() {
    confirm_execute "Do you want to install dependence? [y/N]" \
                    run_cmd dep
    confirm_execute "Do you want to install emacs? [y/N]" \
                    run_cmd emacs
    confirm_execute "Do you want to install font? [y/N]" \
                    run_cmd font
    confirm_execute "Do you want to install other? [y/N]" \
                    run_cmd other
    confirm_execute "Do you want to install third-party? [y/N] " \
                    run_cmd thirdparty
    echo "[all] install done"
}

usage() {
    local app=$(basename $0)
    cat <<EOF
$app {dep|-d|emacs|-e|font|-f|other|-o|thirdparty|-t|all|-a}
$app -f {system|user}     ## default: system
EOF
}

case $1 in
    dep|-d )
        confirm_execute "Do you want to install dependence? [y/N]" \
                        run_cmd dep
        ;;
    emacs|-e )
        confirm_execute "Do you want to install emacs? [y/N]" \
                        run_cmd emacs
        ;;
    font|-f )
        shift
        confirm_execute "Do you want to install font? [y/N]" \
                        run_cmd font "$@"
        ;;
    other|-o )
        confirm_execute "Do you want to install other? [y/N]" \
                        run_cmd other
        ;;
    thirdparty|-t )
        confirm_execute "Do you want to install third-party? [y/N] " \
                        run_cmd thirdparty
        ;;
    all|-a )
        confirm_execute "Do you want to install all configure? [y/N] " \
                        run_cmd all
        ;;
    * )
        usage
        ;;
esac
