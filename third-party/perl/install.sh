#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/../.. && pwd)

. $ZZEMACS_HOME/bin/sample.sh

## https://github.com/aki2o/plsense
## ps -ef | grep perl | grep plsense

echo "For perl develop start ..."

install_deps() {
    echo "==>1 install plsense"
    case "$OS_DISTRO" in
        "Ubuntu" | "LinuxMint" )
            sudo apt install -y plsense
            ;;
        "CentOS" )
            sudo yum install -y perl-Module-Install
            ;;
        * )
            echo "You are about to install on a non supported linux distribution."
            ;;
    esac
}

install_cfg() {
    echo "==>2 install \$HOME/.plsense"
    cat > $HOME/.plsense <<EOF
cachedir=$HOME/.plsense.d
clean-env=0
logfile=
loglevel=
maxtasks=20
perl=perl
perldoc=perldoc
port1=33333
port2=33334
port3=33335
EOF
}

install_deps
install_cfg

echo "==>Run 'plsense' or 'plsense config' to initalize it"

echo "For perl develop end ..."
