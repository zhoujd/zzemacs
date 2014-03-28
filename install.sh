#!/bin/sh

ZZEMACS_ROOT=`pwd`

##Import vars and functions
. $ZZEMACS_ROOT/bin/sample.sh

echo "install .emacs to HOME directory begin..."

Install_package()
{
    # dectect OS version
    if [ "$LINUX_DISTRO" = "SUSE" ]; then
        sudo zypper install -y cscope
        sudo zypper install -y texinfo
    elif [ "$LINUX_DISTRO" = "Ubuntu" ]; then
        sudo apt-get install -y gmrun
        sudo apt-get install -y cscope
        sudo apt-get install -y texinfo
        
        ##install font
        FONT_HOME=/usr/share/fonts/truetype/
        sudo cp -f ${ZZEMACS_ROOT}/font/consola.ttf  ${FONT_HOME}
        sudo cp -f ${ZZEMACS_ROOT}/font/MSYHMONO.ttf ${FONT_HOME}
    else
        echo "You are about to install on a non supported linux distribution."
    fi
}


##install package for emacs
echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        try_command Install_package
        ;;
esac

##setup .emacs
try_command rm -f ~/.emacs
try_command cat > ~/.emacs <<EOF
;;;this is .emacs for zhoujd.
(defvar zzemacs-path "${ZZEMACS_ROOT}/")
(if (file-exists-p (concat zzemacs-path ".emacs"))
    (load-file (concat zzemacs-path ".emacs"))
    (message "zzemacs has not install"))
EOF

##Install thirdparty
Install_thirdparty()
{
    ##git setting
    git config user.name  "zhoujd"
    git config user.email "zjd-405@163.com"

    ##install pymacs
    cd ${ZZEMACS_ROOT}/third-party/python
    ./install.sh
    cd ${ZZEMACS_ROOT}

    ##install pde
    cd ${ZZEMACS_ROOT}/site-lisp/pde
    perl ./Build.PL
    perl ./Build test
    perl ./Build
    sudo perl ./Build install
    cd ${ZZEMACS_ROOT}

    ##install EPL
    cd ${ZZEMACS_ROOT}/third-party/perl/EPL
    perl Makefile.PL
    make
    sudo make install
    cd ${ZZEMACS_ROOT}
}

try_command Install_thirdparty

echo "install .emacs to HOME directory end..."

