#!/bin/sh

ZZEMACS_ROOT=`pwd`

##Import vars and functions
. $ZZEMACS_ROOT/bin/sample.sh

echo "install .emacs to HOME directory begin..."

Install_package()
{
    # dectect OS version
    if [ "$OS_DISTRO" = "SUSE" ]; then
        sudo zypper install -y cscope
        sudo zypper install -y texinfo
    elif [ "$OS_DISTRO" = "Ubuntu" ]; then
        sudo apt-get install -y gmrun
        sudo apt-get install -y cscope
        sudo apt-get install -y texinfo
        sudo apt-get install -y markdown
        sudo apt-get install -y w3m
    elif [ "$OS_DISTRO" = "CentOS" ]; then
        sudo yum install -y gmrun
        sudo yum install -y cscope
        sudo yum install -y texinfo
    elif [ "$OS_DISTRO" = "FreeBSD" ]; then
        sudo pkg_add -r w3m
        sudo pkg_add -r cscope
        sudo pkg_add -r gmrun
    else
        echo "You are about to install on a non supported linux distribution."
    fi
}

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

Install_emacs_run()
{
LANCHER=~/emacs1

cat > ${LANCHER} <<EOF
#!/bin/sh

emacs --no-site-file -q \\
      --eval "(setq zzemacs-path \"${ZZEMACS_ROOT}\")" \\
      --eval "(load-file \"${ZZEMACS_ROOT}/.emacs\")" \\
      --eval "(message \"start emacs finished.\")"
EOF
chmod +x ${LANCHER}
}

Install_other()
{
    ##create ~/.emacs.d folder
    mkdir -p ~/.emacs.d
}

##Install thirdparty
Install_thirdparty()
{
    ##git setting
    cd ${ZZEMACS_ROOT}/bin
    ./git-setup.sh
    cd ${ZZEMACS_ROOT}

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

main()
{
    ##install package for emacs
    confirm_execute "Do you wanna install packages? (y/N): " try_command Install_package

    ##install configure file
    echo "install configure file and fonts"
    if [ -f ~/.emacs ] ; then
        confirm_execute "Do you wanna overwrite .emacs? [y/N]" try_command Install_dot_emacs
    else
        try_command Install_dot_emacs
    fi

    if [ -f ~/.fonts.conf ] ; then
        confirm_execute "Do you wanna overwrite .fonts.conf? [y/N]" try_command Install_fonts_conf
    else
        try_command Install_fonts_conf
    fi

    try_command Install_emacs_run
    try_command Install_other

    ##install third-party
    confirm_execute "Do you wanna install third-party packages? (y/N): " try_command Install_thirdparty

}

main

echo "install .emacs to HOME directory end..."
