#!/usr/bin/env sh

HG_SETUP_ROOT=`pwd`
source $HG_SETUP_ROOT/sample.sh

echo "hg setup start ..."

###Mercurial Books
##http://mercurial.selenic.com/
##http://hginit.com/ (Hg Init: a Mercurial tutorial)
##http://hgbook.red-bean.com/ (Mercurial: The Definitive Guide)

Install_package()
{
    # dectect OS version
    if test "$LINUX_DISTRO" == "SuSE" ; then
        echo "Install on suse"
    elif test "$LINUX_DISTRO" == "Ubuntu" ; then
        sudo apt-get install -y mercurial
    else
        echo "You are about to install on a non supported linux distribution."
    fi
}

##setup packages
echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        try_command Install_package
        ;;
esac

echo "remove ~/.hgrc and setting hg configure ..."
cat > ~/.hgrc <<EOF
[ui]
username = zhoujd<zjd-405@163.com>
verbose = True

[extensions]
hgext.extdiff =

[extdiff]
cmd.bcomp = bcomp
opts.bcomp = /ro

[web] 
push_ssl = false
allow_push = *
EOF

echo "hg setup end ..."
