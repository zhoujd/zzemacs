#!/bin/sh

echo "hg setup start ..."

###Mercurial Books
##http://mercurial.selenic.com/
##http://hginit.com/ (Hg Init: a Mercurial tutorial)
##http://hgbook.red-bean.com/ (Mercurial: The Definitive Guide)

Install_package()
{
    # dectect OS version
    LINUX_DISTRO=`lsb_release -si`
    if [ "$LINUX_DISTRO" == "SUSE LINUX" ]; then
        echo "Install on suse"
    elif [ "$LINUX_DISTRO" == "Ubuntu" ]; then
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
        Install_package
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
