#!/bin/sh

echo hg setup start ...

###Mercurial Books
##http://mercurial.selenic.com/
##http://hginit.com/ (Hg Init: a Mercurial tutorial)
##http://hgbook.red-bean.com/ (Mercurial: The Definitive Guide)

##setup packages
echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        sudo apt-get install -y mercurial
esac

echo "remove ~/.hgrc and setting hg configure ..."
cat > ~/.hgrc <<EOF
[ui]
username = zhoujd<zjd-405@163.com>
verbose = True

[extensions]
extdiff =

[extdiff]
cmd.bcomp = bcomp
opts.bcomp = /ro

[web] 
push_ssl = false
allow_push = *
EOF

echo hg setup end ...
