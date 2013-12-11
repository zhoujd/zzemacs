#!/bin/sh

echo hg setup start ...

## setup packages
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
[web] 
push_ssl = false
EOF

echo hg setup end ...
