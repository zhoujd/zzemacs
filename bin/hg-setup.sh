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

EOF

echo hg setup end ...
