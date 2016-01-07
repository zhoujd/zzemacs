#!/bin/sh

##Import vars and functions
. sample.sh

echo "hg setup start ..."

echo "remove ~/.hgrc and setting hg configure ..."
cat > ~/.hgrc <<EOF
[ui]
username = zhoujd<zjd-405@163.com>
verbose = True

[extensions]
hgext.extdiff =
bookmarks =

[extdiff]
cmd.bcomp = bcomp
opts.bcomp = /ro

[web] 
push_ssl = false
allow_push = *
EOF

echo "hg setup end ..."
