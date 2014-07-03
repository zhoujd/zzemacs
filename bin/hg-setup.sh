#!/bin/sh

echo "hg setup start ..."

###Two books
##http://bucunzai.net/hginit/
##http://hgbook.red-bean.com/read/

###useful urls
##http://mercurial.selenic.com/wiki/TipsAndTricks
##http://mercurial.selenic.com/wiki/UsingExtensions
##http://mercurial.selenic.com/wiki/GitConcepts
##http://mercurial.selenic.com/wiki/Workflows
##http://stackoverflow.com/questions/2428870/hosting-mercurial-hg-via-visualsvn-server

if [ "$OS" = "Windows_NT" ] ; then
    HG_SETUP_HOME=`pwd -W`
else
    HG_SETUP_HOME=`pwd`
fi

echo "remove ~/.hgrc and setting hg configure ..."
cat > ~/.hgrc <<EOF
#
# This is the config file, and
# a '#' or ';' character indicates
# a comment
#

[ui]
username = zhoujd<zjd-405@163.com>
verbose = True
merge = extmerge

[alias]
llog = log --limit 10
mq = hg -R \$(hg root)/.hg/patches \$*
hgrep = hg manifest | grep \$*

[merge-tools]
extmerge.executable = $HG_SETUP_HOME/hg-merge-wrapper.py
extmerge.args = \$base \$local \$other \$output
extmerge.priority = 1
extmerge.premerge = True
extmerge.gui = True
extmerge.binary = True

[extensions]
hgext.extdiff =
hgext.fetch =
bookmarks =
mq =
purge =
pager =
graphlog =
progress =
rebase =
color =

[extdiff]
df = $HG_SETUP_HOME/hg-diff-wrapper.py

[diff-tools]
df.diffargs = \$parent \$child

[bookmarks]
track.current = True

[web] 
push_ssl = false
allow_push = *
cacerts = $HG_SETUP_HOME/hg-cacert.pem

[tortoisehg]
ui.language = en
vdiff = df
EOF

add_cfg_tail()
{
cat >> ~/.hgrc <<EOF

[color]
mode = off
EOF
}

if [ "$OS" = "Windows_NT" ] ; then
    if [ ! $(cd "$HOME" ; pwd -W) = $(cd $USERPROFILE ; pwd -W) ] ; then
        cp -f ~/.hgrc $USERPROFILE
    fi
	# Add tail configure
	add_cfg_tail
fi

echo "hg setup end ..."
