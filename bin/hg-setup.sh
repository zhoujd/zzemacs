#!/bin/sh

echo "hg setup start ..."

##http://mercurial.selenic.com/wiki/UsingExtensions
##http://mercurial.selenic.com/wiki/GitConcepts
##http://mercurial.selenic.com/wiki/Workflows

if [ "$OS" = "Windows_NT" ] ; then
    HG_SETUP_HOME=`pwd -W`
else
    HG_SETUP_HOME=`pwd`
fi

echo "remove ~/.hgrc and setting hg configure ..."
cat > ~/.hgrc <<EOF
[ui]
username = zhoujd<zjd-405@163.com>
verbose = True
merge = extmerge

[alias]
llog = log --limit 10
mq = hg -R \$(hg root)/.hg/patches $*
hgrep = hg manifest | grep $*

[merge-tools]
extmerge.executable = $HG_SETUP_HOME/hg-merge-wrapper.py
extmerge.args = \$base \$local \$other \$output
extmerge.priority = 1
extmerge.premerge = True
extmerge.gui = True

[extensions]
hgext.extdiff =
hgext.fetch =
bookmarks =
mq =
purge =
color =
pager =
graphlog =
progress =
rebase =

[extdiff]
cmd.extdiff = $HG_SETUP_HOME/hg-diff-wrapper.py

[bookmarks]
track.current = True

[web] 
push_ssl = false
allow_push = *
EOF

if [ "$OS" = "Windows_NT" ] ; then
   cp ~/.hgrc $USERPROFILE
fi

echo "hg setup end ..."
