#!/bin/sh

echo "hg setup start ..."

###Two books
##http://bucunzai.net/hginit/
##http://hgbook.red-bean.com/read/

###useful urls
##http://mercurial.selenic.com/wiki/TipsAndTricks
##http://mercurial.selenic.com/wiki/UsingExtensions
##http://mercurial.selenic.com/wiki/CACertificates
##http://mercurial.selenic.com/wiki/GitConcepts
##http://mercurial.selenic.com/wiki/Workflows
##http://stackoverflow.com/questions/2428870/hosting-mercurial-hg-via-visualsvn-server

if [ "$OS" = "Windows_NT" ] ; then
    HG_SETUP_HOME=$(cd $(dirname "${BASH_SOURCE}") && pwd -W)
else
    HG_SETUP_HOME=$(cd $(dirname "${BASH_SOURCE}") && pwd)
fi

EXTMERGE=hg-merge-wrapper.py

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
merge = ${EXTMERGE}

[alias]
nlog = log --limit
llog = log --limit 10
mq = hg -R \$(hg root)/.hg/patches \$*
hgrep = hg manifest | grep \$*

[merge-tools]
${EXTMERGE}.executable = ${HG_SETUP_HOME}/${EXTMERGE}
${EXTMERGE}.args = \$base \$local \$other \$output
${EXTMERGE}.priority = 1
${EXTMERGE}.premerge = True
${EXTMERGE}.gui = True
${EXTMERGE}.binary = True

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
hgk = 

[hgk]
path = wish ${HG_SETUP_HOME}/hgk.tcl

[extdiff]
df = ${HG_SETUP_HOME}/hg-diff-wrapper.py

[diff-tools]
df.diffargs = \$parent \$child

[bookmarks]
track.current = True

[web] 
push_ssl = False
allow_push = *
cacerts = ${HG_SETUP_HOME}/hg-cacert.pem

[hostfingerprints]
bitbucket.org = 45:ad:ae:1a:cf:0e:73:47:06:07:e0:88:f5:cc:10:e5:fa:1c:f7:99

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
