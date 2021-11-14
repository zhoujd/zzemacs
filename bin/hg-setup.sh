#!/bin/sh

echo "hg setup start ..."

###Source
##http://selenic.com/hg

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
##http://hgbook.red-bean.com/read/customizing-the-output-of-mercurial.html

###Install from source on linux
##$ sudo yum install python-docutils python-devel
##$ wget http://selenic.com/hg/archive/tip.tar.gz
##$ tar xf tip.tar.gz
##$ mv Mercurial-* Mercurail-latest
##$ cd Mercurail-latest
##$ make all
##$ make install  =>default to /usr/local


if [ "$OS" = "Windows_NT" ] ; then
    HG_SETUP_HOME=$(cd $(dirname $0) && pwd -W)
    ZZ_MISC_ROOT=$(cd $HG_SETUP_HOME/../misc && pwd -W)
    ZZ_LIBEXEC_ROOT=$(cd $HG_SETUP_HOME/../libexec && pwd -W)
else
    HG_SETUP_HOME=$(cd $(dirname $0) && pwd)
    ZZ_MISC_ROOT=$(cd $HG_SETUP_HOME/../misc && pwd)
    ZZ_LIBEXEC_ROOT=$(cd $HG_SETUP_HOME/../libexec && pwd)
fi

EXTMERGE=hg-merge-wrapper.py

echo "remove ~/.hgrc and setting hg configure ..."
cat <<EOF > ~/.hgrc
#
# This is the config file, and
# a '#' or ';' character indicates
# a comment
#

[ui]
username = Zachary Zhou<zachary.zhou@hotmail.com>
verbose = True
merge = ${EXTMERGE}
editor = emacs -Q

[alias]
nlog = log --limit
llog = log --limit 10
cplog = log --style compact
cglog = log --style changelog
flog = log --template 'files: {files}\n'
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
largefiles =

[largefiles]
minsize = 2
patterns = *.jpg *.{png,bmp} *.ttf

[hgk]
path = wish ${ZZ_LIBEXEC_ROOT}/hgk.tcl

[extdiff]
df = ${HG_SETUP_HOME}/hg-diff-wrapper.py

[diff-tools]
df.diffargs = \$parent \$child

[bookmarks]
track.current = True

[http_proxy]
host =
no = 10.0.0.0/8,192.168.0.0/16,localhost,127.0.0.0/8,134.134.0.0/16

[web]
push_ssl = False
allow_push = *
cacerts = ${ZZ_MISC_ROOT}/cert/hg-cacert.pem

[hostfingerprints]
bitbucket.org = 46:de:34:e7:9b:18:cd:7f:ae:fd:8b:e3:bc:f4:1a:5e:38:d7:ac:24

[tortoisehg]
ui.language = en
vdiff = df
EOF

add_cfg_tail() {
cat <<EOF >> ~/.hgrc

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
