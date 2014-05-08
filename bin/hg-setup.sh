#!/bin/sh

echo "hg setup start ..."

##http://mercurial.selenic.com/wiki/GitConcepts
##http://mercurial.selenic.com/wiki/Workflows

echo "remove ~/.hgrc and setting hg configure ..."
cat > ~/.hgrc <<EOF
[ui]
username = zhoujd<zjd-405@163.com>
verbose = True
merge = p4

[merge-tools]
p4.executable = p4merge
p4.args = $base $local $other $output
p4.priority = 1
p4.premerge = True
p4.gui = True

bcomp.executable = bcomp
bcomp.args = $local $other $base $output
bcomp.priority = 1
bcomp.premerge = True
bcomp.gui = True

[extensions]
hgext.extdiff =
bookmarks =

[extdiff]
cmd.bcomp = bcomp
cmd.p4diff = p4merge

[web] 
push_ssl = false
allow_push = *
EOF

echo "hg setup end ..."
