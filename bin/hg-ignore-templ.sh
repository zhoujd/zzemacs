#!/bin/sh

echo "create .hgignore template start ..."

HG_ROOT=`hg root`

# hg repo folder check
if [ ! -s "$HG_ROOT" ] ; then
    echo "`basename $0` should be run under hg repo"
    exit 1
fi

cat > $HG_ROOT/.hgignore <<EOF
## filter with glob
syntax: glob
.git/
*.elc
*.pyc
*~
*.o

## filter with regexp
syntax: regexp

EOF

echo "create .hgignore template end ..."
