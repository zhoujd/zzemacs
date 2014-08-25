#!/bin/sh

CURRENT_ROOT=`pwd`

echo "create .hgignore template start ..."

cat > $CURRENT_ROOT/.hgignore <<EOF
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
