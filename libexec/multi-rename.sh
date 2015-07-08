#!/bin/sh

cat <<EOF
rename 's/\.sh/\.php/' *               ## *.sh -> *.php
rename 's/\$/\.bak/' /home/www/*.php    ## add bak to *.php tail
rename 's/^/bak_/' *.bin               ## add bak_ to *.sh head
rename 's/\.bin\$//' *                  ## delete *.bin
rename 's/A-Z/a-z/' *                  ## A-Z -> a-z

## string1 -> string2
for i in \`ls\`; do mv -f \$i \`echo \$i | sed 's/oldstring/newstring/'\`; done 
find -name '*.sh' | xargs perl -pi -e 's|string1|string2|g' 
find  ./ -name '*.sh' | xargs sed -i 's/string1|string2/g'

## *.shtml -> *.php
find -name '*.shtml' | perl -pe 's/(.*)\.shtml/ mv \$1.shtml \$1.php/'
EOF
