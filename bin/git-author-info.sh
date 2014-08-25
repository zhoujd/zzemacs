#!/bin/sh

###https://help.github.com/articles/changing-author-info
###If you need to modify the author info in your repository's history.
###1. you can do so with this script.
###$<$PWD>/git-author-info.sh match_mail_address
###2. Force-push the new history up (be careful).
###$git push --force
###$git push -f

new_name="zhoujd"
new_email="zjd-405@163.com"

match_email="$1"

git filter-branch --env-filter " 

an=\"\$GIT_AUTHOR_NAME\"
am=\"\$GIT_AUTHOR_EMAIL\"
cn=\"\$GIT_COMMITTER_NAME\"
cm=\"\$GIT_COMMITTER_EMAIL\"
 
if [ \"\$GIT_COMMITTER_EMAIL\" = \"$match_email\" ]; then
    an=\"$new_name\"
    am=\"$new_email\"
fi
if [ \"\$GIT_AUTHOR_EMAIL\" = \"$match_email\" ]; then
    an=\"$new_name\"
    am=\"$new_email\"
fi
 
export GIT_AUTHOR_NAME=\"\$an\"
export GIT_AUTHOR_EMAIL=\"\$am\"
export GIT_COMMITTER_NAME=\"\$cn\"
export GIT_COMMITTER_EMAIL=\"\$cm\"

"
