#!/bin/bash

URL_BASE=https://www.howmanysyllables.com/words
WEB_BROWER=chrome

if [ $# != 1 ] ; then
    echo "Usage: `basename $0` [logfile]"
    cat <<EOF
## Example file
$ cat logfile
Welcome
to
thegeekstuff
Linux
Unix

EOF
    exit 1
fi

WordBank=(`cat $1 | tr -d '\\r' | sed 's/\ //g'`)

for t in "${WordBank[@]}"
do
    if [ "${t:0:1}" != "#" ] && [ -n $t  ] ; then
        echo $URL_BASE/$t
        $WEB_BROWER $URL_BASE/$t
    fi
done
