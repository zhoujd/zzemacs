#!/bin/bash

URL_BASE=https://www.howmanysyllables.com/pronounce
AUDIO_EXT=mp3

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
        echo $URL_BASE/$t.$AUDIO_EXT
        wget $URL_BASE/$t.$AUDIO_EXT
    fi
done
