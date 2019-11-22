#!/bin/bash

### https://www.techvigil.com/tips-tricks/347/mp3-pronunciation-files

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

## https://ssl.gstatic.com/dictionary/static/sounds/oxford/myword--_us_1.mp3
oxford_us() {
    local word=${1,,}
    local url_base=https://ssl.gstatic.com/dictionary/static/sounds/oxford
    local url_sf="--_us_1"

    AUDIO_EXT="mp3"
    WORD_URL="$url_base/${word}${url_sf}.${AUDIO_EXT}"
}

## https://ssl.gstatic.com/dictionary/static/sounds/oxford/myword--_gb_1.mp3
oxford_gb() {
    local word=${1,,}
    local url_base=https://ssl.gstatic.com/dictionary/static/sounds/oxford
    local url_sf="--_gb_1"

    AUDIO_EXT="mp3"
    WORD_URL="$url_base/${word}${url_sf}.${AUDIO_EXT}"
}

## https://www.howmanysyllables.com/pronounce
syllable() {
    local word=$1
    local url_base=https://www.howmanysyllables.com/pronounce
    local url_sf=""

    AUDIO_EXT="mp3"
    WORD_URL="$url_base/${word}${url_sf}.${AUDIO_EXT}"
}

WORDBANK=(`cat $1 | tr -d '\\r' | sed 's/\ //g'`)

for t in "${WORDBANK[@]}"
do
    if [ "${t:0:1}" != "#" ] && [ -n $t  ] ; then
        oxford_us ${t}
        echo "Download $WORD_URL"
        wget -O ${t}.${AUDIO_EXT} $WORD_URL
    fi
done
