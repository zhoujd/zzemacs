#!/bin/bash

#set -ex

# arguments:
# token = $1
# organization = $2
# repo name = $3
# branch = $4

if [ $# != 4 ]; then

    echo "usage: $0 token my-organization site.com master"
    exit 1
fi

wget --header="Authorization: token ${1}" \
     --header="Accept:application/vnd.github.v3.raw" \
     -O - https://api.github.com/repos/${2}/${3}/tarball/${4} | tar xz
