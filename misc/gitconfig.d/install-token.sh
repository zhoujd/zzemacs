#!/bin/bash

GITHUB_TOKEN=${GITHUB_TOKEN:-$1}
URL_FILE=~/.gitconfig-url
OPT="-f $URL_FILE"

install_token() {
    if [ -z "$GITHUB_TOKEN" ]; then
        echo "Usage: $(basename $0) {GITHUB_TOKEN}"
    else
        git config $OPT \
            url."https://${GITHUB_TOKEN}@github.com/zhoujd".insteadOf "https://github.com/zhoujd"
        echo "Install Token Done"
    fi
}

install_token $@
