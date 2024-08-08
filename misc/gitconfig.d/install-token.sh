#!/bin/bash

GITHUB_TOKEN=${GITHUB_TOKEN:-$1}
GITURL_FILE=~/.gitconfig-url

if [ -z "$GITHUB_TOKEN" ]; then
    echo "Usage: $(basename $0) {GITHUB_TOKEN}"
else
    git config -f $GITURL_FILE \
        url."https://${GITHUB_TOKEN}@github.com/zhoujd".insteadOf "https://github.com/zhoujd"
    echo "Install Token Done"
fi    
