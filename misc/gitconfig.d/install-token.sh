#!/bin/bash

if [ $# != 1 ]; then
    echo "Usage: $(basename $0) {GITHUB_TOKEN}"
    exit 1
fi


GITHUB_TOKEN=$1
git config --global url."https://${GITHUB_TOKEN}@github.com/zhoujd".insteadOf "https://github.com/zhoujd"

echo "Install Token Done"
