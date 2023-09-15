#!/bin/bash

GITHUB_TOKEN=${GITHUB_TOKEN:-$1}
if [ -z "$GITHUB_TOKEN" ]; then
    echo "Usage: $(basename $0) {GITHUB_TOKEN}"
else
    git config --global url."https://${GITHUB_TOKEN}@github.com/zhoujd".insteadOf "https://github.com/zhoujd"
    echo "Install Token Done"
fi    
