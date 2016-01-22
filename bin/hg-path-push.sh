#!/bin/sh

echo "set hg default-push $1"

if [ ! "$#" = "1" ]; then
    echo "Using example: $0 <push-url>"
    exit 0
fi

if [ -z "$(hg root)" ]; then
    exit 0
fi

cat >> $(hg root)/.hg/hgrc <<EOF
[paths]
default = $1
EOF
