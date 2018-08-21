#!/bin/sh
set -x

echo "Init go project directories begin."
mkdir -p api build cmd deployments docs internal pkg scripts test third-party tools vendor
touch Makefile README.md

echo "Init go project directories end."
