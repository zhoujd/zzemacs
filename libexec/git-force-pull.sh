#!/bin/bash

echo "Git force pull to overwrite local files"
git fetch --all
git reset --hard origin/master
git pull origin master
