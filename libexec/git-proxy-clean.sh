#!/bin/bash

git config --global --unset http.proxy
git config --global --unset core.gitproxy

git config --global --list | grep -E http.proxy
git config --global --list | grep -E core.gitproxy

echo "git proxy clean done"
