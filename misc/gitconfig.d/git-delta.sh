#!/bin/bash

## https://github.com/dandavison/delta
## https://dandavison.github.io/delta

git config --global core.pager delta
git config --global interactive.diffFilter 'delta --color-only'
git config --global delta.navigate true
git config --global merge.conflictStyle zdiff3

echo "Config delta done"
