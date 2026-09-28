#!/bin/bash

# Turn off automatic CRLF transformation
git config --global core.autocrlf false

# Force default line endings to LF when creating new files
git config --global core.eol lf

echo "Install Windows git configure done"
