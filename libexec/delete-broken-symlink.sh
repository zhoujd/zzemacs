#!/bin/bash

## Use `-maxdepth 1' to exclude subdirectories
find -xtype l -delete

echo "Delete broken symlink done"
