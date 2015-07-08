#!/bin/sh

## *.shtml -> *.php
find -name '*.shtml' | perl -pe 's/(.*)\.shtml/ mv $1.shtml $1.php/' | bash

