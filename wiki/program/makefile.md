Makefile
========

## Automatic Variables

    ##https://www3.ntu.edu.sg/home/ehchua/programming/cpp/gcc_make.html
    $@: the target filename.
    $*: the target filename without the file extension.
    $<: the first prerequisite filename.
    $^: the filenames of all the prerequisites, separated by spaces, discard duplicates.
    $+: similar to $^, but includes duplicates.
    $?: the names of all prerequisites that are newer than the target, separated by spaces.

## Current relative directory of Makefile

    ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
