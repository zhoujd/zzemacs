#!/bin/bash

RUN_PARAM=(
    -e DISPLAY=$DISPLAY
    -h $HOSTNAME
    -v /tmp/.X11-unix:/tmp/.X11-unix
    -v $HOME/.Xauthority:/home/zach/.Xauthority
    -v $HOME/.emacs:/home/zach/.emacs
    -v $HOME/zzemacs:/home/zach/zzemacs
)

docker run ${RUN_PARAM[@]} ubuntu:zach emacs
