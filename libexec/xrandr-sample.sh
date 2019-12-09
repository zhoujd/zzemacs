#!/usr/bin/env bash

## size and refresh rate of the screen
X=1920
Y=1200
R=60

## interface
INF=DP1

## mode string
MODE="$X"x"$Y"_"$R".00

## turn the output off in case it is on
xrandr --output $INF --off

## delete the mode from the output in case it exists
xrandr --delmode $INF "$MODE"

## delete the mode
xrandr --rmmode "$MODE"

## create the mode, you need to compute the actual mode line using the gtf`or cvt
## program which computes VESA GTF mode lines from size and refresh rates
gtf $X $Y $R | grep Modeline | sed 's/ *Modeline *//' | xargs xrandr --newmode

## add your newly created mode to the output
xrandr --addmode $INF "$MODE"

## turn on the output with the new mode.
xrandr --output $INF --mode $MODE
