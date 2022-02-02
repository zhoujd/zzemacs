#!/bin/bash

if [ $# != 1 ]; then
    echo "Usage: $(basename $0) {output}"
    exit 1
fi

output=$1
read -r x y w h G ID < <(slop -f '%x %y %w %h %g %i')
echo "Recording x:$x, y:$y, w:$w, h:$h, output:$output"
exec ffmpeg -y \
	-f x11grab \
	-s ${w}x${h} \
	-framerate 30 \
	-i $DISPLAY+${x},${y} \
	${output}
