#!/bin/bash

if [ $# != 2 ];then
    cat <<EOF
Usage: $0 {num-of-byte} {file}
Detail: { head -c 10 >head_part; cat >tail_part;} <file
EOF
    exit 0;
fi

NUM=$1
FILE=$2

{ head -c $NUM >head_part; cat >tail_part;} <$FILE

ls -l head_part tail_part

echo "Split $FILE with head $NUM"
