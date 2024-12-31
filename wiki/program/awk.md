awk
===

## Get column average

    $ awk '{ sum += $1 }; END { print sum / NR }' input.log
    $ awk '{ sum += $1 }; END { print sum / NR }' input.log

## Get index value

    $ awk -F"," -vIndex=4 '{print $Index}' filename
    $ awk -F"," -vi=4 '{print $i}' filename
