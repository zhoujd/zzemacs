awk
===

## Get column average

    $ awk '{ sum += $1 }; END { print sum / NR }' input.log
    $ awk '{ sum += $1 }; END { print sum / NR }' input.log
