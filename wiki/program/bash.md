bash
====

## Trap ctrl-c and call ctrl_c()

```
trap ctrl_c INT

ctrl_c()
{
    echo "** Trapped CTRL-C"
}
```

## Regarding ${1:?}

```
## ${1:?}  Errors if $1 is unset or empty.
## ${1?}   Errors only if $1 is unset (empty string is acceptable).

#!/bin/bash
# Exit if $1 is missing or empty
filename=${1:? "Error: No file argument provided"}
echo "Processing $filename"
```
