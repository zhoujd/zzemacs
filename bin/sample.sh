#!/usr/bin/env bash

# Set Bash color
export ECHO_PREFIX_INFO="\033[1;32;40mINFO...\033[0;0m"
export ECHO_PREFIX_ERROR="\033[1;31;40mError...\033[0;0m"

# Try command  for test command result.
function try_command {
    "$@"
    status=$?
    if [ $status -ne 0 ]; then
        echo -e $ECHO_PREFIX_ERROR "ERROR with \"$@\", Return status $status."
        exit $status
    fi
    return $status
}


