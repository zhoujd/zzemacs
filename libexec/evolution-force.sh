#!/bin/bash

if [ -z $(command -v evolution) ] ; then
    echo "Cannot find evolution"
    exit 1
fi

force_restart() {
    echo "evolution restart"
    evolution --force-shutdown > /dev/null 2>&1
    sleep 2s
    evolution
}

force_restart

echo "evolution force restart done"
