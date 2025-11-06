#!/usr/bin/env bash

DIR=$(git rev-parse --show-toplevel 2>/dev/null)

if [ -d "${DIR}/.git/" ]; then
    BRTG=$(git describe --tags --always --all | sed -e 's:heads/::')
    REV=$(git describe --tags --always --dirty --long | sed -e "s:-g\([a-f0-9]\{7\}\):-git-\1:g")
    echo "#define GIT_VERSION ${REV}  /* ${BRTG} */"
else
    echo "#undef GIT_VERSION"
fi
