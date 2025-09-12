#!/usr/bin/env bash

GIT=$(command -v git)
SED=$(command -v sed)
DIR=$(${GIT} rev-parse --show-toplevel 2>/dev/null)

if [ -d "${DIR}/.git/" ] && [ -n "${GIT}" ]; then
    BRTG="$(${GIT} describe --tags --always --all | ${SED} -e 's:heads/::')"
    REV="$(${GIT} describe --tags --always --dirty| ${SED} -e 's:-g\([a-f0-9]\{7\}\):-git-\1:g')"
    echo "#define GIT_VERSION ${REV} (${BRTG})"
else
    echo "#undef GIT_VERSION"
fi
