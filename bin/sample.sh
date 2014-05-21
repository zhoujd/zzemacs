#!/bin/sh

# Try command  for test command result.
try_command()
{
    "$@"
    status=$?
    if [ $status -ne 0 ]; then
        echo "ERROR with \"$@\", Return status $status."
        exit $status
    fi
    return $status
}

## Dectect OS version
try_command lsb_release -si > /dev/null
export LINUX_DISTRO=`lsb_release -si`
if [ "$LINUX_DISTRO" = "SUSE LINUX" ]; then
    LINUX_DISTRO="SuSE"
    echo "Run on SUSE LINUX ..."
elif [ "$LINUX_DISTRO" = "Ubuntu" ]; then
    LINUX_DISTRO="Ubuntu"
    echo "Run on Ubuntu ..."
elif [ "$LINUX_DISTRO" = "CentOS" ]; then
    LINUX_DISTRO="CentOS"
    echo "Run on CentOS ..."
else
    echo "You are about to install on a non supported linux distribution."
fi

## Detect system arch.
export SYSARCH=64
ULONG_MASK=`getconf ULONG_MAX`
if [ $ULONG_MASK = 18446744073709551615 ]; then
    SYSARCH=64
    echo "Run on 64bit System ..."
else
    SYSARCH=32
    echo "Run on 32bit System ..."
fi

## reliable-way-for-a-bash-script-to-get-the-full-path-to-itself
#DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
