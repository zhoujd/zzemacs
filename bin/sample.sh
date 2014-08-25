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

Linux_sample ()
{
    ## Dectect OS version
    try_command lsb_release -si > /dev/null
    export OS_DISTRO=`lsb_release -si`
    if [ "$OS_DISTRO" = "SUSE LINUX" ]; then
        OS_DISTRO="SuSE"
        echo "Run on SUSE LINUX ..."
    elif [ "$OS_DISTRO" = "Ubuntu" ]; then
        OS_DISTRO="Ubuntu"
        echo "Run on Ubuntu ..."
    elif [ "$OS_DISTRO" = "CentOS" ]; then
        OS_DISTRO="CentOS"
        echo "Run on CentOS ..."
    else
        echo "Run on $OS_DISTRO ..."
    fi
}

FreeBSD_sample()
{
    export OS_DISTRO="FreeBSD"
    echo "Run on FreeBSD"
}

## Detect OS type
try_command uname -s > /dev/null
export ZZ_OS_NAME=`uname -s`
if [ "Linux" = "$ZZ_OS_NAME" ]; then
    try_command Linux_sample
elif [ "FreeBSD" = "$ZZ_OS_NAME" ]; then
    try_command FreeBSD_sample
else
    echo "Run on $ZZ_OS_NAME ..."
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
