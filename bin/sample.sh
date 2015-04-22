#!/bin/sh

##sudo yum install redhat-lsb

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

confirm_execute()
{
    echo -n $1
    read answer
    case "$answer" in
        "Y" | "y" )
            $2 $3 $4 $5 $6 $7 $8 $9
            ;;
    esac
}

Linux_sample ()
{
    ## Dectect OS version
    try_command lsb_release -si > /dev/null
    export OS_DISTRO=`lsb_release -si`

    case $OS_DISTRO in
        "SUSE LINUX" )
            OS_DISTRO="SuSE"
            echo "Run on SUSE LINUX ..."
            ;;
        "Ubuntu" )
            OS_DISTRO="Ubuntu"
            echo "Run on Ubuntu ..."
            ;;
        "CentOS" )
            OS_DISTRO="CentOS"
            echo "Run on CentOS ..."
            ;;
        t )
            echo "Run on $OS_DISTRO ..."
            ;;
    esac

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
}

FreeBSD_sample()
{
    export OS_DISTRO="FreeBSD"
    echo "Run on FreeBSD"
}

## Detect OS type
try_command uname -s > /dev/null
case `uname -s` in
    "Linux" )
        try_command Linux_sample
        ;;
    "FreeBSD" )
        try_command FreeBSD_sample
        ;;
    * )
        echo "unknown os ..."
        ;;
esac

## reliable-way-for-a-bash-script-to-get-the-full-path-to-itself
#WDIR=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)   ## only for bash
#WDIR=$(cd $(dirname $0) && pwd)
