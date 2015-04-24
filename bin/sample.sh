#!/bin/sh

## sudo yum install redhat-lsb

export OS_DISTRO="unknown"
export SYSARCH=64

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

linux_issue_check()
{
    try_command cat /etc/issue > /dev/null
    if [ ! -z "$(cat /etc/issue | grep 'Ubuntu')" ]; then
        OS_DISTRO="Ubuntu"
        echo "Run on $OS_DISTRO ..."
    elif [ ! -z "$(cat /etc/issue | grep 'CentOS')" ]; then
        OS_DISTRO="CentOS"
        echo "Run on $OS_DISTRO ..."
    elif [ ! -z "$(cat /etc/issue | grep 'SUSE')" ]; then
        OS_DISTRO="SuSE"
        echo "Run on $OS_DISTRO ..."
    else
        echo "Run on $OS_DISTRO ..."
        false
    fi
}

linux_lsb_check()
{
    try_command lsb_release -si > /dev/null
    case $(lsb_release -si) in
        "SUSE LINUX" )
            OS_DISTRO="SuSE"
            echo "Run on $OS_DISTRO ..."
            ;;
        "Ubuntu" )
            OS_DISTRO="Ubuntu"
            echo "Run on $OS_DISTRO ..."
            ;;
        "CentOS" )
            OS_DISTRO="CentOS"
            echo "Run on $OS_DISTRO ..."
            ;;
        * )
            echo "Run on $OS_DISTRO ..."
            false
            ;;
    esac
}

linux_sample ()
{
    ## Dectect OS version
    linux_issue_check || linux_lsb_check

    ## Detect system arch.
    ULONG_MASK=`getconf ULONG_MAX`
    if [ $ULONG_MASK = 18446744073709551615 ]; then
        SYSARCH=64
        echo "Run on 64bit System ..."
    else
        SYSARCH=32
        echo "Run on 32bit System ..."
    fi
}

freebsd_sample()
{
    OS_DISTRO="FreeBSD"
    echo "Run on $OS_DISTRO ..."
}

mingw_sample()
{
    OS_DISTRO="MINGW"
    echo "Run on $OS_DISTRO ..."
}

## Detect OS type
try_command uname -s > /dev/null
case `uname -s` in
    Linux )
        try_command linux_sample
        ;;
    FreeBSD )
        try_command freebsd_sample
        ;;
    MINGW* )
        try_command mingw_sample
        ;;
    * )
        echo "unknown os ..."
        ;;
esac

## reliable-way-for-a-bash-script-to-get-the-full-path-to-itself
#WDIR=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)   ## only for bash
#WDIR=$(cd $(dirname $0) && pwd)
