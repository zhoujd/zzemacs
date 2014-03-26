#!/usr/bin/env sh

# Set Bash color
export ECHO_PREFIX_INFO="\033[1;32;40mINFO...\033[0;0m"
export ECHO_PREFIX_ERROR="\033[1;31;40mError...\033[0;0m"

# Try command  for test command result.
try_command()
{
    "$@"
    status=$?
    if test $status -ne 0; then
        echo -e $ECHO_PREFIX_ERROR "ERROR with \"$@\", Return status $status."
        exit $status
    fi
    return $status
}

## Dectect OS version
try_command lsb_release -si > /dev/null
export LINUX_DISTRO=`lsb_release -si`
if test "$LINUX_DISTRO" == "SUSE LINUX" ; then
    LINUX_DISTRO="SuSE"
    echo -e $ECHO_PREFIX_INFO "Run on SUSE LINUX ..."
elif test "$LINUX_DISTRO" == "Ubuntu" ; then
    LINUX_DISTRO="Ubuntu"
    echo -e $ECHO_PREFIX_INFO "Run on Ubuntu ..."
else
    echo -e $ECHO_PREFIX_ERROR "You are about to install on a non supported linux distribution."
fi

## Detect system arch.
export SYSARCH=64
ULONG_MASK=`getconf ULONG_MAX`
if test $ULONG_MASK == 18446744073709551615; then
    SYSARCH=64
else
    SYSARCH=32
fi

