#!/bin/sh

## sudo yum install redhat-lsb-core
## sudo pacman -S lsb-release

export OS_DISTRO="unknown"
export SYSARCH=64

# ensure dirs exist
ensure_dirs () {
    for j in "$@"; do
         test -d "$j" || mkdir -p "$j"
    done;
}

# try command for test command result.
run_cmd() {
    "$@"
    status=$?
    if [ $status -ne 0 ]; then
        echo "ERROR with \"$@\", Return status $status."
        exit $status
    fi
    return $status
}

# Print an error message and exit with given status
# call as: die status "message" ["message" ...]
die() {
   exitstat=$1; shift
   for i in "$@"; do
      print -R "$i"
   done
   exit $exitstat
}

# example: $ time_command sleep 10
time_command() {
    time_start=`date +%s`
    "$@"
    time_end=`date +%s`
    interval=$(($time_end-$time_start))
    echo "Latency: $interval sec"
}

confirm_execute() {
    echo -n $1
    read answer
    case "$answer" in
        "Y" | "y" )
            $2 $3 $4 $5 $6 $7 $8 $9
            ;;
    esac
}

linux_issue_check() {
    run_cmd cat /etc/issue > /dev/null
    if [ ! -z "$(cat /etc/issue | grep 'Ubuntu')" ]; then
        OS_DISTRO="Ubuntu"
    elif [ ! -z "$(cat /etc/issue | grep 'CentOS')" ]; then
        OS_DISTRO="CentOS"
    elif [ ! -z "$(cat /etc/issue | grep 'SUSE')" ]; then
        OS_DISTRO="SUSE"
    else
        false
    fi
}

linux_lsb_check() {
    run_cmd lsb_release -si > /dev/null
    case $(lsb_release -si) in
        "SUSE LINUX" )
            OS_DISTRO="SUSE"
            ;;
        "Ubuntu" )
            OS_DISTRO="Ubuntu"
            ;;
        "CentOS" )
            OS_DISTRO="CentOS"
            ;;
        "Fedora" )
            OS_DISTRO="Fedora"
            ;;
        "Arch" )
            OS_DISTRO="Arch"
            ;;
        "ManjaroLinux" )
            OS_DISTRO="Manjaro"
            ;;
        * )
            false
            ;;
    esac
}

linux_sample () {
    ## Dectect OS version
    linux_issue_check || linux_lsb_check
    echo "Run on $OS_DISTRO ..."

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

freebsd_sample() {
    OS_DISTRO="FreeBSD"
    echo "Run on $OS_DISTRO ..."
}

mingw_sample() {
    OS_DISTRO="MINGW"
    echo "Run on $OS_DISTRO ..."
}

## Detect OS type
run_cmd uname -s > /dev/null
case `uname -s` in
    Linux )
        run_cmd linux_sample
        ;;
    FreeBSD )
        run_cmd freebsd_sample
        ;;
    MINGW* )
        run_cmd mingw_sample
        ;;
    * )
        echo "unknown os ..."
        ;;
esac

## Get script path
#script_path=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd) ## only for bash
#script_path=$(cd $(dirname $0) && pwd)

## Note: using bash array for allow the comments for the arguments
#cmd=(
#	./emacs  # run emacs
#   -Q
#   --quick
#)	
#
#"${cmd[@]}"

