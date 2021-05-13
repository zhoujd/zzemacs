### self function setting

## useful function like alias
ipaddr() { ifconfig $1 | grep inet | awk '{print $2}' | sed 's/^addr://g'; }
cdl()    { cd "$@";  l; }

## find /var/ftp -type f -print0 | xargs -0 chmod -v 760
chmodf() { find $2 -type f -exec chmod $1 {} \; }
## find /var/ftp -type d -print0 | xargs -0 chmod -v 770
chmodd() { find $2 -type d -exec chmod $1 {} \; }

## e.g. up -> go up 1 directory
## up 4 -> go up 4 directories
up() {
    dir=""
    if [ -z `echo $1 | grep -E '^[0-9]+$'` ]; then
        dir=..
    else
        x=0
        while [ $x -lt ${1:-1} ]; do
            dir=${dir}../
            x=$(($x+1))
        done
    fi
    cd "$dir"
}

empty() {
    pushd ~/.Trash > /dev/null
    tmp=$(rm -rfv * | wc -l | sed -e 's/^[ \t]*//')
    if [ $tmp == "1" ]; then
        echo "$tmp file was removed."
    else
        echo "$tmp files were removed."
    fi
    popd > /dev/null
}

LS() {
    echo "YOU TYPED 'LS' INSTEAD OF 'ls'!"
    echo "Guess I can list it anyways..."
    ls --color=auto $*
}

sl() {
    echo "YOU TYPED 'sl' INSTEAD OF 'ls'!"
    echo "Guess I can list it anyways..."
    ls --color=auto $*
}

fawk() {
    first="awk '{print "
    last="}'"
    cmd="${first}\$${1}${last}"
    eval $cmd
}

noproxy() {
    unset http_proxy
    unset https_proxy
    unset ftp_proxy
    unset no_proxy
    unset socks_host
    unset socks_port
}

## delete or list invalite soft link
#rmerrln() { for f in $(find $1 -type l); do [ -e $f ] && rm -f $f; done }
#lserrln() { find $1 -type l -print | xargs lsattr -d 2>&1 | grep "No such file or directory" | awk '{print $11}';}
