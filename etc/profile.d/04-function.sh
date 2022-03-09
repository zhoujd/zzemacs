### self function setting

## useful function like alias
ipaddr() { ifconfig $1 | grep inet | awk '{print $2}' | sed 's/^addr://g'; }
cdl()    { cd "$@";  l; }

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

urlencode() {
    # urlencode <string>
    local length="${#1}"
    for (( i = 0; i < length; i++ )); do
        local c="${1:i:1}"
        case $c in
            [a-zA-Z0-9.~_-]) printf "$c" ;;
            *) printf '%%%02X' "'$c"
        esac
    done
}

urldecode() {
    # urldecode <string>
    local url_encoded="${1//+/ }"
    printf '%b' "${url_encoded//%/\\x}"
}

urlenc() {
    # urlenc <string>
    local length="${#1}"
    for (( i = 0; i < length; i++ )); do
        local c="${1:i:1}"
        printf '%%%02X' "'$c"
    done
}

urldec() {
    # urldec <string>
    local url_encoded="${1//+/ }"
    printf '%b' "${url_encoded//%/\\x}"
}

duplfiles() {
    find . ! -empty -type f -exec md5sum {} + | sort | uniq -w32 -dD
}

testmicrophone() {
    arecord -vvv -d 3 -f dat /dev/null
}

## chmod on files and directory
#find -type f -print0 | xargs -0 chmod -v 760
#find -type f -exec chmod 760 {} \;
#find -type d -print0 | xargs -0 chmod -v 770
#find -type d -exec chmod 770 {} \;

## delete or list invalite soft link
#find -L /path -type l -exec rm -i {} \;
#find -xtype l -delete
