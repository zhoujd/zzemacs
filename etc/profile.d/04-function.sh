### self function setting

## useful function like alias
ipaddr() { ifconfig $1 | grep inet | awk '{print $2}' | sed 's/^addr://g'; }
cdl()    { cd "$@";  l; }
rpmxf()  { [ "$1" != "" ] && (rpm2cpio "$1" | cpio -idmv); }

## init rpmbuild fold
rpminit() {     
    mkdir -p ~/rpmbuild/{BUILD,BUILDROOT,RPMS,SOURCES,SPECS,SRPMS}
    echo '%_topdir %(echo $HOME)/rpmbuild' > ~/.rpmmacros
    echo "~/rpmbuild is exist now"
}

## chomod directory recursivly
chmoddr() {
    if [ -d "$1" ]; then
        echo "error: please use the mode first, then the directory"
        return 1
    elif [ -d "$2" ]; then
        find $2 -type d -print0 | xargs -0 chmod $1
    fi
}

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

empty () {
    pushd ~/.Trash > /dev/null
    tmp=$(rm -rfv * | wc -l | sed -e 's/^[ \t]*//')
    if [ $tmp == "1" ]; then
        echo "$tmp file was removed."
    else
        echo "$tmp files were removed."
    fi
    pushd > /dev/null
}

LS () {
    echo "YOU TYPED 'LS' INSTEAD OF 'ls'!"
    echo "Guess I can list it anyways..."
    ls --color=auto $*
}

sl () {
    echo "YOU TYPED 'sl' INSTEAD OF 'ls'!"
    echo "Guess I can list it anyways..."
    ls --color=auto $*
}

## delete or list invalite soft link
#rmerrln() { for f in $(find $1 -type l); do [ -e $f ] && rm -f $f; done }
#lserrln() { find $1 -type l -print | xargs lsattr -d 2>&1 | grep "No such file or directory" | awk '{print $11}';}
