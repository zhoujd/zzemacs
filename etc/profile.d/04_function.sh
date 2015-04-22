### self function setting

## useful function like alias
ipaddr()  { ifconfig $1 | grep inet | awk '{print $2}' | sed 's/^addr://g'; }
cdl()     { cd "$@";  l; }
xrpm()    { [ "$1" != "" ] && (rpm2cpio "$1" | cpio -idmv); }

## chomod directory recursivly
chmoddr() 
{
    if [ -d "$1" ]; then 
        echo "error: please use the mode first, then the directory"
        return 1
    elif [ -d "$2" ]; then
        find $2 -type d -print0 | xargs -0 chmod $1
    fi
}

## e.g. up -> go up 1 directory
## up 4 -> go up 4 directories
up()
{
    dir=""
    if [ ! -z "$(echo $1 | grep -E '^[0-9]+$')" ]; then
        x=0
        while [ $x -lt ${1:-1} ]; do
            dir=${dir}../
            x=$(($x+1))
        done
    else
        dir=..
    fi
    cd "$dir";
}
