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
