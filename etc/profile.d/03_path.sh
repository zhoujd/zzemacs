### PATH

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/../.. && pwd)

ADD_PATH=(
    $ZZEMACS_HOME/bin
    $ZZEMACS_HOME/libexec
    ~/local/bin
)

for path in ${ADD_PATH[@]} ; do
    if [ -d $path ]; then
        PATH=$path:$PATH
    fi
done
