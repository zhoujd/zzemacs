### PATH

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/../.. && pwd)

ADD_PATH=(
    $HOME/local/bin
    $ZZEMACS_HOME/bin
    $ZZEMACS_HOME/libexec
)

add-to-path() {
    if [ -d $1 ]; then
        case ":$PATH:" in
            *":$1:"*) :;;
            *) PATH="$1:$PATH";;
        esac
    fi
}

add-to-path-group() {
    for new_entry in ${ADD_PATH[@]} ; do
        add-to-path $new_entry
    done
    
    export PATH
}

add-to-path-group
