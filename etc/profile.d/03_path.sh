### PATH

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/../.. && pwd)

ADD_PATH=(
    $HOME/local/bin
    $ZZEMACS_HOME/bin
    $ZZEMACS_HOME/libexec
)

add-to-path() {
    for new_entry in ${ADD_PATH[@]} ; do
        if [ -d $new_entry ]; then
            case ":$PATH:" in
                *":$new_entry:"*) :;;
                *) PATH="$new_entry:$PATH";;
            esac
        fi
    done

    export PATH
}

add-to-path
