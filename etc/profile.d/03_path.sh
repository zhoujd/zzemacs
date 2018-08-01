### PATH

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/../.. && pwd)

ADD_PATH=(
    $ZZEMACS_HOME/bin
    $ZZEMACS_HOME/libexec
    $HOME/local/bin
)

add-to-path() {
    case ":$PATH:" in
        *":$1:"*) :;; # already there
        *) PATH="$1:$PATH";; # or PATH="$PATH:$1"
    esac
}

for new_entry in ${ADD_PATH[@]} ; do
    if [ -d $new_entry ]; then
        add-to-path $new_entry
    fi
done
