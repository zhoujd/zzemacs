### PATH setting

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/../.. && pwd)

pathmunge() {
    case ":${PATH}:" in
        *:"$1":*)
            ;;
        *)
            if [ "$2" = "after" ] ; then
                PATH=$PATH:$1
            else
                PATH=$1:$PATH
            fi
    esac
}

addpath() {
    for ARG in "$@"
    do
        if [ -d "$ARG" ]; then
            pathmunge $ARG
        fi
    done
    export PATH
}

addpath \
    $HOME/.zach/bin \
    $HOME/.local/bin \
    $HOME/.venv/emacs/bin \
    $ZZEMACS_HOME/bin \
    $ZZEMACS_HOME/libexec
