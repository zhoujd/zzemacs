### PATH

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/../.. && pwd)

add-to-path() {
    for ARG in "$@"
    do
        if [ -d "$ARG" ] && [[ ":$PATH:" != *":$ARG:"* ]]; then
            PATH="$ARG${PATH:+":$PATH"}"
        fi
    done
}

add-to-path \
    $HOME/local/bin       \
    $ZZEMACS_HOME/bin     \
    $ZZEMACS_HOME/libexec
