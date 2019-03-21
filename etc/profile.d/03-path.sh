### PATH

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/../.. && pwd)

addpath() {
    for ARG in "$@"
    do
        if [ -d "$ARG" ] && [[ ":$PATH:" != *":$ARG:"* ]]; then
            PATH="$ARG${PATH:+":$PATH"}"
        fi
    done
    export PATH
}

addldpath() {
    for ARG in "$@"
    do
        if [ -d "$ARG" ] && [[ ":$LD_LIBRARY_PATH:" != *":$ARG:"* ]]; then
            LD_LIBRARY_PATH="$ARG${LD_LIBRARY_PATH:+":$LD_LIBRARY_PATH"}"
        fi
    done
    export LD_LIBRARY_PATH
}

addpkgpath() {
    for ARG in "$@"
    do
        if [ -d "$ARG" ] && [[ ":$PKG_CONFIG_PATH:" != *":$ARG:"* ]]; then
            PKG_CONFIG_PATH="$ARG${PKG_CONFIG_PATH:+":$PKG_CONFIG_PATH"}"
        fi
    done
    export PKG_CONFIG_PATH
}

addpath \
    $HOME/local/bin \
    $ZZEMACS_HOME/bin \
    $ZZEMACS_HOME/libexec
