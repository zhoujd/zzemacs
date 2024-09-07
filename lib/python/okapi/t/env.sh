### env.sh

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

add_path() {
    ARG=$1
    if [ -d "$ARG" ] && [[ ":$PATH:" != *":$ARG:"* ]]; then
        PATH="$ARG${PATH:+":$PATH"}"
    fi

    export PATH
}

add_path $SCRIPT_ROOT
