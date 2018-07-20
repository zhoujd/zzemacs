### PATH

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/../.. && pwd)

export PATH=${ZZEMACS_HOME}/bin:${ZZEMACS_HOME}/libexec:$PATH

if [ -d ~/local/bin ]; then
    export PATH=~/local/bin:$PATH
fi
