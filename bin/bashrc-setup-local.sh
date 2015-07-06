### For local source to bash
## $ cd <current directory>
## $ source <This file>

echo "Setup local base start ..."

# get script path
SETUP_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SETUP_ROOT/.. && pwd)

export PATH=${ZZEMACS_ROOT}/bin:${ZZEMACS_ROOT}/libexec:$PATH
for i in ${ZZEMACS_ROOT}/etc/profile.d/*.sh ; do
    if [ -r "$i" ]; then
        if [ "${-#*i}" != "$-" ]; then
            . "$i"
        else
            . "$i" >/dev/null 2>&1
        fi
    fi
done

echo "Setup local base end ..."
