#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/.. && pwd)
HELM_ROOT=$(cd $ZZEMACS_ROOT/site-lisp/helm && pwd)
ASYNC_ROOT=$(cd $ZZEMACS_ROOT/site-lisp/emacs-async && pwd)

build() {
    pushd $HELM_ROOT
    export EMACSLOADPATH="$ASYNC_ROOT:"
    make
    popd
}

build

echo "helm build done"
