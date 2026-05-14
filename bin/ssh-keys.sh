#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
CORE_ROOT=$(cd $SCRIPT_ROOT/.. && pwd)
CORE_TOP=$(cd $CORE_ROOT/.. && pwd)

EMAIL=zchrzhou@gmail.com
DST=tmp

renew() {
  mkdir -p $DST
  pushd $DST
  echo "Renew rsa keys for zachary.zhou@yizhu-tech.com"
  ssh-keygen -t rsa -b 4096 -f ./id_rsa -C $EMAIL -N "" -q
  echo "Renew ed25519 keys for runtime@yizhu-tech.com"
  ssh-keygen -o -t ed25519 -a 100 -f ./id_ed25519 -C $EMAIL -N "" -q
  popd
  echo "Check keys in $DST"
  ls -l $DST
  echo "Renew done"
}

renew
