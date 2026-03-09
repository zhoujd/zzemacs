#!/bin/bash

build() {
  echo "Build the Extension"
  python3 setup.py build_ext --inplace
}

run() {
  echo "Test the Extension"
  python3 ./test.py
}

clean() {
  rm -rf build *.so
}

case $1 in
  build|-b )
    build
    ;;
  run|-r )
    run
    ;;
  clean|-c )
    clean
    ;;
  * )
    echo "usage: $(basename $0) {build|-b|run|-r|clean|-c}"
    ;;
esac
