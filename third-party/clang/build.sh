#!/bin/bash

echo "install clang-complete start ..."
sudo apt install -y llvm-dev libclang-dev

make
sudo mv clang-complete /usr/bin/

echo "install clang-complete end ..."
