#!/bin/bash

##Install from source on linux
sudo yum install -y  python-docutils python-devel

##Init install temp directory
INS_TEMP_DIR=$HOME/Downloads/hg-src-install
rm -rf $INS_TEMP_DIR
mkdir -p $INS_TEMP_DIR

pushd $INS_TEMP_DIR

wget http://selenic.com/hg/archive/tip.tar.gz
tar xf tip.tar.gz
mv Mercurial-* Mercurail-latest
cd Mercurail-latest
make all
sudo make install #default to /usr/local

popd

echo "Install hg from source finished"
