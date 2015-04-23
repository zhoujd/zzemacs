#!/bin/sh

# sudo yum install pcre-devel

TMP_DIR=tmp

echo "Prepare $TMP_DIR folder ..."
rm -rf $TMP_DIR
mkdir $TMP_DIR

pushd $TMP_DIR

echo "Fetch ccze source from github ..."
git clone https://github.com/cornet/ccze.git

if [ -d "ccze" ]; then
	echo "Try to compile ccze ..."
	cd ccze
	./configure --prefix=/usr --with-builtins=all
	make
	sudo make install
fi

popd

echo "Clean $TMP_DIR ..."
rm -rf $TMP_DIR
