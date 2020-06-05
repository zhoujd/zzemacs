#!/bin/bash

## https://github.com/aki2o/plsense
## ps -ef | grep perl | grep plsense

echo "For perl develop start ..."

echo "==>1 install plsense"
sudo apt install -y plsense

echo "==>2 install \$HOME/.plsense"
cat > $HOME/.plsense <<EOF
cachedir=$HOME/.plsense.d
clean-env=0
logfile=
loglevel=
maxtasks=20
perl=perl
perldoc=perldoc
port1=33333
port2=33334
port3=33335
EOF

echo "==>Run 'plsense' or 'plsense config' to initalize it"

echo "For perl develop end ..."
