#!/bin/bash

source /etc/os-release

case $ID in
   ubuntu|debian )
      echo "Run on $ID"
      sudo apt install -y ccls clangd
      ;;
   centos )
      echo "Run on $ID"
      sudo yum install -y epel-release
      sudo yum install -y ccls clangd
      ;;
   * )
      echo "Unsupport OS: $ID"
      ;;
esac

echo "Install tool done"
