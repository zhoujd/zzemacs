#!/bin/bash

## https://plantuml.com/emacs

Install_plantuml() {
    echo "Install plantuml"
    sudo apt install plantuml
}

Install_jar() {
    echo "Install jar"
    local target=~/.plantuml
    mkdir -p $target
    wget -O $target/plantuml.jar http://sourceforge.net/projects/plantuml/files/plantuml.jar/download
}

Install_plantuml
Install_jar

echo "Install plantuml setup done"
