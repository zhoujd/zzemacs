#!/bin/bash

kube_ac() {
    mkdir -p ~/.bashrc.d
    cat > ~/.bashrc.d/zz-kube.sh <<EOF
alias k=kubectl
source <(kubectl completion bash | sed s/kubectl/k/g)
export KUBE_EDITOR="emacs -nw -Q"
EOF
}

kube_ac

echo "Kube AC setup Done"
