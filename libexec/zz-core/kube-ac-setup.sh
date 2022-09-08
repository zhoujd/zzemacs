#!/bin/bash

kube_ac() {
    mkdir -p ~/.bashrc.d
    cat > ~/.bashrc.d/zz-kube.sh <<EOF
### zz-kube.sh

## kubectl
if command -v kubectl &>/dev/null; then
    alias k=kubectl
    source <(kubectl completion bash)
    complete -F __start_kubectl k
fi

## minikube
if command -v minikube &>/dev/null; then
    alias m=minikube
    source <(minikube completion bash)
    complete -F __start_minikube m
fi
EOF
}

kube_ac

echo "Kube AC setup Done"
