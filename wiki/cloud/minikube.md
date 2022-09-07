minikube
========

## How to Install Minikube on Ubuntu

    ##Step 1: Update System and Install Required Packages
    $ sudo apt update
    $ sudo apt install curl
    $ sudo apt install apt-transport-https

    ##Step 2: Install VirtualBox Hypervisor
    $ sudo apt install virtualbox virtualbox-ext-pack

    ##Step 3: Install Minikube
    $ wget https://storage.googleapis.com/minikube/releases/latest/minikube-linux-amd64
    $ sudo cp minikube-linux-amd64 /usr/local/bin/minikube
    $ sudo chmod +x /usr/local/bin/minikube
    $ minikube version

    ##Step 4: Install Kubectl
    $ curl -LO https://storage.googleapis.com/kubernetes-release/release/`curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt`/bin/linux/amd64/kubectl
    $ chmod +x ./kubectl
    $ sudo mv ./kubectl /usr/local/bin/kubectl
    $ kubectl version -o json

    ##Step 5: Start Minikube
    $ minikube start

    ##Managing Kubernetes with Minikube
    $ kubectl config view
    $ kubectl cluster-info
    $ kubectl get nodes
    $ kubectl get pod

    $ minikube ssh
    $ minikube stop
    $ minikube status
    $ minikube delete
    $ minikube addons list
    $ minikube dashboard
    $ minikube dashboard --url

## How to Install Minikube on CentOS

    ##Step 1: Updating the System
    $ sudo yum -y update

    ##Step 2: Installing KVM Hypervisor
    $ sudo yum -y install epel-release
    $ sudo yum -y install libvirt qemu-kvm virt-install virt-top libguestfs-tools bridge-utils
    $ sudo systemctl start libvirtd
    $ sudo systemctl enable libvirtd
    $ systemctl status libvirtd
    $ sudo usermod -a -G libvirt $(whoami)
    $ sudo vi /etc/libvirt/libvirtd.conf
    unix_sock_group = "libvirt"
    unix_sock_rw_perms = "0770"
    $ sudo systemctl restart libvirtd.service

    ##Step 3,4,5 as same as on Ubuntu

## Minikube on Github
   https://github.com/kubernetes/minikube

## MInikube use virtualbox

    ## VM virtualbox
    $ minikube start --driver=virtualbox
    $ minikube config set driver virtualbox

    ## Better and suggest kvm2
    $ minikube start --driver=kvm2
    $ minikube config set driver kvm2

    ## Docker
    $ minikube start --driver=docker
    $ minikube config set driver docker

## Install multus CNI

    $ git clone https://github.com/k8snetworkplumbingwg/multus-cni && cd multus-cni
    $ cat ./deployments/multus-daemonset-thick-plugin.yml | kubectl apply -f -

## Minikube and Bridged Networking

    ## https://dpb587.me/post/2020/04/11/minikube-and-bridged-networking/
    $ sudo route add -net 10.96.0.0/12 $( get_bridged_ip )
    $ sudo tcptraceroute 10.107.154.9 443
      Tracing the path to 10.107.154.9 on TCP port 443 (https), 30 hops max
       1  kubernetes.default.svc (192.0.2.113)  2.732 ms  1.108 ms  0.983 ms
       2  10.107.154.9  1.222 ms  1.075 ms  0.994 ms
       3  10.107.154.9 [open]  1.026 ms  1.514 ms  1.334 ms
    $ open https://10.107.154.9/#/login

## Install and Set Up kubectl on Linux

    ## Download the latest release
    $ curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
    $ curl -LO https://dl.k8s.io/release/v1.25.0/bin/linux/amd64/kubectl

    ## Validate the binary (optional)
    $ curl -LO "https://dl.k8s.io/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl.sha256"
    $ echo "$(cat kubectl.sha256)  kubectl" | sha256sum --check
    kubectl: OK

    ## Install kubectl
    $ sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl
    $ kubectl version --client
    $ kubectl version --client --output=yaml
