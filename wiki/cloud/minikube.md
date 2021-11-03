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

## Install multus CNI

    $ git clone https://github.com/k8snetworkplumbingwg/multus-cni && cd multus-cni
    $ cat ./deployments/multus-daemonset-thick-plugin.yml | kubectl apply -f -
