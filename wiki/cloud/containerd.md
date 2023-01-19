containerd
==========

## Import images

    # ctr -n=k8s.io image import <image>.tar   (RUNC=Containerd)

## Install Containerd on Using Official Binaries

    ## https://www.itzgeek.com/how-tos/linux/ubuntu-how-tos/install-containerd-on-ubuntu-22-04.html
    ## Install Containerd
    $ wget https://github.com/containerd/containerd/releases/download/v1.6.2/containerd-1.6.2-linux-amd64.tar.gz
    $ sudo tar Czxvf /usr/local containerd-1.6.2-linux-amd64.tar.gz

    $ wget https://raw.githubusercontent.com/containerd/containerd/main/containerd.service
    $ sudo mv containerd.service /usr/lib/systemd/system/

    $ sudo systemctl daemon-reload
    $ sudo systemctl enable --now containerd

    $ sudo systemctl status containerd

    ## Install runC
    $ wget https://github.com/opencontainers/runc/releases/download/v1.1.1/runc.amd64
    $ sudo install -m 755 runc.amd64 /usr/local/sbin/runc

    ## Containerd configuration for Kubernetes
    $ sudo mkdir -p /etc/containerd/
    containerd config default | sudo tee /etc/containerd/config.toml

    ## Configure the systemd cgroup driver for runC
    sudo sed -i 's/SystemdCgroup \= false/SystemdCgroup \= true/g' /etc/containerd/config.toml

    ## Restart the containerd service
    $ sudo systemctl restart containerd

## Install Containerd Using Docker Repository

    ## Setup Docker repository
    $ sudo apt update
    $ sudo apt install -y ca-certificates curl gnupg lsb-release
    $ curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
    $ echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] \
      https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list

    ## Install Containerd
    $ sudo apt update
    $ sudo apt install -y containerd.io
    $ sudo systemctl status containerd

    ## Containerd configuration for Kubernetes
    cat <<EOF | sudo tee -a /etc/containerd/config.toml
    [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.runc]
    [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.runc.options]
    SystemdCgroup = true
    EOF

    ## Enable CRI plugins
    $ sudo sed -i 's/^disabled_plugins \=/\#disabled_plugins \=/g' /etc/containerd/config.toml

    ## Restart the containerd service
    $ sudo systemctl restart containerd

## Install CNI Plugins For Containerd

    $ sudo mkdir -p /opt/cni/bin/
    $ wget https://github.com/containernetworking/plugins/releases/download/v1.1.1/cni-plugins-linux-amd64-v1.1.1.tgz
    $ sudo tar Cxzvf /opt/cni/bin cni-plugins-linux-amd64-v1.1.1.tgz
    $ sudo systemctl restart containerd

## Install nerdctl (CLI)

    ## nerdctl is a Docker-compliant command-line interface for containerd
    $ wget https://github.com/containerd/nerdctl/releases/download/v0.19.0/nerdctl-0.19.0-linux-amd64.tar.gz
    $ sudo tar Cxzvf /usr/local/bin nerdctl-0.19.0-linux-amd64.tar.gz

    ## A sample command
    $ sudo nerdctl run -d -p 80:80 --name=nginx nginx
