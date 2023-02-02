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

## Use nerdctl for docker-compose.yaml

    ## https://github.com/containerd/nerdctl
    ## To run containers from docker-compose.yaml:
    $ nerdctl compose -f ./examples/compose-wordpress/docker-compose.yaml up


## Pull images via Proxy

    $ host=localhost
    $ port=913
    $ sudo mkdir /etc/systemd/system/containerd.service.d
    $ sudo tee /etc/systemd/system/containerd.service.d/http-proxy.conf <<EOF
    [Service]
    Environment="HTTP_PROXY=http://$host:$port"
    Environment="HTTPS_PROXY=http://$host:$port"
    Environment="FTP_PROXY=http://$host:$port"
    Environment="NO_PROXY=.intel.com,intel.com,localhost,127.0.0.0/8,10.0.0.0/8,172.16.0.0/12,192.168.0.0/16"
    EOF

    $ sudo systemctl daemon-reload
    $ sudo systemctl restart containerd
    $ sudo systemctl show --property=Environment containerd
    $ sudo nerdctl run hello-world

## Configuring nerdctl with nerdctl.toml

    ## https://github.com/containerd/nerdctl/blob/main/docs/config.md
    $ Rootful mode: /etc/nerdctl/nerdctl.toml
    $ Rootless mode: ~/.config/nerdctl/nerdctl.toml

## Using registry

    ## https://github.com/containerd/nerdctl/blob/main/docs/registry.md
    ## Using insecure registry
    $ nerdctl --insecure-registry run --rm 192.168.12.34:5000/foo

    ## Specifying certificates
    ## Create ~/.config/containerd/certs.d/<HOST:PORT>/hosts.toml (or /etc/containerd/certs.d/... for rootful) to specify ca certificates.
    # An example of ~/.config/containerd/certs.d/192.168.12.34:5000/hosts.toml
    # (The path is "/etc/containerd/certs.d/192.168.12.34:5000/hosts.toml" for rootful)

    server = "https://192.168.12.34:5000"
    [host."https://192.168.12.34:5000"]
      ca = "/path/to/ca.crt"

    ## Accessing 127.0.0.1 from rootless nerdctl
    ## Currently, rootless nerdctl cannot pull images from 127.0.0.1, because the pull operation occurs in RootlessKit's network namespace.

    ## Docker Hub login
    $ nerdctl login -u <USERNAME>
    Enter Password: ********[Enter]

    Login Succeeded

    ## Quay.io
    $ nerdctl login quay.io -u <USERNAME>
    Enter Password: ********[Enter]

    Login Succeeded
