Kubernetes Error
================

## kubelet cgroup driver: “systemd” is different from docker cgroup driver: “cgroupfs”

    ## the same cgroupdriver between k8s and docker
    $ cat /etc/default/kubelet
    KUBELET_EXTRA_ARGS="--cgroup-driver=cgroupfs"
    $ sudo systemctl daemon-reload
    $ sudo systemctl restart kubelet
    $ sudo systemctl status kubelet
    $ journalctl -xeu kubelet

    cat /etc/docker/daemon.json
    {
      "exec-opts": ["native.cgroupdriver=cgroupfs"],
      "log-driver": "json-file",
      "log-opts": {
        "max-size": "100m"
      },
      "storage-driver": "overlay2"
    }

    $ sudo systemctl daemon-reload
    $ sudo systemctl restart docker
    $ sudo systemctl status docker
