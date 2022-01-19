Kubernetes Error
================

## kubelet cgroup driver: “systemd” is different from docker cgroup driver: “cgroupfs”

    $ cat /etc/default/kubelet
    KUBELET_EXTRA_ARGS="--cgroup-driver=cgroupfs"
    $ sudo systemctl daemon-reload
    $ sudo systemctl restart kubelet
    $ sudo systemctl status kubelet
    $ journalctl -xeu kubelet
