K8s CNI sample
==============

## CNI test POD
```
apiVersion: v1
kind: Pod
metadata:
  name: overlay2pod-hw-device
  annotations:
    k8s.v1.cni.cncf.io/networks: host-devices-a

spec:
  containers:
  - name: overlay2pod
    image: docker.io/centos/tools:latest
    command:
      - /sbin/init
```

## CNI host device
```
apiVersion: k8s.cni.cncf.io/v1
kind: NetworkAttachmentDefinition
metadata:
  name: host-devices-a
  annotations:
    k8s.v1.cni.cncf.io/resourceName: host-devices
spec:
  config: |
    {
      "cniVersion": "0.3.0",
      "type": "host-device",
      "device": "ens260f1",
      "ipam": {
         "type": "dhcp"
      }
    }
```

## CNI macvlan
```
apiVersion: "k8s.cni.cncf.io/v1"
kind: NetworkAttachmentDefinition
metadata:
  name: macvlan-conf-3
spec:
  config: '{
            "cniVersion": "0.3.0",
            "type": "macvlan",
            "master": "ens260f0",
            "mode": "bridge",
            "ipam": {
                "type": "dhcp"
            }
        }'
```

## CNI bridge
```
apiVersion: k8s.cni.cncf.io/v1
kind: NetworkAttachmentDefinition
metadata:
  name: bridge-network
  annotations:
    k8s.v1.cni.cncf.io/resourceName: bridge.network.kubevirt.io/br0
spec:
  config: >
    {
        "cniVersion": "0.3.1",
        "name": "br-aeed45cb0408",
        "plugins": [{
            "type": "bridge",
            "bridge": "br0",
            "ipam": {
                "type": "static",
                 "addresses": [
                        {
                                "address": "172.19.0.77/32",
                                "gateway": "172.19.0.1"
                        },
                        {
                                "address": "3ffe:ffff:0:01ff::1/64",
                                "gateway": "3ffe:ffff:0::1"
                        }
                ]
            }
        }]
    }

```
