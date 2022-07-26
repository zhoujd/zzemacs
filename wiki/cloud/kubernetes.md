Kubernetes
==========

## Kubernetes delete all evicted pods

    $ kubectl get pods --all-namespaces -owide | grep Evicted | awk '{ printf "kubectl delete pods -n %s %s --force --grace-period 0\n", $1, $2}' | sh
    $ kubectl get pods -A | grep Evicted | awk '{print $2}' | xargs kubectl delete pods -A

## Weave test

    $ docker run --name hello ubuntu netcat -lk 1234
    Hello, Weave!

    $ docker run -it ubuntu
    ## in container
    $ ping -c1 hello
    $ echo 'Hello, Weave!' | netcat hello 1234
    $ echo "What's up?" | nc hello 1234

## Debug Service

   https://kubernetes.io/docs/tasks/debug-application-cluster/debug-service/
   https://www.weave.works/docs/net/latest/troubleshooting/

## Install kubernetes (k8s) on Ubuntu

    ## Step 1: Install Docker
    $ sudo apt install docker.io
    $ docker --version

    ## Step 2: Start and Enable Docker
    $ sudo systemctl enable docker
    $ sudo systemctl status docker
    $ sudo systemctl start docker

    ## Step 3: Add Kubernetes Signing Key
    $ sudo apt install apt-transport-https ca-certificates curl
    $ sudo curl -fsSLo /usr/share/keyrings/kubernetes-archive-keyring.gpg https://packages.cloud.google.com/apt/doc/apt-key.gpg
    $ echo "deb [signed-by=/usr/share/keyrings/kubernetes-archive-keyring.gpg] https://apt.kubernetes.io/ kubernetes-xenial main" | sudo tee /etc/apt/sources.list.d/kubernetes.list

    ## Step 4: Add Software Repositories
    $ sudo apt-add-repository "deb http://apt.kubernetes.io/ kubernetes-xenial main"
    or
    $ echo "deb https://apt.kubernetes.io/ kubernetes-xenial main" | sudo tee -a /etc/apt/sources.list.d/kubernetes.list

    ## Step 5: Kubernetes Installation Tools
    ## https://kubernetes.io/docs/tasks/tools/install-kubectl-linux/
    $ sudo apt install kubeadm kubelet kubectl
    $ sudo apt-mark hold kubeadm kubelet kubectl
    $ kubeadm version
    $ sudo systemctl enable  kubelet.service

    ## Step 6: Begin Kubernetes Deployment
    $ sudo swapoff -a
    $ sudo sed -i '/ swap / s/^\(.*\)$/#\1/g' /etc/fstab
    $ sudo sed -i '/ swap / s/^/#/' /etc/fstab

    ## Step 7: Assign Unique Hostname for Each Server Node
    $ sudo hostnamectl set-hostname master-node
    $ sudo hostnamectl set-hostname worker01

    ## Step 8: Initialize Kubernetes on Master Node
    $ sudo kubeadm init --pod-network-cidr=10.245.0.0/16

    ## Specify a network interface when initialize the Kubernetes cluster
    $ sudo kubeadm init --pod-network-cidr=10.245.0.0/16 --apiserver-advertise-address=<ip> --service-cidr=10.1.0.0/16
    $ sudo kubeadm init --pod-network-cidr=10.245.0.0/16 --apiserver-advertise-address=<hostname>
    $ sudo kubeadm init --pod-network-cidr=10.245.0.0/16 --control-plane-endpoint=<ip>
    $ sudo kubeadm init --pod-network-cidr=10.245.0.0/16 --control-plane-endpoint=<hostname>

    kubernetes-master:~$ mkdir -p $HOME/.kube
    kubernetes-master:~$ sudo cp -i /etc/kubernetes/admin.conf $HOME/.kube/config
    kubernetes-master:~$ sudo chown $(id -u):$(id -g) $HOME/.kube/config

    ## Step 9: Deploy Pod Network to Cluster

    ## multus-cni
    kubernetes-master:~$ git clone https://github.com/k8snetworkplumbingwg/multus-cni && cd multus-cni
    kubernetes-master:~$ cat ./deployments/multus-daemonset-thick-plugin.yml | kubectl apply -f -
    ## flannel
    kubernetes-master:~$ kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/master/Documentation/kube-flannel.yml
    ## calico
    kubernetes-master:~$ kubectl apply -f https://docs.projectcalico.org/manifests/calico.yaml

    ## Redefine CIDR
    kubernetes-master:~$ curl https://docs.projectcalico.org/manifests/calico.yaml -O
    kubernetes-master:~$ vim calio.yaml
                         - name: CALICO_IPV4POOL_CIDR
                           value: "10.245.0.0/16"
    kubernetes-master:~$ kubectl apply -f calico.yaml

    kubernetes-master:~$ kubectl cluster-info
    kubernetes-master:~$ kubectl get pods --all-namespaces

    ## Remove the taint to be able to schedule Pods on the control-plane node (single_node_deployment)
    kubernetes-master:~$ kubectl taint nodes --all node-role.kubernetes.io/master-

    ## Step 10: Join Worker Node to Cluster
    kubernetes-master:~$ kubeadm token create --print-join-command --ttl=10m --description="token for kubernetes-master"
    kubernetes-worker01:~$ kubeadm join --discovery-token abcdef.1234567890abcdef --discovery-token-ca-cert-hash sha256:1234..cdef 1.2.3.4:6443
    kubernetes-master:~$ kubectl get nodes
    kubernetes-master:~$ kubectl label node worker01 node-role.kubernetes.io/worker=worker --overwrite

## nodeSelector must be wrapped with a spec. Like so

    $ kubectl run --generator=run-pod/v1 -ti --rm test --image=ubuntu:18.04 --overrides='{"spec": { "nodeSelector": {"nodename": "eks-prod-4"}}}'

## Kubernetes multi-container pods and container communication

    ## https://www.mirantis.com/blog/multi-container-pods-and-container-communication-in-kubernetes/
    $ kubectl exec mc1 -c 2nd -- /bin/cat /html/index.html

## Calicoctl

    $ curl -o calicoctl -O -L  "https://github.com/projectcalico/calicoctl/releases/download/v3.20.2/calicoctl"
    $ chmod +x calicoctl
    $ curl -o kubectl-calico -O -L  "https://github.com/projectcalico/calicoctl/releases/download/v3.20.2/calicoctl"
    $ chmod +x kubectl-calico

## How to limit the number of CPUs in Kubernetes

    ## https://community.denodo.com/kb/en/view/document/How%20to%20limit%20the%20number%20of%20CPUs%20in%20Kubernetes?category=Operation
    ## https://kubernetes.io/docs/tasks/administer-cluster/cpu-management-policies/
    ## Check the number of cores in your nodes
    $ kubectl get nodes -o custom-columns=NODE:.metadata.name,CPU:.status.capacity.cpu

    ## The Kubernetes CPU Manager allows two different policies: none and static
    ## default policy “none” does not really provide any affinity to the pods,
    ## so the restrictions applied in the Pod definition are enforced using CFS quota.
    ## the static policy allows to assign dedicated CPUs on the node to a pod,
    ## and that CPU will be removed from the shared pool and not available for other processes.
    ## For this, the pods must run in Guaranteed QoS mode and request full cores (not fractions).
    ## The other pods running in Burstable mode or Guaranteed but with fractional CPU requests run with the shared pool of CPUs.

    ## CPUManager Feature Gate
    ## https://kubernetes.io/docs/reference/command-line-tools-reference/feature-gates/
    ## Make sure CPUManager Feature Gate is true (BETA - default=true) in kubelet

    ## Enable static policy
    ## Step 1:
    ## apply the CPU Manager configuration into the kubelet.service file
    $ sudo vi /etc/systemd/system/kubelet.service
    ...
    --cpu-manager-policy=static \
    --kube-reserved=cpu=1,memory=200Mi,ephemeral-storage=1Gi \
    --system-reserved=cpu=1,memory=200Mi,ephemeral-storage=1Gi \
    ...
    ## Step 2
    ## reset the CPU Manager by deleting the folder /var/lib/kubelet/cpu_manager_state and restarting the kubelet:
    $ sudo rm -rf /var/lib/kubelet/cpu_manager_state
    $ sudo systemctl daemon-reload
    $ sudo systemctl stop kubelet
    $ sudo systemctl start kubelet
    ## Step 3
    ## Deploy your Denodo pods in Guaranteed mode and request full cores in the pod definition.
    ## For instance, you can append the following to the pod YAML to request 1 exclusive core
    ...
    resources:
      limits:
        memory: "2000Mi"
        cpu: "1"
    ...

    ## Testing the CPU Manager (Java Code)
    public class cores {
        public static void main(String[] args){
            Runtime runtime = Runtime.getRuntime();
            int num_cores = runtime.availableProcessors();
            System.out.println(num_cores);
        }
    }

## How to configure kubectl with cluster information from a .conf file?

    ## http://kubernetes.io/docs/user-guide/kubeconfig-file/
    $ export KUBECONFIG=.kubeconfig:$HOME/.kube/config
    $ alias k8='kubectl'
    $ alias k8prd='kubectl --kubeconfig ~/.kube/config_prd.conf'
    $ kubectl --kubeconfig

## Get the POD and Node IP from inside a Pod?

    containers:
    - env:
      - name: MY_POD_IP
        valueFrom:
          fieldRef:
            fieldPath: status.podIP
      - name: NODE_IP
        valueFrom:
          fieldRef:
            fieldPath: status.hostIP

    $ kubectl get pods "${PodName}" -o json | jq .status.podIP | tr -d "\"")
    $ kubectl get pods "${PodName}" -o json | jq .status.hostIP | tr -d "\"")

## Add a route for the NIC in container in POD

    $ ContainerID=$(docker ps -f name=<app name> -q)
    $ ContainerID=$(docker ps | grep <app name>
    $ ProcessID=$(docker inspect -f {{.State.Pid}} $ContainerID)
    $ nsenter -n -t $ProcessID
    $ route add -net xxx.xxx.1.0 netmask 255.255.255.0 gw x.x.x.1
