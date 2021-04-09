Kubernetes
==========

1. kubernetes delete all evicted pods

        $ kubectl get pods --all-namespaces -owide | grep Evicted | awk '{ printf "kubectl delete pods -n %s %s --force --grace-period 0\n", $1, $2}' | sh
        $ kubectl get pods -A | grep Evicted | awk '{print $2}' | xargs kubectl delete pods -A

2. weave test

        $ docker run --name hello ubuntu netcat -lk 1234
        Hello, Weave!

        $ docker run -it ubuntu
        ## in container
        $ ping -c1 hello
        $ echo 'Hello, Weave!' | netcat hello 1234
        $ echo "What's up?" | nc hello 1234

3. Debug Service

   https://kubernetes.io/docs/tasks/debug-application-cluster/debug-service/
   https://www.weave.works/docs/net/latest/troubleshooting/

4. Install kubernetes (k8s) on Ubuntu

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

        ## Step 6: Begin Kubernetes Deployment
        $ sudo swapoff –a

        ## Step 7: Assign Unique Hostname for Each Server Node
        $ sudo hostnamectl set-hostname master-node
        $ sudo hostnamectl set-hostname worker01

        ## Step 8: Initialize Kubernetes on Master Node
        $ sudo kubeadm init --pod-network-cidr=10.244.0.0/16
        or
        $ sudo kubeadm init --pod-network-cidr=10.244.0.0/16 --control-plane-endpoint=<hostname>

        kubernetes-master:~$ mkdir -p $HOME/.kube
        kubernetes-master:~$ sudo cp -i /etc/kubernetes/admin.conf $HOME/.kube/config
        kubernetes-master:~$ sudo chown $(id -u):$(id -g) $HOME/.kube/config

        ## Step 9: Deploy Pod Network to Cluster
        kubernetes-master:~$ kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/master/Documentation/kube-flannel.yml
        or
        kubernetes-master:~$ kubectl apply -f https://docs.projectcalico.org/manifests/calico.yaml

        kubernetes-master:~$ kubectl cluster-info
        kubernetes-master:~$ kubectl get pods --all-namespaces

        ## Step 10: Join Worker Node to Cluster
        kubernetes-master:~$ kubeadm token create --print-join-command --ttl=10m --description="token for kubernetes-master"
        kubernetes-worker01:~$ kubeadm join --discovery-token abcdef.1234567890abcdef --discovery-token-ca-cert-hash sha256:1234..cdef 1.2.3.4:6443
        kubernetes-master:~$ kubectl get nodes
        kubernetes-master:~$ kubectl label node worker01 node-role.kubernetes.io/worker=worker --overwrite

5. nodeSelector must be wrapped with a spec. Like so

        $ kubectl run --generator=run-pod/v1 -ti --rm test --image=ubuntu:18.04 --overrides='{"spec": { "nodeSelector": {"nodename": "eks-prod-4"}}}'

6.
