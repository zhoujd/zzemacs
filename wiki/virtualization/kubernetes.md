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

