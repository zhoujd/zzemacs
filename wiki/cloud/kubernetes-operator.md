Kubernetes Operator
===================

## Go Operator Tutorial

    ## https://sdk.operatorframework.io/docs/building-operators/golang/tutorial/

## Install operator sdk

    ## https://sdk.operatorframework.io/docs/contribution-guidelines/developer-guide/#prerequisites
    $ export RELEASE_VERSION=v1.8.1
    $ export RELEASE_PKG=operator-sdk_linux_amd64
    $ curl -LO https://github.com/operator-framework/operator-sdk/releases/download/${RELEASE_VERSION}/${RELEASE_PKG}
    $ chmod +x ${RELEASE_PKG}
    $ sudo mkdir -p /usr/local/bin/
    $ sudo cp ${RELEASE_PKG} /usr/local/bin/operator-sdk
    $ rm ${RELEASE_PKG}

## Create project base on template

    $ operator-sdk new <controller-name> --repo github.com/kubernetes/sample-controller

## Create CRD

    $ operator-sdk create api --api-version=<api-version> --kind=<kind-name>
    $ operator-sdk create api --api-version=test.k8s.realibox.com/v1 --kind=Realibox
    $ vim pkg/apis/test/v1/realibox_types.go
    // RealiboxSpec defines the desired state of Realibox
    type RealiboxSpec struct {
        // INSERT ADDITIONAL SPEC FIELDS - desired state of cluster
        // Important: Run "operator-sdk generate k8s" to regenerate code after modifying this file
        // Add custom validation using kubebuilder tags: https://book-v1.book.kubebuilder.io/beyond_basics/generating_crd.html
    }
    type RealiboxStatus struct {    // INSERT ADDITIONAL STATUS FIELD - define observed state of cluster
        // Important: Run "operator-sdk generate k8s" to regenerate code after modifying this file
        // Add custom validation using kubebuilder tags: https://book-v1.book.kubebuilder.io/beyond_basics/generating_crd.html
    }

    ## Modifying Spec
    type RealiboxSpec struct {
        Domain string `json:"domain,omitempty"`
        OSS    string `json:"oss,omitempty"`
        Size   string `json:"size,omitempty"`
    }

## Update CRD

    ## Update CRD
    $ operator-sdk generate k8s
    $ operator-sdk generate crds

    ## Create CRD on k8s
    $ kubectl apply -f deploy/crds/test.k8s.realibox.com_realiboxes_crd.yaml

    ## Check CRD on k8s cluster
    $ kubectl get crd
    NAME                                      CREATED AT
    clusterauthtokens.cluster.cattle.io       2020-08-29T06:41:42Z
    clusteruserattributes.cluster.cattle.io   2020-08-29T06:41:42Z
    realiboxes.test.k8s.realibox.com          2020-08-29T07:57:44Z

## Create Controller

    $ operator-sdk create controller --api-version=<api-version> --kind=<kind-name>
    $ operator-sdk create controller --api-version=test.k8s.realibox.com/v1 --kind=Realibox

    $ vim pkg/controller/realibox/realibox_controller.go
    ...
    func (r *ReconcileRealibox) Reconcile(request reconcile.Request) (reconcile.Result, error) {
        ...
        reqLogger.Info(fmt.Sprintf("Domain: %v created, oss info:%v, size: %v",instance.Spec.Domain,instance.Spec.OSS, instance.Spec.Size))
        // Define a new Pod object
        pod := newPodForCR(instance)
        ...
    }
    ...

    ##Use client-go for more complex control k8s resources

## Run controller
### Run on local

    $ export WATCH_NAMESPACE=default
    $ go run cmd/manager/main.go

    ## Create CRD first in k8s, both for local and k8s running
    $ kubectl apply -f <<EOF
    apiVersion: test.k8s.realibox.com/v1
    kind: Realibox
    metadata:
      name: example-realibox
    spec:
      domain: "realibox.com"
      oss: "aliyun.com"
      size: "3Gb"
    EOF

    ...
    {"level":"info","ts":1598689291.273161,"logger":"controller_realibox","msg":"Domain: realibox.com created, oss info:aliyun.com, size: 3Gb","Request.Namespace":"default","Request.Name":"example-realibox"}
    {"level":"info","ts":1598689291.2731829,"logger":"controller_realibox","msg":"Skip reconcile: Pod already exists","Request.Namespace":"default","Request.Name":"example-realibox","Pod.Namespace":"default","Pod.Name":"example-realibox-pod"}
    ...

### Run on k8s

    ## Build image
    $ operator-sdk build registry.**/realibox-operator-test:v0.1 --image-builder docker
    $ docker push registry.**/realibox-operator-test:v0.1

    ## Change image name
    $ vim deploy/operator.yaml
    image: registry.**/realibox-operator-test:v0.1

    ## Apply YAML
    $ kubectl apply -f deploy/service_account.yaml
    $ kubectl apply -f deploy/role.yaml
    $ kubectl apply -f deploy/role_binding.yaml
    $ kubectl apply -f deploy/operator.yaml
    $ kubectl get all

    ## Submit CR
    $ kubectl logs -f test-controller-75bf886d9c-whjdn

## Extend the Kubernetes API by creating our very own object/resource via CRDs

    ## Create the CRD
    $ cat my-new-crd.yaml
    apiVersion: apiextensions.k8s.io/v1beta1
    kind: CustomResourceDefinition
    metadata:
      name: mysql.db.example.com
    spec:
      group: db.example.com
      version: v1
      scope: Namespaced
      names:
        plural: mysqls
        singular: mysql
        kind: MySql
        shortNames:
        - ms
    $ kubectl create -f my-new-crd.yaml
    ## Verify CRD Creation via CLI
    $ kubectl get crd
    NAME                 KIND
    mysql.db.example.com CustomResourceDefinition.v1beta1.apiextensions.k8s.io
    $ curl -XGET localhost:8001/apis/apiextensions.k8s.io/v1beta1/customresourcedefinitions
    ...
    ## Verify New Database Resource via CLI
    $ kubectl get mysql
    No resources found.
    $ curl -XGET localhost:8001/apis/db.example.com/v1/namespaces/default/mysqls
    {
      "apiVersion": "db.example.com/v1",
      "items": [],
      "kind": "MySqlList",
      "metadata": {
        "resourceVersion": "240591",
        "selfLink": "/apis/stable.example.com/v1/namespaces/default/mysqls"
      }
    }
    ## Create a new mysql object
    $ cat new-mysql-object.yaml
    apiVersion: "db.example.com/v1"
    kind: MySql
    metadata:
      name: wordpress
    spec:
      user: wp
      password: secret
      foo: bar
    $ kubectl create -f new-mysql-object.yaml
    ## verify the creation of the mysql object
    $ kubectl get mysql
    $ kubectl get mysql wordpress -o yaml
    apiVersion: db.example.com/v1
    kind: MySql
    metadata:
      clusterName: ""
      creationTimestamp: 2017-10-14T03:23:26Z
      deletionGracePeriodSeconds: null
      deletionTimestamp: null
      name: wordpress
      namespace: default
      resourceVersion: "238701"
      selfLink: /apis/db.example.com/v1/namespaces/default/mysqls/wordpress
      uid: 0afd1584-b08f-11e7-9176-080027b424ef
    spec:
      foo: bar
      password: secret
      user: wp
    ## A Custom Resource needs a controller to ACT upon its presence
    ## Kubernetes Controllers
       Observe -> Analyze -> Act
    ## Need a custom controller to notice the new database object and ACT
    ## Operators! = Custom Resource Definitions (CRD) + Custom Controller + Your Knowledge!

## Kubernetes object reading

    $ oc proxy
    $ curl localhost:8001
    $ curl http://localhost:8001/api/v1/ | jq .resources[].name
    $ kubectl get pod kube-dns-1187388186-rr1jb -n kube-system -o yaml
    $ curl -XGET ../api/v1/namespaces/kube-system/pods/kube-dns-1187388186-rr1jb

## Build a Kubernetes Operator in 10 Minutes

    ## https://betterprogramming.pub/build-a-kubernetes-operator-in-10-minutes-11eec1492d30
    ## 1. Set up your environment
    ## Install kubebuilder
    $ curl -L -o kubebuilder https://go.kubebuilder.io/dl/latest/$(go env GOOS)/$(go env GOARCH) && chmod +x kubebuilder && sudo mv kubebuilder /usr/local/bin/
    $ kubebuilder version

    ## 2. Create a simple operator
    $ kubebuilder init --domain my.domain --repo my.domain/tutorial
    $ ls -a
    $ kubebuilder create api --group tutorial --version v1 --kind Foo

    ## 3. Customize the CRD and the controller
    ## Foo CRD customised (see api/v1/foo_types.go)
    $ make manifests

    ## 4. Run the controller
    $ make install
    $ kubectl get crds
    $ make run

    ## 5. Test the controller
    ## Foo custom resources manifests in config/sample
    $ kubectl apply -f config/samples
    $ kubectl describe foos
