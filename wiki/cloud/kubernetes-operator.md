Kubernetes Operator
===================

## Install operator sdk

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

    $ operator-sdk add api --api-version=<api-version> --kind=<kind-name>
    $ operator-sdk add api --api-version=test.k8s.realibox.com/v1 --kind=Realibox
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

    $ operator-sdk add controller --api-version=<api-version> --kind=<kind-name>
    $ operator-sdk add controller --api-version=test.k8s.realibox.com/v1 --kind=Realibox

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
    $ kubectl apply -f deploy/role.yamlkubectl apply -f deploy/role_binding.yamlkubectl apply -f deploy/operator.yaml
    $ kubectl get all

    ## Submit CR
    $ kubectl logs -f test-controller-75bf886d9c-whjdn
