helm
====

## How to set multiple values with helm

    ## helm install [NAME] [CHART] [flags]
    ## field:
    ##  - value1
    ##  - value2
    ##  - value3
    ## --set field[0]=value1 --set field[1]=value2 --set field[2]=value3
    ## --set field={value1,value2,value3}

## Deprecating Charts

    ## https://v2.helm.sh/docs/developing_charts/
    ## These dependencies can be dynamically linked through the requirements.yaml file
    ## or brought in to the charts/ directory and managed manually
    ## A requirements.yaml file is a simple file for listing your dependencies
    dependencies:
      - name: apache
        version: 1.2.3
        repository: http://example.com/charts
      - name: mysql
        version: 3.2.1
        repository: http://another.example.com/charts

    $ helm dep up foochart
    $ helm dependency update
    charts/
      apache-1.2.3.tgz
      mysql-3.2.1.tgz

## Using Helm To Manage Charts

    $ helm create mychart
    Created mychart/
    $ helm package mychart
    Archived mychart-0.1.-.tgz
    $ helm lint mychart
    No issues found

## Hosting Chart Repositories

    ## https://chartmuseum.com/
    ## https://github.com/helm/chartmuseum
    $ helm install stable/chartmuseum

    ## As a Docker image
    ## https://hub.docker.com/r/chartmuseum/chartmuseum/tags
    docker run --rm -it \
      -p 8080:8080 \
      -v $(pwd)/charts:/charts \
      -e DEBUG=true \
      -e STORAGE=local \
      -e STORAGE_LOCAL_ROOTDIR=/charts \
      chartmuseum/chartmuseum

    ## add the repo
    $ helm repo add chartmuseum http://localhost:8080

## Set Up A Local Chart Repository Directory

    ## Local Chart Repository Directory
    $ mkdir fantastic-charts
    $ mv alpine-0.1.0.tgz fantastic-charts/

    ## generate an updated index.yaml
    $ helm repo index fantastic-charts/ --url https://fantastic-charts.storage.googleapis.com

    ## verify a chart using helm verify
    $ helm verify mychart-0.1.0.tgz
