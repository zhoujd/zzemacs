YAML
====

## Install yq

    ## https://github.com/mikefarah/yq
    ## https://github.com/mikefarah/yq/releases/latest

    ## Binary install
    $ VERSION=v4.26.1
    $ BINARY=yq_linux_amd64
    $ wget https://github.com/mikefarah/yq/releases/download/${VERSION}/${BINARY} -O /usr/bin/yq
    $ chmod +x /usr/bin/yq

    ## Go install
    $ go install github.com/mikefarah/yq/v4@latest

## Quick guide of yq

    ## https://mikefarah.gitbook.io/yq/
    ## Read a value
    $ yq '.a.b[0].c' file.yaml
    ## Pipe from STDIN
    $ cat file.yaml | yq '.a.b[0].c'
    ## Update a yaml file, inplace
    $ yq -i '.a.b[0].c = "cool"' file.yaml
    ## Update using environment variables
    $ NAME=mike yq -i '.a.b[0].c = strenv(NAME)' file.yaml
    ## Merge multiple files
    $ yq ea '. as $item ireduce ({}; . * $item )' path/to/*.yml
    ## Multiple updates to a yaml file
    $ yq -i '
    .a.b[0].c = "cool" |
    .x.y.z = "foobar" |
    .person.name = strenv(NAME)
    ' file.yaml

## Use case scenarios

    ## Reading YAML values
    $ yq r pod.yaml "spec.containers[0].env[0].value"
    postgres://db_url:5432

    ## Changing YAML values
    $ yq w pod.yaml "spec.containers[0].env[0].value" "postgres://prod:5432"

    ## Merging YAML files
    $ yq m --append pod.yaml envoy-pod.yaml
