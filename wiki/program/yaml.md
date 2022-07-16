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
