YAML
====

## Install yq

    ## https://github.com/mikefarah/yq
    ## https://github.com/mikefarah/yq/releases/latest

    ## Binary install
    $ VERSION=v4.26.1
    $ BINARY=yq_linux_amd64
    $ sudo wget https://github.com/mikefarah/yq/releases/download/${VERSION}/${BINARY} -O /usr/bin/yq
    $ sudo chmod +x /usr/bin/yq

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

    ## Delete nested entry in map
    $ yq 'del(.a.a1)' sample.yml

## Use case scenarios

    ## https://mikefarah.gitbook.io/yq/v/v2.x/write-update
    ## https://github.com/mikefarah/yq
    ## Reading YAML values
    $ yq "spec.containers[0].env[0].value" pod.yaml
    postgres://db_url:5432

    ## Changing YAML values
    $ yq -i "spec.containers[0].env[0].value" "postgres://prod:5432" pod.yaml

## How to prevent yq removing comments and empty lines

    ## https://stackoverflow.com/questions/57627243/how-to-prevent-yq-removing-comments-and-empty-lines
    $ yq . input.yml > input.yml.1
    $ yq e .env.recording=false input.yml > input.yml.2
    $ diff input.yml.1 input.yml.2 > input.yml.diff
    $ patch -o input.yml.new input.yml < input.yml.diff
