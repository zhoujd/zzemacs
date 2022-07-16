JSON
=====

## JSON processor

    ## https://github.com/stedolan/jq
    $ sudo apt install jq
    $ echo '{"foo": 0}' | jq .
    $ echo '{"foo": 0}' | jq -r .foo
    $ echo '{"foo": "0"}' | jq .foo
    $ echo '{"foo": "0"}' | jq -r .foo

## Read Json from stdin

    #!/bin/bash
    # Read stdin
    conf=$(cat /dev/stdin)

    # Retrieve custom vars
    bridge=$(echo $conf | jq -r ".bridge")
    gateway=$(echo $conf | jq -r ".gateway)

## Modify a key-value in a json using jq in-place

    ## jq does not support in-place editing
    $ sudo apt install moreutils
    $ jq '.address = "abcde"' test.json | sponge test.json

## Updating JSON

    $ echo '{ "foo": "bar" }' | jq '.foo |= "baz"'

## Checking if an object has a key

    ## https://linuxconfig.org/how-to-parse-a-json-file-from-linux-command-line-using-jq
    ## Use the has function
    $ jq 'has("weapons")' characters.json
    false

    ## The “characters” array has only 3 elements
    $ jq '.characters | has(3)' characters.json
    false

    ## The map function
    $ jq '.characters | map(has("name"))' characters.json
    [
      true,
      true,
      true
    ]
