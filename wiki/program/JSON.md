JSON
=====

## JSON processor
```
## https://github.com/stedolan/jq
$ sudo apt install jq
$ echo '{"foo": 0}' | jq .
$ echo '{"foo": 0}' | jq -r .foo
$ echo '{"foo": "0"}' | jq .foo
$ echo '{"foo": "0"}' | jq -r .foo
```

## Read Json from stdin
```
#!/bin/bash
# Read stdin
conf=$(cat /dev/stdin)

# Retrieve custom vars
bridge=$(echo $conf | jq -r ".bridge")
gateway=$(echo $conf | jq -r ".gateway)

```
