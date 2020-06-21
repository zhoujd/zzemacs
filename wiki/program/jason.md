JASON
=====


1. JSON processor

        ## https://github.com/stedolan/jq
        $ sudo apt install jq
        $ echo '{"foo": 0}' | jq .
        $ echo '{"foo": 0}' | jq -r .foo
        $ echo '{"foo": "0"}' | jq .foo
        $ echo '{"foo": "0"}' | jq -r .foo
