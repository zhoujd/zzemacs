Docker
======

1. Container entrypoint

```bash
tee entrypoint <<EOF
#!/bin/sh -e

cmd=${1:-""}
case ${cmd} in
   *)
      echo "invalid command ${cmd}"
      sleep infinity
      ;;
esac
EOF
```
