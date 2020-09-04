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

2. Redirecting command output in docker

        $ docker run -it --log-driver=none -a stdin -a stdout -a stderr 

        ## When command in background, need use 'wait' to background
        process ending
