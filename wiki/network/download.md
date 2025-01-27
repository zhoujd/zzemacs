Download
========

## Build static axel for multi-thread download

```
## On Alpine
# wget https://github.com/axel-download-accelerator/axel/releases/download/v2.17.14/axel-2.17.14.tar.bz2
# tar xf axel-2.17.14.tar.bz2
# cd axel-2.17.14/
# ./configure
# apk update
# apk add openssl-libs-static
# make LDFLAGS=-static
```
