ARM
===

## Arm in container

    ## https://github.com/multiarch/qemu-user-static
    $ uname -m
    x86_64
    $ docker run --rm -t arm64v8/ubuntu uname -m
    standard_init_linux.go:211: exec user process caused "exec format error"
    $ docker run --rm --privileged multiarch/qemu-user-static --reset -p yes
    $ docker run --rm -t arm64v8/ubuntu uname -m
    aarch64

    ## https://repo.openeuler.org/openEuler-22.03-LTS/docker_img/aarch64/
    $ docker run --rm -it \
    -v /usr/bin/qemu-aarch64-static:/usr/bin/qemu-aarch64-static \
    -v /etc/timezone:/etc/timezone:ro \
    -v /etc/localtime:/etc/localtime:ro \
    openeuler-22.03-lts \
    bash
