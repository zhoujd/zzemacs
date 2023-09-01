### Emacs in Docker

all: ubuntu2204

## --no-cache
BUILD_PARM := --build-arg UID=$(shell id -u) --build-arg GID=$(shell id -g)
BUILD_TAG := ubuntu:zach

ubuntu2204: ubuntu2204.dockerfile
	docker build $(BUILD_PARM) -t $(BUILD_TAG) -f $^ .
