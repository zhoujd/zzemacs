### Emacs in Docker

all: ubuntu2204

## --no-cache
BUILD_PARM := --build-arg UID=$(shell id -u) --build-arg GID=$(shell id -g) --build-arg USER=${USER}
BUILD_PARM += --build-arg DOCKER_GID=$(shell getent group docker | cut -d: -f3)
BUILD_TAG := ubuntu-22.04-zzemacs:zach

ubuntu2204: ubuntu2204.dockerfile
	docker build $(BUILD_PARM) -t $(BUILD_TAG) -f $^ .
