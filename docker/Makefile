### Emacs in Docker

all: ubuntu2204

CTN_USER ?= ${USER}
BUILD_PARM += --no-cache
BUILD_PARM += --build-arg USER_NAME=$(CTN_USER)
BUILD_PARM += --build-arg USER_ID=$(shell id -u ${USER})
BUILD_PARM += --build-arg USER_GID=$(shell id -g ${USER})
BUILD_PARM += --build-arg DOCKER_GID=$(shell getent group docker | cut -d: -f3)
Ubuntu22_TAG := ubuntu-22.04-zzemacs:zach

ubuntu2204: ubuntu2204.dockerfile
	docker build $(BUILD_PARM) -t $(Ubuntu22_TAG) -f $^ .

.PHONY: clean
clean:
	docker rmi $(Ubuntu22_TAG)
