### Emacs in Docker

all: ubuntu2204 ubuntu1804

## --no-cache
BUILD_PARM := --build-arg UID=$(shell id -u) --build-arg GID=$(shell id -g) --build-arg USER=${USER}
BUILD_PARM += --build-arg DOCKER_GID=$(shell getent group docker | cut -d: -f3)
Ubuntu22_TAG := ubuntu-22.04-zzemacs:zach
Ubuntu18_TAG := ubuntu-18.04-zzemacs:zach

ubuntu2204: ubuntu2204.dockerfile
	docker build $(BUILD_PARM) -t $(Ubuntu22_TAG) -f $^ .

ubuntu1804: ubuntu1804.dockerfile
	docker build $(BUILD_PARM) -t $(Ubuntu18_TAG) -f $^ .

clean:
	docker rmi $(Ubuntu22_TAG)
	docker rmi $(Ubuntu18_TAG)
