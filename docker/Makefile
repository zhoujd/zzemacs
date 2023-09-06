### Emacs in Docker

all: ubuntu2004 ubuntu2204

CTN_USER ?= ${USER}
BUILD_PARM += ## --no-cache
BUILD_PARM += --build-arg USER_NAME=$(CTN_USER)
BUILD_PARM += --build-arg USER_ID=$(shell id -u ${USER})
BUILD_PARM += --build-arg USER_GID=$(shell id -g ${USER})
BUILD_PARM += --build-arg DOCKER_GID=$(shell getent group docker | cut -d: -f3)
Ubuntu2004_TAG := ubuntu-2004-zzemacs:zach
Ubuntu2204_TAG := ubuntu-2204-zzemacs:zach

ubuntu2004: ubuntu2004.dockerfile
	docker build $(BUILD_PARM) -t $(Ubuntu2004_TAG) -f $^ .

ubuntu2204: ubuntu2204.dockerfile
	docker build $(BUILD_PARM) -t $(Ubuntu2204_TAG) -f $^ .

.PHONY: clean
clean:
	docker rmi $(Ubuntu2004_TAG)
	docker rmi $(Ubuntu2204_TAG)

.PHONY: help
help:
	@echo "Usage:"
	@echo "$$ make"
	@echo "$$ make CTN_USER=zach"
	@echo "$$ make clean"
	@echo "$$ make help"
	@echo "$$ make ubuntu2004"
	@echo "$$ make ubuntu2204"
	@echo "$$ make ubuntu2004 CTN_USER=zach"
	@echo "$$ make ubuntu2204 CTN_USER=zach"
