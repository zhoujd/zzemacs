### Emacs in Docker

.PHONY: all
all: ubuntu2004 ubuntu2204 ubuntu2004-dev ubuntu2204-dev

CTN_USER ?= ${USER}

BUILD_BASE += --rm ## --no-cache
BUILD_BASE += --build-arg USER_NAME=$(CTN_USER)
BUILD_BASE += --build-arg USER_UID=$(shell id -u ${USER})
BUILD_BASE += --build-arg USER_GID=$(shell id -g ${USER})
BUILD_BASE += --build-arg DOCKER_GID=$(shell getent group docker | cut -d: -f3)

BUILD_DEV += --rm ## --no-cache

Ubuntu2004_BASE := zz/ubuntu-20.04-zzemacs:base
Ubuntu2204_BASE := zz/ubuntu-22.04-zzemacs:base
Ubuntu2004_DEV := zz/ubuntu-20.04-zzemacs:dev
Ubuntu2204_DEV := zz/ubuntu-22.04-zzemacs:dev

ubuntu2004: ubuntu2004.dockerfile
	docker build $(BUILD_BASE) -t $(Ubuntu2004_BASE) -f $< .

ubuntu2204: ubuntu2204.dockerfile
	docker build $(BUILD_BASE) -t $(Ubuntu2204_BASE) -f $< .

ubuntu2004-dev: ubuntu2004-dev.dockerfile ubuntu2004
	docker build $(BUILD_DEV) -t $(Ubuntu2004_DEV) -f $< .

ubuntu2204-dev: ubuntu2204-dev.dockerfile ubuntu2204
	docker build $(BUILD_DEV) -t $(Ubuntu2204_DEV) -f $< .

.PHONY: clean
clean:
	docker rmi $(Ubuntu2004_BASE)
	docker rmi $(Ubuntu2204_BASE)
	docker rmi $(Ubuntu2004_DEV)
	docker rmi $(Ubuntu2204_DEV)

.PHONY: help
help:
	@echo "Usage:"
	@echo "$$ make"
	@echo "$$ make CTN_USER=zach"
	@echo "$$ make clean"
	@echo "$$ make help"
	@echo "$$ make ubuntu2004"
	@echo "$$ make ubuntu2204"
	@echo "$$ make ubuntu2004-dev"
	@echo "$$ make ubuntu2204-dev"
