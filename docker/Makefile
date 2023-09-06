### Emacs in Docker

.PHONY: all
all: ubuntu2004 ubuntu2204 ubuntu2004-dev ubuntu2204-dev

CTN_USER ?= ${USER}

BUILD_PAR += --rm ## --no-cache
BUILD_PAR += --build-arg USER_NAME=$(CTN_USER)
BUILD_PAR += --build-arg USER_ID=$(shell id -u ${USER})
BUILD_PAR += --build-arg USER_GID=$(shell id -g ${USER})
BUILD_PAR += --build-arg DOCKER_GID=$(shell getent group docker | cut -d: -f3)

BUILD_DEV += --rm ## --no-cache

Ubuntu2004_TAG := ubuntu-2004-zzemacs:latest
Ubuntu2204_TAG := ubuntu-2204-zzemacs:latest
Ubuntu2004_DEV := ubuntu-2004-zzemacs:dev
Ubuntu2204_DEV := ubuntu-2204-zzemacs:dev

ubuntu2004: ubuntu2004.dockerfile
	docker build $(BUILD_PAR) -t $(Ubuntu2004_TAG) -f $< .

ubuntu2204: ubuntu2204.dockerfile
	docker build $(BUILD_PAR) -t $(Ubuntu2204_TAG) -f $< .

ubuntu2004-dev: ubuntu2004-dev.dockerfile ubuntu2004
	docker build $(BUILD_DEV) -t $(Ubuntu2004_DEV) -f $< .

ubuntu2204-dev: ubuntu2204-dev.dockerfile ubuntu2204
	docker build $(BUILD_DEV) -t $(Ubuntu2204_DEV) -f $< .

.PHONY: clean
clean:
	docker rmi $(Ubuntu2004_TAG)
	docker rmi $(Ubuntu2204_TAG)
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
