### Emacs in Docker

.PHONY: all
all: ubuntu-base ubuntu-dev

CTN_USER ?= ${USER}
VARIANT ?= 20.04

BUILD_ARG += ## --rm --no-cache
BUILD_ARG += --build-arg VARIANT=$(VARIANT)
BUILD_ARG += --build-arg USER_NAME=$(CTN_USER)
BUILD_ARG += --build-arg USER_UID=$(shell id -u ${USER})
BUILD_ARG += --build-arg USER_GID=$(shell id -g ${USER})
BUILD_ARG += --build-arg DOCKER_GID=$(shell getent group docker | cut -d: -f3)

BUILD_DEV += ## --rm --no-cache
BUILD_DEV += --build-arg VARIANT=$(VARIANT)

Ubuntu_BASE_TAG := zz/ubuntu-$(VARIANT)-zzemacs:base
Ubuntu_DEV_TAG := zz/ubuntu-$(VARIANT)-zzemacs:dev

ubuntu-base: ubuntu-base.dockerfile
	docker build $(BUILD_ARG) -t $(Ubuntu_BASE_TAG) -f $< .

ubuntu-dev: ubuntu-dev.dockerfile ubuntu-base
	docker build $(BUILD_DEV) -t $(Ubuntu_DEV_TAG) -f $< .

.PHONY: clean
clean:
	-docker rmi $(Ubuntu_BASE_TAG)
	-docker rmi $(Ubuntu_DEV_TAG)

.PHONY: help
help:
	@echo "Usage:"
	@echo "$$ make"
	@echo "$$ make CTN_USER=zach VARIANT=20.04"
	@echo "$$ make clean"
	@echo "$$ make help"
	@echo "$$ make ubuntu-base"
	@echo "$$ make ubuntu-dev"
