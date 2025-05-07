### Emacs in Docker

.PHONY: all
all: ubuntu-base

CTN_USER ?= ${USER}
VARIANT ?= 22.04
IMG_NS ?= zhoujd

BUILD_ARG += ## --rm --no-cache
BUILD_ARG += --build-arg VARIANT=$(VARIANT)
BUILD_ARG += --build-arg USER_NAME=$(CTN_USER)
BUILD_ARG += --build-arg USER_UID=$(shell id -u ${USER})
BUILD_ARG += --build-arg USER_GID=$(shell id -g ${USER})
BUILD_ARG += --build-arg DOCKER_GID=$(shell getent group docker | cut -d: -f3)

BUILD_DEV += ## --rm --no-cache
BUILD_DEV += --build-arg VARIANT=$(VARIANT)

Ubuntu_BASE_TAG := $(IMG_NS)/ubuntu-$(VARIANT)-zzemacs:base

ubuntu-base: ubuntu-base.dockerfile
	docker build $(BUILD_ARG) -t $(Ubuntu_BASE_TAG) -f $< .

.PHONY: clean
clean:
	docker rmi $(Ubuntu_BASE_TAG)

.PHONY: help
help:
	@echo "Usage:"
	@echo "$$ make"
	@echo "$$ make CTN_USER=zach VARIANT=22.04"
	@echo "$$ make clean"
	@echo "$$ make help"
	@echo "$$ make ubuntu-base"
