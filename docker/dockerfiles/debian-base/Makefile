### Emacs in Docker

.PHONY: all
all: debian-base

CTN_USER ?= ${USER}
VARIANT ?= 12
IMG_NS ?= zhoujd

BUILD_ARG += ## --rm --no-cache
BUILD_ARG += --build-arg VARIANT=$(VARIANT)
BUILD_ARG += --build-arg USER_NAME=$(CTN_USER)
BUILD_ARG += --build-arg USER_UID=$(shell id -u ${USER})
BUILD_ARG += --build-arg USER_GID=$(shell id -g ${USER})
BUILD_ARG += --build-arg DOCKER_GID=$(shell getent group docker | cut -d: -f3)

BUILD_DEV += ## --rm --no-cache
BUILD_DEV += --build-arg VARIANT=$(VARIANT)

Debian_BASE_TAG := $(IMG_NS)/debian-$(VARIANT)-zzemacs:base

debian-base: debian-base.dockerfile
	docker build $(BUILD_ARG) -t $(Debian_BASE_TAG) -f $< .

.PHONY: clean
clean:
	docker rmi $(Debian_BASE_TAG)

.PHONY: help
help:
	@echo "Usage:"
	@echo "$$ make"
	@echo "$$ make CTN_USER=zach VARIANT=12"
	@echo "$$ make clean"
	@echo "$$ make help"
	@echo "$$ make debian-base"
