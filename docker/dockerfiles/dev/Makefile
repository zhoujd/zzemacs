### Emacs in Docker

.PHONY: all
all: ubuntu-dev

CTN_USER ?= ${USER}
VARIANT ?= 22.04
IMG_NS ?= zhoujd

BUILD_DEV += ## --rm --no-cache
BUILD_DEV += --build-arg VARIANT=$(VARIANT)

Ubuntu_DEV_TAG := $(IMG_NS)/ubuntu-$(VARIANT)-zzemacs:dev

ubuntu-dev: ubuntu-dev.dockerfile
	docker build $(BUILD_DEV) -t $(Ubuntu_DEV_TAG) -f $< .

.PHONY: clean
clean:
	docker rmi $(Ubuntu_DEV_TAG)

.PHONY: help
help:
	@echo "Usage:"
	@echo "$$ make"
	@echo "$$ make CTN_USER=zach VARIANT=22.04"
	@echo "$$ make clean"
	@echo "$$ make help"
	@echo "$$ make ubuntu-dev"
