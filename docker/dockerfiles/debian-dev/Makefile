### Emacs in Docker

.PHONY: all
all: debian-dev

CTN_USER ?= ${USER}
VARIANT ?= 12
IMG_NS ?= zhoujd

BUILD_DEV += ## --rm --no-cache
BUILD_DEV += --build-arg VARIANT=$(VARIANT)

Debian_DEV_TAG := $(IMG_NS)/debian-$(VARIANT)-zzemacs:dev

debian-dev: debian-dev.dockerfile
	docker build $(BUILD_DEV) -t $(Debian_DEV_TAG) -f $< .

.PHONY: clean
clean:
	docker rmi $(Debian_DEV_TAG)

.PHONY: help
help:
	@echo "Usage:"
	@echo "$$ make"
	@echo "$$ make CTN_USER=zach VARIANT=12"
	@echo "$$ make clean"
	@echo "$$ make help"
	@echo "$$ make debian-dev"
