## Makefile

.PHONY: ubuntu-base ubuntu-dev debian-base debian-dev help

all: ubuntu debian
ubuntu: ubuntu-base ubuntu-dev
debian: debian-base debian-dev

ubuntu-base:
	make -C ubuntu-base

ubuntu-dev:
	make -C ubuntu-dev

debian-base:
	make -C debian-base

debian-dev:
	make -C debian-dev

help:
	@echo "Usage:"
	@echo "$$ make"
	@echo "$$ make all"
	@echo "$$ make help"
	@echo "$$ make ubuntu"
	@echo "$$ make debian"
