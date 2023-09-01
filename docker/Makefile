### Emacs in Docker

all: ubuntu2204

BUILD_PARM := --no-cache --build-arg id=$(shell id -u) --build-arg group=$(shell id -g)
BUILD_TAG := ubuntu:zach

ubuntu2204: ubuntu2204.dockerfile
	docker build $(BUILD_PARM) -t $(BUILD_TAG) -f $^ .
