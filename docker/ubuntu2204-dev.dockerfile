FROM ubuntu-2204-zzemacs:latest

USER root

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update
RUN apt-get install -y --no-install-recommends \
        libdrm-dev libx11-dev libgl1-mesa-glx libgl1-mesa-dev \
        make automake autoconf libtool cmake g++ pkg-config \
        libncurses5-dev libpthread-stubs0-dev libpciaccess-dev libxvmc-dev \
        nasm yasm xutils-dev libsdl2-dev \
        gcc-multilib g++-multilib module-assistant ccache \
        texi2html bison flex libssl-dev \
        libkmod-dev libprocps-dev libunwind-dev libdw-dev \
        gtk-doc-tools \
        libgudev-1.0-dev \
        libgtest-dev \
        && apt-get autoremove \
        && apt-get clean

ARG USER_NAME=zach
USER $USER_NAME
