ARG VARIANT=20.04
FROM zhoujd/ubuntu-${VARIANT}-zzemacs:base

## Setup apt-get
RUN sudo apt-get update

## Setup develop package
RUN sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        libdrm-dev libx11-dev libgl1-mesa-glx libgl1-mesa-dev \
        make automake autoconf libtool cmake g++ pkg-config \
        libncurses5-dev libpthread-stubs0-dev libpciaccess-dev libxvmc-dev \
        nasm yasm xutils-dev libsdl2-dev \
        gcc-multilib g++-multilib module-assistant ccache \
        texi2html bison flex libssl-dev \
        libkmod-dev libprocps-dev libunwind-dev libdw-dev \
        gtk-doc-tools \
        libgudev-1.0-dev \
        libgtest-dev

## Setup develop tool
RUN sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        gitk meld psmisc

## Setup python tool
RUN sudo pip3 install meson ninja

## Setup google-chrome
RUN wget -q https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
RUN sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        ./google-chrome-stable_current_amd64.deb
RUN rm -f google-chrome-stable_current_amd64.deb
RUN echo -n "Chrome: " && google-chrome --version


## Setup vscode
RUN mkdir -p /etc/apt/keyrings
RUN curl -fsSL https://packages.microsoft.com/keys/microsoft.asc | sudo gpg --dearmor -o /usr/share/keyrings/vscode.gpg
RUN echo "deb [arch=amd64 signed-by=/usr/share/keyrings/vscode.gpg] https://packages.microsoft.com/repos/vscode stable main" \
        | sudo tee /etc/apt/sources.list.d/vscode.list
RUN sudo apt-get update \
        && sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        code

## Setup libvirt
RUN sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        qemu qemu-utils qemu-kvm virt-manager libvirt-daemon-system libvirt-clients bridge-utils
RUN sudo usermod -aG kvm,libvirt,libvirt-qemu $USER

# Clean up APT when done.
RUN sudo apt-get clean \
        && sudo rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
