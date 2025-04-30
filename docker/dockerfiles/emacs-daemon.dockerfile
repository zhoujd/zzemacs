ARG VARIANT=22.04
FROM ubuntu:$VARIANT

USER root

RUN apt-get update \
        && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        apt-utils ca-certificates lsb-release software-properties-common gnupg xz-utils \
        sudo xauth iproute2 inetutils-ping net-tools socat dnsutils curl wget \
        gdb gdbserver openssh-server git patch tig bash-completion texinfo \
        silversearcher-ag cscope markdown pandoc w3m perl-doc ack-grep \
        python3-pip python3-venv python3-virtualenv

RUN mkdir -p /etc/apt/keyrings \
        && curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg \
        && echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" \
        | tee /etc/apt/sources.list.d/docker.list \
        && apt-get update \
        && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        docker-ce docker-ce-cli containerd.io docker-compose-plugin docker-buildx-plugin

ARG USER_NAME=zach
ARG USER_HOME=/home/$USER_NAME
ARG USER_UID=1000
ARG USER_GID=1000
ARG USER_SHELL=/bin/bash
RUN groupadd -g $USER_GID $USER_NAME \
        && useradd -d $USER_HOME -s $USER_SHELL -m $USER_NAME -u $USER_UID -g $USER_GID \
        && adduser $USER_NAME sudo \
        && echo "$USER_NAME ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/$USER_NAME

WORKDIR $USER_HOME
USER $USER_NAME
ENV HOME=$USER_HOME \
        USER=$USER_NAME \
        SHELL=$USER_SHELL \
        PATH=$USER_HOME/.local/bin:$PATH

ARG REPO=https://github.com/zhoujd/emacs.git
RUN git clone $REPO $HOME/emacs \
        && $HOME/emacs/build.sh all \
        && rm -rf $HOME/emacs

# Clean up APT when done
RUN sudo apt-get clean \
        && sudo rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

CMD ["bash"]
