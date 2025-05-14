ARG VARIANT=12
FROM debian:$VARIANT

USER root

RUN apt-get update \
        && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        apt-utils ca-certificates lsb-release software-properties-common gnupg locales sudo \
        sudo xauth iproute2 inetutils-ping net-tools socat dnsutils curl wget lsof xz-utils \
        gdb gdbserver openssh-server git patch tig bash-completion texinfo \
        silversearcher-ag cscope markdown pandoc w3m perl-doc ccls \
        python3-pip python3-venv python3-virtualenv

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen \
        && locale-gen
ENV LC_ALL=en_US.UTF-8 \
        LANG=en_US.UTF-8 \
        LANGUAGE=en_US.UTF-8

RUN mkdir -p /etc/apt/keyrings \
        && curl -fsSL https://download.docker.com/linux/debian/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg \
        && echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/debian $(. /etc/os-release && echo "$VERSION_CODENAME") stable" \
        | tee /etc/apt/sources.list.d/docker.list \
        && apt-get update \
        && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        docker-ce docker-ce-cli containerd.io docker-compose-plugin docker-buildx-plugin

ARG USER_NAME=zach
ARG USER_HOME=/home/$USER_NAME
ARG USER_UID=1000
ARG USER_GID=1000
ARG USER_SHELL=/bin/bash
ARG DOCKER_GID=133
RUN groupadd -g $USER_GID $USER_NAME \
        && useradd -d $USER_HOME -s $USER_SHELL -m $USER_NAME -u $USER_UID -g $USER_GID \
        && adduser $USER_NAME sudo \
        && echo "$USER_NAME ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/$USER_NAME

ARG DOCKER_GID=133
RUN groupmod -g $DOCKER_GID docker \
        && usermod -aG docker $USER_NAME

WORKDIR $USER_HOME
USER $USER_NAME
ENV HOME=$USER_HOME \
        USER=$USER_NAME \
        SHELL=$USER_SHELL

# Build emacs
ARG EMACS_REPO=https://github.com/zhoujd/emacs.git
RUN git clone $EMACS_REPO $HOME/emacs \
        && $HOME/emacs/build.sh all \
        && rm -rf $HOME/emacs

# Build st
ARG ST_REPO=https://github.com/zhoujd/zzst.git
RUN git clone $ST_REPO $HOME/zzst \
        && $HOME/zzst/init.sh all \
        && rm -rf $HOME/zzst

# Install Bin
COPY bin/* /usr/local/bin

# Python ENV
ARG PYENV=$HOME/.venv/emacs
RUN python3 -m venv $PYENV \
        && $PYENV/bin/pip3 install --no-cache-dir \
        python-lsp-server[all] git-imerge

# Clean APT
RUN sudo apt-get clean \
        && sudo rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Setup entrypoint
RUN sudo mkdir -p /app
COPY entrypoint.sh /app
ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["run"]
