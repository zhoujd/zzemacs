ARG VARIANT=20.04
FROM ubuntu:$VARIANT

USER root

RUN apt-get update \
        && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        apt-utils ca-certificates lsb-release software-properties-common gnupg xz-utils \
        sudo xauth iproute2 inetutils-ping net-tools socat dnsutils curl wget \
        gdb gdbserver openssh-server git patch bash-completion texinfo \
        silversearcher-ag cscope markdown pandoc w3m perl-doc \
        python3-pip python3-venv python3-virtualenv \
        emacs vim rxvt-unicode tmux nnn sbcl

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
ARG USER_PASSWD=123456
ARG USER_SHELL=/bin/bash
RUN groupadd -g $USER_GID $USER_NAME \ 
        && useradd -d $USER_HOME -s $USER_SHELL -m $USER_NAME -u $USER_UID -g $USER_GID \
        && echo $USER_NAME:$USER_PASSWD | chpasswd \
        && adduser $USER_NAME sudo \
        && echo "$USER_NAME ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/$USER_NAME

ARG ROOT_PASSWD=$USER_PASSWD
RUN echo root:$ROOT_PASSWD | chpasswd

RUN mkdir -p /app \
        && mkdir -p /var/run/sshd

RUN sed -i \
        -e 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' \
        -e 's/#PermitRootLogin yes/PermitRootLogin yes/g' \
        -e 's/#AllowTcpForwarding yes/AllowTcpForwarding yes/g' \
        -e 's/#X11Forwarding yes/X11Forwarding yes/g' \
        -e 's/#X11DisplayOffset 10/X11DisplayOffset 10/g' \
        -e 's/#X11UseLocalhost yes/X11UseLocalhost no/g' \
        /etc/ssh/sshd_config

ARG DOCKER_GID=133
RUN groupmod -g $DOCKER_GID docker \
        && usermod -aG docker $USER_NAME

# Clean up APT when done.
RUN apt-get clean \
        && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

WORKDIR $USER_HOME
USER $USER_NAME
ENV HOME=$USER_HOME \
        USER=$USER_NAME \
        SHELL=$USER_SHELL \
        PATH=$USER_HOME/.local/bin:$PATH

RUN touch $HOME/.Xauthority

COPY requirements.txt /app
RUN pip3 install --requirement /app/requirements.txt

COPY entrypoint.sh /app
ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["init"]
