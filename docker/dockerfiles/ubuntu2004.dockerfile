FROM ubuntu:20.04

USER root

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
        && apt-get install -y --no-install-recommends \
        apt-utils sudo xauth gnupg ca-certificates lsb-release curl wget \
        python3-pip python3-venv python3-virtualenv \
        silversearcher-ag cscope markdown pandoc w3m texinfo perl-doc \
        iproute2 inetutils-ping net-tools socat dnsutils \
        gdb gdbserver openssh-server git patch \
        emacs vim rxvt-unicode tmux nnn sbcl \
        && apt-get autoremove \
        && apt-get clean

RUN mkdir -p /etc/apt/keyrings
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg
RUN echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" \
        | sudo tee /etc/apt/sources.list.d/docker.list
RUN apt-get update \
        && apt-get install -y --no-install-recommends \
        docker-ce docker-ce-cli containerd.io docker-compose-plugin \
        && apt-get autoremove \
        && apt-get clean

ARG USER_NAME=zach
ARG USER_HOME=/home/$USER_NAME
ARG USER_UID=1000
ARG USER_GID=1000
ARG USER_PASSWD=123456
RUN groupadd -g $USER_GID $USER_NAME
RUN useradd -d $USER_HOME -s /bin/bash -m $USER_NAME -u $USER_UID -g $USER_GID \
        && echo $USER_NAME:$USER_PASSWD | chpasswd \
        && adduser $USER_NAME sudo
RUN echo "$USER_NAME ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/$USER_NAME

ARG ROOT_PASSWD=$USER_PASSWD
RUN echo root:$ROOT_PASSWD | chpasswd

RUN mkdir -p /app
RUN mkdir -p /var/run/sshd

RUN sed -i \
        -e 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' \
        -e 's/#PermitRootLogin yes/PermitRootLogin yes/g' \
        -e 's/#AllowTcpForwarding yes/AllowTcpForwarding yes/g' \
        -e 's/#X11Forwarding yes/X11Forwarding yes/g' \
        -e 's/#X11DisplayOffset 10/X11DisplayOffset 10/g' \
        -e 's/#X11UseLocalhost yes/X11UseLocalhost no/g' \
        /etc/ssh/sshd_config

ARG DOCKER_GID=133
RUN groupmod -g $DOCKER_GID docker
RUN usermod -aG docker $USER_NAME

WORKDIR $USER_HOME
USER $USER_NAME
ENV HOME $USER_HOME
ENV USER $USER_NAME
RUN touch $HOME/.Xauthority

ENV PATH $PATH:$HOME/.local/bin
COPY requirements.txt /app
RUN pip3 install --requirement /app/requirements.txt

COPY entrypoint.sh /app
ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["init"]
