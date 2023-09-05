FROM ubuntu:22.04

USER root

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
        && \
        apt-get install -y --no-install-recommends \
        apt-utils sudo libterm-readkey-perl \
        silversearcher-ag cscope markdown pandoc w3m texinfo \
        iproute2 inetutils-ping net-tools socat dnsutils curl \
        gdb gdbserver openssh-server git docker.io vim \
        emacs perl-doc rxvt-unicode tmux nnn \
        && \
        apt-get autoremove \
        && \
        apt-get clean

ARG USER_NAME=zach
ARG USER_HOME=/home/$USER_NAME
ARG USER_ID=1000
ARG USER_GID=1000
ARG USER_PASSWD=123456
RUN groupadd -g $USER_GID $USER_NAME
RUN useradd -d $USER_HOME -s /bin/bash -m $USER_NAME -u $USER_ID -g $USER_GID \
        && echo $USER_NAME:$USER_PASSWD | chpasswd \
        && adduser $USER_NAME sudo
RUN echo "$USER_NAME ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/$USER_NAME

ARG ROOT_PASSWD=123456
RUN echo root:$ROOT_PASSWD | chpasswd

RUN mkdir -p /var/run/sshd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' /etc/ssh/sshd_config
RUN sed -i 's/#PermitRootLogin yes/PermitRootLogin yes/g' /etc/ssh/sshd_config
RUN sed -i 's/#AllowTcpForwarding yes/AllowTcpForwarding yes/g' /etc/ssh/sshd_config
RUN sed -i 's/#X11Forwarding yes/X11Forwarding yes/g' /etc/ssh/sshd_config
RUN sed -i 's/#X11DisplayOffset 10/X11DisplayOffset 10/g' /etc/ssh/sshd_config
RUN sed -i 's/#X11UseLocalhost yes/X11UseLocalhost no/g' /etc/ssh/sshd_config

ARG DOCKER_GID=133
RUN groupmod -g $DOCKER_GID docker
RUN usermod -aG docker $USER_NAME

RUN apt-get install -y --no-install-recommends \
        python3-pip \
        python3-venv \
        python3-virtualenv \
        && \
        apt-get autoremove \
        && \
        apt-get clean
RUN pip3 install virtualenv epc rope jedi flake8 importmagic autopep8 yapf black

WORKDIR $USER_HOME
USER $USER_NAME
ENV HOME $USER_HOME
RUN touch ~/.Xauthority

COPY entrypoint.sh /
ENTRYPOINT ["/entrypoint.sh"]
