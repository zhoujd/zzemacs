FROM ubuntu:18.04

USER root

RUN apt-get update \
        && apt-get install -y apt-utils sudo libterm-readkey-perl \
        && apt-get install -y silversearcher-ag iproute2 cscope \
        && apt-get install -y gdb openssh-server \
        && apt-get install -y git docker.io vim \
        && apt-get install -y emacs perl-doc \
        && apt-get autoclean

ARG USER=zach
ARG HOME=/home/$USER
ARG UID=1000
ARG GID=1000
ARG PASSWD=123456
RUN groupadd -g $GID $USER
RUN useradd -d $HOME -s /bin/bash -m $USER -u $UID -g $GID \
        && echo $USER:$PASSWD | chpasswd \
        && adduser $USER sudo
RUN echo "$USER ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/$USER

RUN mkdir -p /var/run/sshd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' /etc/ssh/sshd_config
RUN sed -i 's/#PermitRootLogin yes/PermitRootLogin yes/g' /etc/ssh/sshd_config
RUN sed -i 's/#AllowTcpForwarding yes/AllowTcpForwarding yes/g' /etc/ssh/sshd_config
RUN sed -i 's/#X11Forwarding yes/X11Forwarding yes/g' /etc/ssh/sshd_config
RUN sed -i 's/#X11DisplayOffset 10/X11DisplayOffset 10/g' /etc/ssh/sshd_config
RUN sed -i 's/#X11UseLocalhost yes/X11UseLocalhost yes/g' /etc/ssh/sshd_config

ARG DOCKER_GID=133
RUN groupmod -g $DOCKER_GID docker
RUN usermod -aG docker $USER

WORKDIR $HOME
USER $USER
ENV HOME $HOME

CMD ["sudo", "-H", "/usr/sbin/sshd", "-D"]
