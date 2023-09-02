FROM ubuntu:22.04

USER root

RUN apt-get update \
        && apt-get install -y apt-utils sudo libterm-readkey-perl \
        && apt-get install -y silversearcher-ag iproute2 cscope \
        && apt-get install -y gdb openssh-server \
        && apt-get install -y git docker.io \
        && apt-get install -y emacs perl-doc \
        && apt-get autoclean

## Setup User
ARG USER=zach
ARG HOME=/home/$USER
ARG UID=1000
ARG GID=1000
ARG PASSWD=123456

RUN groupadd -g $GID $USER
RUN useradd -d $HOME -s /bin/bash -m $USER -u $UID -g $GID \
        && echo $USER:$PASSWD | chpasswd \
        && adduser $USER sudo

## Setup SSH
RUN mkdir -p /var/run/sshd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' /etc/ssh/sshd_config
RUN sed -i 's/#PermitRootLogin yes/PermitRootLogin yes/g' /etc/ssh/sshd_config

## Setup Docker
RUN usermod -aG docker $USER

## Setup Run
WORKDIR $HOME
USER $USER
ENV HOME $HOME

RUN touch ~/.sudo_as_admin_successful

CMD ["/usr/sbin/sshd", "-D"]
