FROM ubuntu:22.04

USER root

RUN apt-get update \
        && apt-get install -y apt-utils libterm-readkey-perl \
        && apt-get install -y silversearcher-ag iproute2 cscope \
        && apt-get install -y gdb openssh-server \
        && apt-get install -y emacs sudo perl-doc

ARG user=zach
ARG home=/home/$user
ARG id=1000
ARG group=1000
ARG passwd=123456

RUN groupadd -g $group $user
RUN useradd -d $home -s /bin/bash -m $user -u $id -g $group \
        && echo $user:$passwd | chpasswd \
        && adduser $user sudo

RUN mkdir -p /var/run/sshd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' /etc/ssh/sshd_config
RUN sed -i 's/#PermitRootLogin yes/PermitRootLogin yes/g' /etc/ssh/sshd_config

WORKDIR $home
USER $user
ENV HOME $home

RUN touch ~/.sudo_as_admin_successful

CMD ["/usr/sbin/sshd", "-D"]
