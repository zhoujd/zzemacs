#!/bin/sh

tunctl -t tap0 -u root              # 创建一个tap0接口，只允许root用户访问
brctl addif br0 tap0                # 在虚拟网桥中增加一个tap0接口
ifconfig tap0 0.0.0.0 promisc up    # 打开tap0接口
brctl showstp br0                   # 显示br0的各个接口

