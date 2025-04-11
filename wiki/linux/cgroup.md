Cgroup
======

## Install Cgroup on Ubuntu

```
$ sudo apt install cgroup-tools
```

## Cgroups v2 - limit memory and CPU usage for all users on Linux

```
##This is an example of setting a memory limit of 30GB and CPU usage equivalent to 8 x 100% loaded CPUs
$ sudo mkdir -p /etc/systemd/system/user-.slice.d

$ sudo cat /etc/systemd/system/user-.slice.d/50-memory.conf
[Slice]
MemoryMax=30G
CPUQuota=800%

$ sudo systemctl daemon-reload

## NOTE: Use values > 100% for allotting CPU time on more than one CPU.
## Limits will be applied immediately.

## Check applied limits. Get any logged-in user UID with "id" command.
$ cat /sys/fs/cgroup/user.slice/user-[UID].slice/memory.max
32212254720

$ cat /sys/fs/cgroup/user.slice/user-[UID].slice/cpu.max
800000 100000


## Useful links: https://github.com/systemd/systemd/commit/5396624506e155c4bc10c0ee65b939600860ab67
## https://www.freedesktop.org/software/systemd/man/latest/systemd.resource-control.html
```
