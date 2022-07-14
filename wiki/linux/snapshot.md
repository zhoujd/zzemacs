snapshot
========

## Before proceeding make sure LVM Volume Group (VG) has enough space for the snapshot.
```
# vgs
VG  #PV  #LV  #SN   Attr    VSize   VFree
ol   2    2    0    wz--n-  12.00g  1.01g
```

## In this example we are creating a root snapshot of 512M. Here "snap" is the snapshot name of a root volume.
```
# lvcreate -L 512M -s -n snap /dev/ol/root
Logical volume "snap" created.
```

## Verify the snapshot created.
```
# lvs
LV    VG   Attr       LSize Pool   Origin  Data% Meta% Move Log Cpy%Sync Convert
root  ol   owi-aos--- 9.79g
snap  ol   swi-a-s--- 512.00m       root   0.01
swap  ol   -wi-ao---- <1.20g
```

## Verify the snapshot functioning.
```
# fallocate -l 100m test.img
# lvs
LV    VG  Attr       LSize Pool  Origin  Data% Meta% Move Log Cpy%Sync Convert
root  ol  owi-aos--- 9.79g
snap  ol  swi-a-s--- 512.00m      root   0.03
swap  ol  -wi-ao---- <1.20g
# ls
bin boot dev etc home lib lib64 media mnt opt proc root run sbin srv sys test.img tmp usr var
```
Note that the snapshot volume is larger.

## Merge the snapshot. When the merge finishes, the merged snapshot will be removed.
```
# lvconvert --merge /dev/ol/snap
Delaying merge since origin is open.
Merging of snapshot ol/snap will occur on next activation of ol/root.
```

## Map Logical value (LV) to Physical volume (PV):
```
# lvs -ao +devices
LV     VG   Attr       LSize Pool  Origin Data% Meta% Move Log Cpy%Sync Convert Devices
root   ol   Owi-aos--- 9.79g                                                   /dev/sda2(307)
[snap] ol   Swi-a-s--- 512.00m      root   0.05                                /dev/sdb(0)
swap   ol   -wi-ao---                                                          /dev/sda2(0)
```

## The snapshot should merge during boot and the boot should complete successfully.
```
# reboot
# ls
bin boot dev etc home lib lib64 media mnt opt proc root run sbin srv sys tmp usr var
```
