Sriov Network Device Plugin
===========================

## URLs
    https://github.com/k8snetworkplumbingwg/sriov-network-device-plugin

## Creating VFs with sysfs

    # echo 8 > /sys/class/net/${PF_NAME}/device/sriov_numvfs
    # lspci | grep "Virtual Function"

    Placing the above command in a script that is run on startup such as /etc/rc.local

## Common issues

    ## write error: Cannot allocate memory
    # modprobe -r ixgbe; modprobe ixgbe max_vfs=8
    # echo 8 > /sys/class/net/${PF_NAME}/device/sriov_numvfs

    ## Device or resource busy
    # echo 0 > /sys/class/net/${PF_NAME}/device/sriov_numvfs
    # echo 8 > /sys/class/net/${PF_NAME}/device/sriov_numvfs
