SR-IOV
======

## Using SR-IOV
    https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/5/html/virtualization/sect-para-virtualized_windows_drivers_guide-how_sr_iov_libvirt_works
    This section covers attaching Virtual Function to a guest as an additional network device.
    SR-IOV requires Intel VT-d support.

    >Important
    >Xen requires additional kernel arguments to use SR-IOV.
    >Modify the /boot/grub/grub.conf file to enable SR-IOV.
    >To enable SR-IOV with Xen for Intel systems append the pci_pt_e820_access=on parameter to the kernel.

## Enable Intel VT-d in BIOS and in the kernel
    Enable Intel VT-D in BIOS, see your system manufacturer's documentation for specific instructions.

## Verify support
    Verify if the PCI device with SR-IOV capabilities are detected.
    This example lists an Intel 82576 network interface card which supports SR-IOV.
    Use the lspci command to verify if the device was detected.

    # lspci
    03:00.0 Ethernet controller: Intel Corporation 82576 Gigabit Network Connection (rev 01)
    03:00.1 Ethernet controller: Intel Corporation 82576 Gigabit Network Connection (rev 01)

## Start the SR-IOV kernel modules

    # modprobe igb [<option>=<VAL1>,<VAL2>,]
    # lsmod |grep igb
    igb    87592  0
    dca    6708    1 igb

## Activate Virtual Functions
    For this particular card the valid range is 0 to 7

    # modprobe -r igb
    # modprobe igb max_vfs=1

## Inspect the new Virtual Functions

    # lspci | grep 82576
    03:00.0 Ethernet controller: Intel Corporation 82576 Gigabit Network Connection (rev 01)
    03:00.1 Ethernet controller: Intel Corporation 82576 Gigabit Network Connection (rev 01)
    03:10.0 Ethernet controller: Intel Corporation 82576 Virtual Function (rev 01)
    03:10.1 Ethernet controller: Intel Corporation 82576 Virtual Function (rev 01)


    # lspci -n | grep 03:00.0
    03:00.0 0200: 8086:10c9 (rev 01)
    # lspci -n | grep 03:10.0
    03:10.0 0200: 8086:10ca (rev 01)

    The Physical Function corresponds to 8086:10c9 and the Virtual Function to 8086:10ca.

## Find the devices with virsh

    # virsh nodedev-list | grep 8086
    pci_8086_10c9
    pci_8086_10c9_0
    pci_8086_10ca
    pci_8086_10ca_0
    [output truncated]

## Get advanced details

    # virsh nodedev-dumpxml pci_8086_10ca
    # virsh nodedev-dumpxml pci_8086_10ca_0
    <device>
      <name>pci_8086_10ca_0</name>
      <parent>pci_8086_3408</parent>
      <driver>
        <name>igbvf</name>
      </driver>
      <capability type='pci'>
        <domain>0</domain>
        <bus>3</bus>
        <slot>16</slot>
        <function>1</function>
        <product id='0x10ca'>82576 Virtual Function</product>
        <vendor id='0x8086'>Intel Corporation</vendor>
      </capability>
    </device>

## Add the Virtual Function to the guest
### Shut down the guest
### Use the output from the virsh nodedev-dumpxml pci_8086_10ca_0 command to calculate the values for the configuration file

    $ printf %x 3
    3
    $ printf %x 16
    10
    $ printf %x 1
    1

    This example would use the following values in the configuration file:
    bus='0x03'
    slot='0x10'
    function='0x01'

### Open the XML configuration file with the virsh edit command

    # virsh edit MyGuest
    <hostdev mode='subsystem' type='pci' managed='yes'>
      <source>
        <address bus='0x03' slot='0x10' function='0x01'/>
      </source>
    </hostdev>

### Save the configuration

## Restart

    # virsh start MyGuest

## Error starting the guest

    # virsh start test
    error: Failed to start domain test
    error: internal error unable to start guest: char device redirected to
    /dev/pts/2
    get_real_device: /sys/bus/pci/devices/0000:03:10.0/config: Permission denied
    init_assigned_device: Error: Couldn't get real device (03:10.0)!
    Failed to initialize assigned device host=03:10.0

    This error is often caused by a device which is already assigned to another guest or to the host itself
