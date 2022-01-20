VirtualBox
==========

## VirtualBox	Command

    ## https://www.virtualbox.org/manual/ch08.html#vboxmanage-general
    ## List All VMs
    $ vboxmanage list vms
    ## Starting a VM in the background
    $ vboxmanage startvm {VM NAME} --type headless
    ## List Running VMs
    $ vboxmanage list runningvms
    ## Getting the VM's IP address
    $ vboxmanage guestproperty get {VM NAME} "/VirtualBox/GuestInfo/Net/0/V4/IP"
    ## Add Port Forwarding (e.g. SSH)
    $ vboxmanage modifyvm {VM NAME} --natpf1 "guestssh,tcp,,22111,,22"
