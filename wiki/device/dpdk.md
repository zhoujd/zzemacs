DPDK
====

## Poll Mode Driver (PMD)

    ## User Space: e1000, i40e, mlx5 ... nic driver
    ## Kernel Space: uio -> igb_uio

    ## Open("/dev/uioX", O_RDWR) -> VFS -> uio_fops->open -> uio_open -> igbuio_pci_open -> igbuio_pci_enable_interrupts
