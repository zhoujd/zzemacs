Linux something
================

1. rpm package

	<http://linux.vbird.org/linux_basic/0520rpm_and_srpm.php>

2. linux cross reference
	
	<http://lxr.oss.org.cn/source/>

3. android cross reference

	<http://androidxref.com/>

4. QEMU

	[QEMU for windows]<http://qemu.weilnetz.de/>

	[QEMU wiki]<http://wiki.qemu.org/Main_Page>

	sudo apt-get install kvm qemu qemu-kvm virt-manager kernel-package linux-source kqemu-source build-essential
	yum install kvm kmod-kvm qemu
	modprobe kvm-intel or modprobe kvm-amd
	/sbin/lsmod | grep kvm
	#yum provides "*/qemu-kvm"
	sudo qemu-img create –f qcow windows.img 8G
	sudo kvm –localtime –cdrom /dev/cdrom -m 512 -boot d win2.img
	sudo kvm –localtime –m 512 –hda windows.img –cdrom winxp.iso –boot d –clock –rtc –no-acpi

4. cdreord for ISO
	sudo apt-get install cdrecord
	cdrecord --scanbus
	cdrecord dev=0,0,0 <file.iso>

