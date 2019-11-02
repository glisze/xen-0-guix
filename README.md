* Virtualisation and GNU Guix -*- mode: org; -*-
 Experimental: virtualising with  Xen on GNU Guix
 
 This is for x86_64. It requires familiarity with GNU Guix, and virtualisation.

* What have we here
 Here, this is the list of packages for GNU Guix that are at an early stage and
that could be looked at like this:
: guix environment --load-path=./here xen-0-tools
: guix environment --load-path=./here xen-0-docs
: guix environment --load-path=./here xen-0-boot
: guix environment --load-path=./here xen-0-qemu
: guix environment --load-path=./here grub-hybrid-xen

~xen-0-boot~ builds the hypervisor to start a Xen Dom0. A snippet for
a ~grib.cfg~ might look like this:
: multiboot2 /gnu/store/...-xen-0-boot-.../boot/xen.4.12.1.gz
: module2 /gnu/store/...-linux-libre-.../bzImage ...
: module2 /gnu/store/.../initrd.cpio.gz

~grub-hybrid-xen~ can make a bootloader that can be given as the ~kernel~ to create
and boot a DomU machine with a ~--full-boot~ image.

~xen-0-tools~ can start the Xen daemons, and gives the tools to work with the
virtual environment.
