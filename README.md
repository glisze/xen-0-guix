# (c) 2019 Gunter Liszewski -*- mode: org; -*-
* Virtualisation and GNU Guix
 Experimental: virtualisation with  Xen on GNU Guix
 
 This is for x86_64.  It requires familiarity with GNU Guix, and virtualisation.

* What have we here
 Here, this is the list of packages for GNU Guix that are at an early stage and
that could be looked at like this:
: guix environment --load-path=./here --ad-hoc xen-0-tools
: guix environment --load-path=./here --ad-hoc xen-0-docs
: guix environment --load-path=./here --ad-hoc xen-0-boot
: guix environment --load-path=./here --ad-hoc xen-0-qemu
: guix environment --load-path=./here --ad-hoc grub-hybrid-xen

: guix environment --load-path=./here --ad-hoc xen-0-busybox
: guix environment --load-path=./here --ad-hoc linux-for-x501u
: guix environment --load-path=./here --ad-hoc linux-firmware-for-x501u

The GNU Guix package ~xen-0-boot~ provides the hypervisor to start the Xen Dom0.  A snippet for
a ~grub.cfg~ might look like this:
: multiboot2 /gnu/store/...-xen-0-boot-.../boot/xen.4.12.1.gz
: module2 /gnu/store/...-linux-libre-.../bzImage ...
: module2 /gnu/store/.../initrd.cpio.gz

The tool that the ~grub-hybrid-xen~ package provides can make a bootloader which may be used as the ~kernel~
for a Xen DomU and start a machine that is instantiated as a GNU Guix ~--full-boot~ VM image.  (Other
disk images may also be used to create such a virtual machine.)

Package ~xen-0-tools~ gives the tools to start the Xen daemons, and to work with the domains of 
the virtual environment.

Here is a shell function that may start the necessary daemons.
: d___ () 
: { 
:    local D="xen-0-tools: init.d/xencommons [start|stop|...]";
:    local a=$( dirname $( dirname $( readlink -e $( which xl ))));
:    sudo $a/etc/init.d/xencommons ${1:-start}
: }

