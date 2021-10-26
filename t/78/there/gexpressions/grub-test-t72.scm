(use-modules (guix gexp)(gnu packages xen-0-linux)(gnu packages busybox))
(let ((this-system-tbc "this-operating-system/"))
   (mixed-text-file
        "grub-test-t72.cfg"
        "linux "   linux-for-ak3v "bzImage "
        "rdinit="  busybox "bin/sh "
        "--root=" "this-system-tbc root "
        "--system=" this-system-tbc " "
        "--load=" this-system-tbc "boot "
        "quiet" "\n"))
