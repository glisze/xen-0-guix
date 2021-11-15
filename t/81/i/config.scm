;; 20211107 (c) Gunter Liszewski -*- mode: scheme; -*-
;; (S10: linux-for-ak3v, linux-firmware-for-ak3v) 

(use-modules (gnu) (guix) (guix gexp) (gnu system nss)
	     (gnu system keyboard)(srfi srfi-1))

(use-service-modules desktop xorg virtualization ssh
		     nfs)

(use-package-modules certs gnome audio
		     xen-0-linux busybox nfs
		     version-control ssh ed
		     gnupg
		     emacs emacs-xyz ratpoison suckless wm
		     xorg virtualization linux)
(operating-system
  (host-name "S10")
  (timezone "Europe/Belfast")
  (locale "en_GB.utf8")
  (keyboard-layout (keyboard-layout "gb"))
  (label (string-titlecase
	  (string-append
	   "GNU Guix: S10 ak3v: with "
	   (package-name (operating-system-kernel this-operating-system)) " "
	   (package-version (operating-system-kernel this-operating-system)))))
  (kernel linux-for-ak3v)
  ;; "modprobe.blacklist=iwlwifi"
  (kernel-arguments (append (list "iommu_intel=on"
				  "vfio.ids=8086:095a,10ec:8168")
			    %default-kernel-arguments))
  (firmware `(,linux-firmware-for-ak3v))
  #;(initrd-modules (append (list "mmc_block" "sdhci_pci")
  %base-initrd-modules))
  (initrd-modules
    (list ;; "mmc_block" "sdhci_pci"
	  "ahci" "usb-storage" "uas" ;; "usbhid"
	  ;; "hid-generic"
	  "hid-apple"
	  "dm-crypt" "xts" "serpent_generic" "wp512" "nls_iso8859-1"
	  "pata_acpi" "pata_atiixp" "isci"
	  ;; "i915"
	  ;; "r8169"
	  ;; "iwlwifi"
	  ))
  (initrd (lambda (a . b)
	    (apply
	     raw-initrd a
	     #:linux-modules
	     (list ;; "mmc_block" "sdhci_pci"
		   "ahci" "usb-storage" "uas" ;; "usbhid"
		   ;; "hid-generic"
		   "hid-apple"
		   "dm-crypt" "xts" "serpent_generic" "wp512" "nls_iso8859-1"
		   "pata_acpi" "pata_atiixp" "isci"
		   ;; "i915"
		   ;;"r8169"
		   ;; "iwlwifi"
		   )
	     ;; virtio_pci virtio_balloon virtio_blk virtio_net
	     ;; virtio_console virtio-rng
	     #:helper-packages (list
				busybox
				linux-firmware-for-ak3v
				lvm2-static
				btrfs-progs/static
				loadkeys-static)
	     #:qemu-networking? #f
	     b)))
;;;  (bootloader (bootloader-configuration
;;;	       (bootloader grub-bootloader)
;;;	       (targets '("/dev/sda"))
;;;	       (keyboard-layout keyboard-layout)
  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efib" "/boot/efi1"))
    (keyboard-layout keyboard-layout)
    (timeout 3)
    (default-entry 0)
    (menu-entries
     (list (menu-entry
	    (label "GNU Guix: S10: linux-for-ak3v@5.15.0-rc7 (t/78)")
	    (linux "/@/0@LINUX@")
	    (linux-arguments (list
			      "--root=@ROOT@"
			      "--system=@SYSTEM@"
			      "--load=@BOOT@"
			      "iommu_intel=on"
			      "vfio.ids=8086:095a,10ec:8168"
			      "quiet"))
	    (initrd "/@/0@INITRD@"))
	   (menu-entry
	    (label "GNU Guix: S10: linux-for-ak3v@5.15.0-rc7 (rdinit,t/78)")
	    (linux "/@/0@LINUX@")
	    (linux-arguments (list
			      "rdint=@RDINIT@"
			      "--root=@ROOT@"
			      "--system=@SYSTEM@"
			      "--load=@BOOT@"
			      "iommu_intel=on"
			      "vfio.ids=8086:095a,10ec:8168"
			      "quiet"))
	    (initrd "/@/0@INITRD@"))
	   (menu-entry
	    (label "The Other,still to be confirmed")
	    (linux "Linux")
	    (linux-arguments (list
			      "root=/dev/sda2"
			      "console=ttyS13"))
	    (initrd "boot/initrd.img"))
	   (menu-entry
	    (label "(operating-system-full-name or so)")
	    (linux "(operating-system-kernel-name)")
	    (linux-arguments (list
			      "root=10.128.192.51:/srv/S2"
			      "quiet"))
	    (initrd "boot/nothere.img"))))))
  (mapped-devices
   (list
    (mapped-device
     (source "HyperVG11")
     (targets (list "HyperVG11-S2" "HyperVG11-SRC1" "HyperVG11-BUILD1" "HyperVG11-media--6"))
     (type lvm-device-mapping))))
  (file-systems (append
		 (list
		  (file-system
		   (device (file-system-label "S10"))
		   (mount-point "/")
		   (type "btrfs"))
		  (file-system
		   (device (uuid "E4F1-45BB" 'fat))
		   (mount-point "/boot/efi")
		   (type "vfat"))
		  (file-system
		   (device (uuid "0FCB-D531" 'fat))
		   (mount-point "/boot/efib")
		   (type "vfat"))
		  (file-system
		   (device (uuid "DEA7-9178" 'fat))
		   (mount-point "/boot/efi1")
		   (type "vfat"))
		  ;; review
		  (file-system
		   (device "/dev/mapper/HyperVG11-S2")
		   (mount-point "/home/S2")
		   (type "btrfs")
		   (options "subvol=@/0")
		   (dependencies mapped-devices))
		  #;(file-system
		  (device (uuid "DEA7-9178" 'fat))
		  (mount-point "/boot/efi")
		  (type "vfat"))
		  #;(file-system
		  (device "/dev/mmcblk0p5")
		  (mount-point "/home/S10")
		  (type "btrfs")
		  (create-mount-point? #t)
		  (mount? #t))
		;; ssk
		  (file-system
		     (device (file-system-label "store-b"))
		     (mount-point "/home/source-b")
		     (type "btrfs")
		     (options "subvol=@/b")
		     (create-mount-point? #t)
		     (mount? #t))
		  (file-system
		     (device (file-system-label "store-b"))
		     (mount-point "/home/data-b")
		     (type "btrfs")
		     (options "subvol=@/0")
		     (create-mount-point? #t)
		     (mount? #t))
		  (file-system
		     (device (file-system-label "store-b"))
		     (mount-point "/home/root-b")
		     (type "btrfs")
		     (options "subvol=/")
		     (create-mount-point? #t)
		     (mount? #f))
		  #;(file-system
		     (device (file-system-label "EFIB"))
		     (mount-point "/home/EFIB")
		     (type "vfat")
		     (options "defaults")
		     (create-mount-point? #t)
		     (mount? #f))
		  ;; mmc2
		  (file-system
		   (device "/dev/mmcblk2p1")
		   (mount-point "/home/RASPIFIRM")
		   (type "vfat")
		   (create-mount-point? #t)
		   (mount? #f))
		  (file-system
		   (device "/dev/mmcblk2p2")
		   (mount-point "/home/RASPIROOT")
		   (type "ext4")
		   (create-mount-point? #t)
		   (mount? #f))
		  (file-system
		   (device "/dev/mmcblk2p3")
		   (mount-point "/home/CORE64")
		   (type "ext2")
		   (create-mount-point? #t)
		   (mount? #f))
		  ;; Constants
		  (file-system
		   (device "/dev/mapper/HyperVG11-SRC1")
		   (mount-point "/home/source-1")
		   (type "btrfs")
		   (create-mount-point? #t)
		   (mount? #t)
		   (dependencies mapped-devices))
		  (file-system
		   (device "/dev/mapper/HyperVG11-BUILD1")
		   (mount-point "/home/build-1")
		   (type "btrfs")
		   (create-mount-point? #t)
		   (mount? #t)
		   (dependencies mapped-devices))
		  (file-system
		   (device "/dev/mapper/HyperVG11-media--6")
		   (mount-point "/home/media-1")
		   (type "btrfs")
		   (create-mount-point? #t)
		   (mount? #t)
		   (dependencies mapped-devices)))
		 %base-file-systems))
  ;; Create this user
  (users (append (list
		  (user-account
		   (name "gunter")
		   (comment "But yet")
		   (password (crypt "password" "$6$abc"))
		   (group "users")
		   (supplementary-groups '("wheel" "netdev"
					"audio" "video" "lp")))
		  (user-account
		   (name "anonymous")
		   (uid 1037)
		   (comment "Not now")
		   (group "anonymous")
		   (supplementary-groups '())
		   (system? #t)))
		  %base-user-accounts))
  (groups (append (list (user-group
			 (name "anonymous")
			 (id 1037)))
			%base-groups))
  (hosts-file (plain-file "hosts" "
127.0.0.1     localhost S2
::1           localhost S2
10.128.192.1  localhost S2
10.128.192.51 pi02.wired
10.128.192.50 airnine
192.168.43.49 gw
192.168.43.51 pi02.wireless pi02
192.168.43.11 pixma
192.168.43.10 ipad-03
192.168.43.6  ipad
192.168.43.3  pixi-4
185.157.233.143 m2"))
  #;(use-modules (guix gexp)(gnu packages xen-0-linux)(gnu packages busybox))
  ;; this does not belong here, but is kept as a note for to be done elsewhere
  #;(let ((this-system-tbc "to-be-confirmed/"))
    (mixed-text-file
     "grub-test-t72.cfg"
     "linux "   linux-for-ak3v "bzImage "
     "rdinit="  busybox "bin/sh "
     "--root=" (this-system-tbc root) " "
     "--system=" (this-system-tbc) " "
     "--load=" (this-system-tbc) "boot "
     "quiet" "\n"))
  ;; System-wide packages
  (packages (append
	     (list
	      ratpoison i3-wm i3status dmenu
	      emacs emacs-exwm emacs-desktop-environment
	      xterm
	      ;; for HTTPS access
	      nss-certs
	      ;; for user mounts
	      ;; gvfs
	      alsa-utils
	      ;; Bluetooth things
	      gnome-bluetooth bluez-alsa bluez
	      ;; connectivity and such like
	      git ;; git:gui XXX:
	      openssh-sans-x ed
	      gnupg pinentry-tty
	      qemu)
	     %base-packages))

;; Add GNOME and Xfce---we can choose at the log-in screen
    ;; by clicking the gear.  Use the "desktop" services, which
    ;; include the X11 log-in service, networking with
    ;; NetworkManager, and more.
    (services (append
	       (list
		(service gnome-desktop-service-type)
		(service xfce-desktop-service-type)
		(service bluetooth-service-type)
		(service qemu-binfmt-service-type
			 (qemu-binfmt-configuration
			  (qemu qemu)
			  (platforms
			   (lookup-qemu-platforms "aarch64" "arm"))))
		(set-xorg-configuration
		 (xorg-configuration
		  (keyboard-layout keyboard-layout)))

(service nfs-service-type
 (nfs-configuration
  (nfs-versions '("4.2" "4.1" "4.0" "3" "2"))
  (exports '(("/srv/export/pub"
	      "*(rw,insecure,no_subtree_check,all_squash,anonuid=1037,anongid=1037)")))
  (nfsd-threads 8)
  (nfsd-tcp? #t)
  (nfsd-udp? #t)
  #;(debug '(nfsd mountd))))

(service hurd-vm-service-type
			  (hurd-vm-configuration
			   (id 1)
			   ;; (image "/srv/Ha-1/store/Ha-1.img")
			   ;; (options '())
			   (disk-size (* 5000 (expt 2 20)))
			   (memory-size 1024))))
                 %desktop-services))
;; Allow resolution of '.local' host names with mDNS.
(name-service-switch %mdns-host-lookup-nss))
