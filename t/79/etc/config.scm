;; 20210820 (c) Gunter Liszewski -- (details for a new machine: this-04, or S10)

;; "desktop" setup with GNOME and Xfce 

(use-modules (gnu) (gnu system nss))
(use-service-modules desktop xorg virtualization)
(use-package-modules certs gnome
		     emacs emacs-xyz ratpoison suckless wm
		     xorg virtualization)

(operating-system
  (host-name "S10")
  (timezone "Europe/Belfast")
  (locale "en_GB.utf8")

  (keyboard-layout (keyboard-layout "gb" "altgr-intl"))

  ;; GRUB with the EFI System, but here is the thing: this is a budged PC
  ;; \ddanger We want to accomodate coexistance, (we archived the original)
  
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (target "/boot/efi")
                (keyboard-layout keyboard-layout)))

  ;; Accomodate the SSD drive
  (initrd-modules (append (list "mmc_block" "sdhci_pci")
			  %base-initrd-modules))
  
  (file-systems (append
                 (list (file-system
                         (device (file-system-label "S10"))
                         (mount-point "/")
                         (type "btrfs"))
                       (file-system
                         (device (uuid "E4F1-45BB" 'fat))
                         (mount-point "/boot/efi")
                         (type "vfat")))
                 %base-file-systems))

  ;; Create this user
  (users (cons (user-account
                (name "gunter")
                (comment "And another")
                (password (crypt "password" "$6$abc"))
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video")))
               %base-user-accounts))

  ;; System-wide packages
  (packages (append (list
                     ratpoison i3-wm i3status dmenu
                     emacs emacs-exwm emacs-desktop-environment
                     xterm
                     ;; for HTTPS access
                     nss-certs
                     ;; for user mounts
                     gvfs qemu)
                    %base-packages))

  ;; Add GNOME and Xfce---we can choose at the log-in screen
  ;; by clicking the gear.  Use the "desktop" services, which
  ;; include the X11 log-in service, networking with
  ;; NetworkManager, and more.
  (services (append (list (service gnome-desktop-service-type)
                          (service xfce-desktop-service-type)
			  (service hurd-vm-service-type
			   (hurd-vm-configuration
			    ;; (id 0)
			    (image "/srv/Ha-1/store/hurd-vm-disk-image.img")
			    (options '())
			    (disk-size (* 5000 (expt 2 20)))
			    (memory-size 1024)))
			  (service hurd-vm-service-type
			   (hurd-vm-configuration
			    (id 1)
			    ;; (image "/srv/Ha-1/store/Ha-1.img")
			    ;; (options '())
			    (disk-size (* 5000 (expt 2 20)))
			    (memory-size 1024)))
                          (set-xorg-configuration
                           (xorg-configuration
                            (keyboard-layout keyboard-layout))))
                    %desktop-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))