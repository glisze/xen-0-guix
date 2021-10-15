;;; xen-0-linux (C) 2019 Gunter Liszewski
;;;  guix build --load-path=here linux-for-ak3v-defconfig (20211015)

;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2017 Andy Wingo <wingo@igalia.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages xen-0-linux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system trivial)
  ;; #:use-module (guix build-system gnu)  
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)  
  #:use-module (guix download))

(define-public linux-machine-base
  (let* ((version "v5.15-rc5"))
    (package
     (inherit linux-libre)
     (name "linux-machine-base")
     (version version)
     (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux-stable.git")
		    (commit version)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"0b7ym072s318r70006c7z052y9jiqv5m1mf4ksljiqqw0xgxl3r0"))))
     (synopsis "Linux kernel that permits non-free things.")
     (description "A base for a machine specific kernel.")
     (license license:gpl2)
     (home-page "http://kernel.org/"))))

(define-public linux-for-x501u
  (let* ((machine "x501u")
	 (version "v5.15-rc5"))
    (package
     (inherit linux-machine-base)
     (name "linux-for-x501u")
     (arguments
      (substitute-keyword-arguments (package-arguments linux-libre)
	((#:phases phases '%standard-phases)
	 `(modify-phases ,phases
	    (replace 'configure
	      (lambda* (#:key inputs native-inputs target #:allow-other-keys)
		(setenv "KCONFIG_NOTIMESTAMP" "1")
		(setenv "KBUILD_BUILD_TIMESTAMP" (getenv "SOURCE_DATE_EPOCH"))
		(for-each (lambda (a)
			    ;; Mung our own include/ out of our environment
			    (setenv a
				    (string-join
				     (cdr (string-split (or (getenv a) "") #\:))
				     ":")))
			  '("CPATH" "CPLUS_INCLUDE_PATH" "C_INCLUDE_PATH"))
		(let ((build  (assoc-ref %standard-phases 'build))
		      (config (assoc-ref inputs "Kconfig")))
		  (invoke "make" "mrproper")
		  (if config
		      (begin
			(copy-file config ".config")
			(chmod ".config" #o666)
			(invoke "make" "olddefconfig")))
		  #t)))
	    (replace 'install
	      (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
		(let* ((out    (assoc-ref outputs "out"))
		       (moddir (string-append out "/lib/modules"))
		       (dtbdir (string-append out "/lib/dtbs"))
		       (kmod   (assoc-ref (or native-inputs inputs) "kmod")))
		  (for-each (lambda (a) (install-file a out))
			    (find-files "." "^(\\.config|bzImage|zImage|Image|vmlinuz|System\\.map|Module\\.symvers)$"))
		  (unless (null? (find-files "." "\\.dtb$"))
		    (mkdir-p dtbdir)
		    (invoke "make" (string-append "INSTALL_DTBS_PATH=" dtbdir)
			    "dtbs_install"))
		  (mkdir-p moddir)
		  #;(invoke "make"
			  (string-append "INSTALL_PATH=" out)
			  (string-append "INSTALL_HDR_PATH=" out)
			  "headers_install")
		  (invoke "make"
			  (string-append "DEPMOD=" kmod "/bin/depmod")
			  (string-append "MODULE_DIR=" moddir)
			  (string-append "INSTALL_PATH=" out)
			  (string-append "INSTALL_MOD_PATH=" out)
			  "INSTALL_MOD_STRIP=1"
			  "modules_install"))))))))
     (inputs
      `(("Kconfig"
	 ,(search-auxiliary-file "linux-0/x501u.5.15-rc5.config")
	,@(package-inputs linux-libre))))
     (synopsis "Linux for a x501u machine")
     (description "Linux with non-free things for one particular machine model."))))

(define-public linux-for-ak3v
  (let* ((machine "ak3v")
	 (version "v5.15-rc5"))
    (package
     (inherit linux-machine-base)
     (name "linux-for-ak3v")
     (arguments
      (substitute-keyword-arguments (package-arguments linux-libre)
	((#:phases phases '%standard-phases)
	 `(modify-phases ,phases
	    (replace 'configure
	      (lambda* (#:key inputs native-inputs target #:allow-other-keys)
		(setenv "KCONFIG_NOTIMESTAMP" "1")
		(setenv "KBUILD_BUILD_TIMESTAMP" (getenv "SOURCE_DATE_EPOCH"))
		(for-each (lambda (a)
			    ;; Mung our own include/ out of our environment
			    (setenv a
				    (string-join
				     (cdr (string-split (or (getenv a) "") #\:))
				     ":")))
			  '("CPATH" "CPLUS_INCLUDE_PATH" "C_INCLUDE_PATH"))
		(let ((build  (assoc-ref %standard-phases 'build))
		      (config (assoc-ref inputs "Kconfig")))
		  (invoke "make" "mrproper")
		  (if config
		      (begin
			(copy-file config ".config")
			(chmod ".config" #o666)
			(invoke "make" "olddefconfig")))
		  #t)))
	    (replace 'install
	      (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
		(let* ((out    (assoc-ref outputs "out"))
		       (moddir (string-append out "/lib/modules"))
		       (dtbdir (string-append out "/lib/dtbs"))
		       (kmod   (assoc-ref (or native-inputs inputs) "kmod")))
		  (for-each (lambda (a) (install-file a out))
			    (find-files "." "^(\\.config|bzImage|zImage|Image|vmlinuz|System\\.map|Module\\.symvers)$"))
		  (unless (null? (find-files "." "\\.dtb$"))
		    (mkdir-p dtbdir)
		    (invoke "make" (string-append "INSTALL_DTBS_PATH=" dtbdir)
			    "dtbs_install"))
		  (mkdir-p moddir)
		  #;(invoke "make"
			  (string-append "INSTALL_PATH=" out)
			  (string-append "INSTALL_HDR_PATH=" out)
			  "headers_install")
		  (invoke "make"
			  (string-append "DEPMOD=" kmod "/bin/depmod")
			  (string-append "MODULE_DIR=" moddir)
			  (string-append "INSTALL_PATH=" out)
			  (string-append "INSTALL_MOD_PATH=" out)
			  "INSTALL_MOD_STRIP=1"
			  "modules_install"))))))))
     (native-inputs
      `(("kmod" ,kmod)
        ("cpio" ,cpio)
	,@(package-native-inputs linux-libre)))
     (inputs
      `(("Kconfig" ,(local-file "ak3v.5.15-rc5.config"))
	 #;,(search-auxiliary-file "linux-0/ak3v.5.15-rc5.config")
	,@(package-inputs linux-libre)))
     (synopsis "Linux for an ak3v machine")
     (description "Linux with non-free things for one particular machine model."))))

(define-public linux-for-ak3v-defconfig
  (let* ((machine "ak3v")
	 (version "v5.15-rc5"))
    (package
     (inherit linux-machine-base)
     (name "linux-for-ak3v-defconfig")
     (arguments
      (substitute-keyword-arguments (package-arguments linux-libre)
	((#:phases phases '%standard-phases)
	 `(modify-phases ,phases
	    (replace 'configure
	      (lambda* (#:key inputs native-inputs target #:allow-other-keys)
		(setenv "KCONFIG_NOTIMESTAMP" "1")
		(setenv "KBUILD_BUILD_TIMESTAMP" (getenv "SOURCE_DATE_EPOCH"))
		(for-each (lambda (a)
			    ;; Mung our own include/ out of our environment
			    (setenv a
				    (string-join
				     (cdr (string-split (or (getenv a) "") #\:))
				     ":")))
			  '("CPATH" "CPLUS_INCLUDE_PATH" "C_INCLUDE_PATH"))
		(let ((build  (assoc-ref %standard-phases 'build))
		      (config (assoc-ref inputs "Kconfig")))
		  (invoke "make" "mrproper")
		  (if config
		      (begin
			(copy-file config ".config")
			(chmod ".config" #o666)
			(invoke "make" "olddefconfig")))
		  #t)))
	    (replace 'install
	      (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
		(let* ((out    (assoc-ref outputs "out"))
		       (moddir (string-append out "/lib/modules"))
		       (dtbdir (string-append out "/lib/dtbs"))
		       (kmod   (assoc-ref (or native-inputs inputs) "kmod")))
		  (for-each (lambda (a) (install-file a out))
			    (find-files "." "^(\\.config|bzImage|zImage|Image|vmlinuz|System\\.map|Module\\.symvers)$"))
		  (unless (null? (find-files "." "\\.dtb$"))
		    (mkdir-p dtbdir)
		    (invoke "make" (string-append "INSTALL_DTBS_PATH=" dtbdir)
			    "dtbs_install"))
		  (mkdir-p moddir)
		  #;(invoke "make"
			  (string-append "INSTALL_PATH=" out)
			  (string-append "INSTALL_HDR_PATH=" out)
			  "headers_install")
		  (invoke "make"
			  (string-append "DEPMOD=" kmod "/bin/depmod")
			  (string-append "MODULE_DIR=" moddir)
			  (string-append "INSTALL_PATH=" out)
			  (string-append "INSTALL_MOD_PATH=" out)
			  "INSTALL_MOD_STRIP=1"
			  "modules_install"))))))))
     (native-inputs
      `(("kmod" ,kmod)
        ("cpio" ,cpio)
	,@(package-native-inputs linux-libre)))
     (inputs
      `(("Kconfig" ,(local-file "ak3v.5.15-rc5.defconfig"))
	 #;,(search-auxiliary-file "linux-0/ak3v.5.15-rc5.defconfig")
	,@(package-inputs linux-libre)))
     (synopsis "Linux for an ak3v machine")
     (description "Linux with non-free things for one particular machine model."))))


(define-public linux-firmware-for-x501u
  (package
    (name "linux-firmware-for-x501u")
    (version "20191022")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git")
                    (commit version)))
              (sha256
               (base32
                "03ycc55h7vgd4fmb7v7gl7lplf7pg7acs16aa2rramgldxqvyx7j"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((source (assoc-ref %build-inputs "source"))
			  (source-radeon (string-append source "/radeon"))
			  (fw-dir (string-append %output "/lib/firmware"))
			  (fw-dir-radeon (string-append fw-dir "/radeon")))
		     (mkdir-p fw-dir-radeon)
		     (for-each (lambda (file)
				 (copy-file file
					    (string-append fw-dir
							   "/" (basename file))))
			       (map (lambda (a) (string-append source a))
				    '("/WHENCE"
				      "/LICENCE.ralink-firmware.txt" "/rt2870.bin" "/rt2860.bin"
				      "/LICENSE.dib0700" "/dvb-usb-dib0700-1.20.fw"
				      "/LICENSE.radeon")))
		     (for-each (lambda (file)
                                 (copy-file file
                                            (string-append fw-dir-radeon
                                                           "/" (basename file))))
			       (find-files source-radeon))
		     #t))))
    (home-page "rt2800: http://www.mediatek.com/en/downloads1/downloads/
dib0700: tbc, radeon: tbc")
    (synopsis "Non-free firmware")
    (description "Non-free firmware: rt28xx, dib0700, and radeon.
Licence: Redistributable. See LICENCE.ralink-firmware.txt for details.
Licence: Redistributable. See LICENSE.dib0700 for details.
Licence: Redistributable. See LICENSE.radeon for details.
")
    (license (license:non-copyleft "http://git.kernel.org/?p=linux/kernel/git/firmware/linux-firmware.git;a=blob_plain;f=WHENCE;hb=HEAD"))))

(define-public linux-firmware-for-ak3v
  (package
    (name "linux-firmware-for-ak3v")
    (version "20210919")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git")
                    (commit version)))
              (sha256
               (base32
                "1ix43qqpl5kvs6xpqrs3l5aj6vmwcaxcnv8l04mqqkyi9wamjydn"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((source (assoc-ref %build-inputs "source"))
			  (source-i915 (string-append source "/i915"))
			  (source-rtl-nic (string-append source "/rtl_nic"))
			  (source-intel (string-append source "/intel"))
			  (fw-dir (string-append %output "/lib/firmware"))
			  (fw-dir-i915 (string-append fw-dir "/i915"))
			  (fw-dir-rtl-nic (string-append fw-dir "/rtl_nic"))
			  (fw-dir-intel (string-append fw-dir "/intel")))
		     (mkdir-p fw-dir-i915)
		     (mkdir-p fw-dir-rtl-nic)
		     (mkdir-p fw-dir-intel)
		     (for-each (lambda (file)
				 (copy-file file
					    (string-append fw-dir
							   "/" (basename file))))
			       (map (lambda (a) (string-append source a))
				    '("/WHENCE" "/iwlwifi-7265D-29.ucode")))
		     (for-each (lambda (file)
                                 (copy-file file
                                            (string-append fw-dir-i915
                                                           "/" (basename file))))
			       (find-files source-i915))
		     (for-each (lambda (file)
                                 (copy-file file
                                            (string-append fw-dir-rtl-nic
                                                           "/" (basename file))))
			       (find-files source-rtl-nic))
		     (for-each (lambda (file)
                                 (copy-file file
                                            (string-append fw-dir-intel
                                                           "/" (basename file))))
			       (find-files source-intel "ibt-hw-37.8.*"))
		     #t))))
    (home-page "i915: tbc, iwlwifi: tbc, rtl-nic: tbc, intel: tbc")
    (synopsis "Non-free firmware")
    (description "Non-free firmware: i915, iwlwifi, r8169, intel.
Licence: Redistributable. See WHENCE for details.
")
    (license (license:non-copyleft "http://git.kernel.org/?p=linux/kernel/git/firmware/linux-firmware.git;a=blob_plain;f=WHENCE;hb=HEAD"))))

#;(define-public perf-nonfree
  (package
    (inherit perf)
    (name "perf-nonfree")
    (version (package-version linux-nonfree))
    (source (package-source linux-nonfree))
    (license (package-license linux-nonfree))))


