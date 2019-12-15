;;; xen-0.scm experiments of virtualisation (c) 2019 Gunter Liszewski
;;; usage example:
;;; guix environment --load-path=./here xen-0-tools

(define-module (gnu packages xen-0)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages figlet)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages selinux)  
  #:use-module (gnu packages spice)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xdisorg)  
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public xen-0-boot
  (package
    (name "xen-0-boot")
    (version "4.12.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://xenbits.xenproject.org/xen.git")
                    (commit (string-append "RELEASE-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
		"1lchpxmqsza6b720mlvglm8fc3j5c8kdhdhn6m7b6vk1cfd6zaz1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-stubdom"
	     "--enable-xen"
	     "--disable-tools"
	     "--disable-docs"
	     "--enable-rpath"
	     "--disable-ocamltools"
	     "--disable-qemu-traditional"
             (string-append "--with-initddir="
                            (assoc-ref %outputs "out")
                            "/etc/init.d"))
       #:make-flags (list "-j" "1"
                          "XEN_BUILD_DATE=Thu Jan  1 01:00:01 CET 1970"
                          "XEN_BUILD_TIME=01:00:01"
                          "XEN_BUILD_HOST="
                          "ETHERBOOT_NICS="
                          "SMBIOS_REL_DATE=01/01/1970"
                          "VGABIOS_REL_DATE=01 Jan 1970"
                          "SYSCONFIG_DIR=/tmp/etc/default"
                          (string-append "BOOT_DIR="
                                         (assoc-ref %outputs "out")
                                         "/boot")
                          (string-append "DEBUG_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/debug")
                          (string-append "EFI_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/efi")
                          "MINIOS_UPSTREAM_URL=")
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'unpack-mini-os
          (lambda* (#:key inputs #:allow-other-keys)
            (copy-recursively (assoc-ref inputs "mini-os") "extras/mini-os")
            #t))
        (add-after 'unpack-mini-os 'patch-minios-rules
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "tools/firmware/Rules.mk"
             (("override XEN_TARGET_ARCH = x86_32")
              (string-append "override XEN_TARGET_ARCH = x86_32
override CC = " (assoc-ref inputs "cross-gcc") "/bin/i686-linux-gnu-gcc"))
             (("^CFLAGS =$")
              (string-append "CFLAGS=-I" (assoc-ref inputs "cross-libc")
                             "/include\n")))
            (substitute* "config/x86_32.mk"
             (("CFLAGS += -m32 -march=i686")
              (string-append "CFLAGS += -march=i686 -I"
                             (assoc-ref inputs "cross-libc")
                             "/include")))
            (substitute* '("tools/Makefile"
                           "tools/xenstore/Makefile"
                           "tools/xenpaging/Makefile"
			   "tools/flask/policy/Makefile.common")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(DUMP|LOG|RUN|LIB|PAGING)_DIR.*")
              "\n")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(RUN|LIB)_STORED.*")
              "\n"))
            (substitute* "tools/examples/Makefile"
             ((" install-readmes") "")
             ((" install-configs") ""))
            (substitute* "tools/pygrub/setup.py"
             (("library_dirs =")
              (string-append "runtime_library_dirs = ['"
                             (assoc-ref outputs "out")
                             "/lib'],\nlibrary_dirs =")))
            #t))
        (add-before 'configure 'patch-xen-script-directory
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* '("configure"
                           "tools/configure"
                           "docs/configure")
             (("XEN_SCRIPT_DIR=.*")
              (string-append "XEN_SCRIPT_DIR="
                             (assoc-ref outputs "out")
                             "/etc/xen/scripts")))
            #t))
        (add-before 'patch-xen-script-directory 'set-environment-up
          (lambda* (#:key make-flags #:allow-other-keys)
             (define (cross? x)
               (string-contains x "cross-i686-linux"))
             (define (filter-environment! filter-predicate
                                          environment-variable-names)
               (for-each
                (lambda (env-name)
                  (let* ((env-value (or (getenv env-name) ""))
                         (search-path (search-path-as-string->list env-value))
                         (new-search-path (filter filter-predicate
                                                  search-path))
                         (new-env-value (list->search-path-as-string
                                         new-search-path ":")))
                    (setenv env-name new-env-value)))
                environment-variable-names))
             (setenv "CROSS_C_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))
             (setenv "CROSS_CPLUS_INCLUDE_PATH" (getenv "CPLUS_INCLUDE_PATH"))
             (setenv "CROSS_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
             (filter-environment! cross?
              '("CROSS_C_INCLUDE_PATH" "CROSS_CPLUS_INCLUDE_PATH"
                "CROSS_LIBRARY_PATH"))
             (filter-environment! (lambda (e) (not (cross? e)))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
             (filter-environment! (lambda (e)
                                    (not
                                     (string-contains e
                                      "mini-os-git-checkout")))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
            (setenv "EFI_VENDOR" "guix")
	    #t))
	(add-after 'configure 'make-xen-silentoldconfig
	   (lambda* (#:key make-flags inputs outputs #:allow-other-keys)
	     (let ((config-file (assoc-ref inputs "xen-config")))
	       (apply invoke "make" "clean" make-flags)
	       (if config-file
		   (begin
		     (copy-file config-file "xen/.config")
		     (apply invoke "make" "-C" "xen" "silentoldconfig" make-flags))))
	       #t))
	(delete 'patch)
        (replace 'build
		 (lambda* (#:key make-flags #:allow-other-keys)
			  (invoke "mkdir" "-p" "tools/include/gnu/")
			  (invoke "touch" "tools/include/gnu/stubs-32.h")
			  (apply invoke "make" "dist-xen" make-flags)))
	(delete 'check)
        (replace 'install
	 (lambda* (#:key make-flags outputs #:allow-other-keys)
	  (let* ((out (assoc-ref outputs "out"))
		 (command_line "test -f ./install.sh && chmod +x ./install.sh && ./install.sh /")
		 (shell "bash"))
	    (invoke shell "-c" command_line))
	    #t)))))
    (inputs
     `(("acpica" ,acpica)
       ("bridge-utils" ,bridge-utils)
       ("checkpolicy" ,checkpolicy)
       ("glib" ,glib)
       ("iproute" ,iproute)
       ("libaio" ,libaio)
       ("libx11" ,libx11)
       ("libyajl" ,libyajl)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("pixman" ,pixman)
       ("util-linux" ,util-linux)
       ("xz" ,xz)
       ("zlib" ,zlib)
       ("xen-config" ,(search-auxiliary-file "xen-0/xen.config"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("dev86" ,dev86)
       ("bison" ,bison)
       ("cmake" ,cmake)
       ("figlet" ,figlet)
       ("flex" ,flex)
       ("gettext" ,gettext-minimal)
       ("libnl" ,libnl)
       ("mini-os"
       ,(origin
         (method git-fetch)
         (uri (git-reference
               (url "http://xenbits.xen.org/git-http/mini-os.git")
               (commit (string-append "xen-RELEASE-" version))))
         (sha256
          (base32
           "1i8pcl19n60i2m9vlg79q3nknpj209c9ic5x10wxaicx45kc107f"))
         (file-name "mini-os-git-checkout")))
       ("ocaml" ,ocaml)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python2)
       ("wget" ,wget)
       ("cross-gcc" ,(cross-gcc "i686-linux-gnu"
                                #:xbinutils (cross-binutils "i686-linux-gnu")
                                #:libc (cross-libc "i686-linux-gnu")))
       ("cross-libc" ,(cross-libc "i686-linux-gnu"))
       ("cross-libc-static" ,(cross-libc "i686-linux-gnu") "static")))
    (home-page "https://xenproject.org/")
    (synopsis "Xen Virtual Machine Monitor Tools")
    (description "This package provides tools for the Xen Virtual Machine Monitor.")
    (license license:gpl2)
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))))

(define-public xen-0-docs
  (package
    (name "xen-0-docs")
    (version "4.12.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://xenbits.xenproject.org/xen.git")
                    (commit (string-append "RELEASE-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
		"1lchpxmqsza6b720mlvglm8fc3j5c8kdhdhn6m7b6vk1cfd6zaz1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-stubdom"
	     "--disable-xen"
	     "--disable-tools"
	     "--enable-docs"
	     "--enable-rpath")       
       #:make-flags (list "-j" "1"
                          "XEN_BUILD_DATE=Thu Jan  1 01:00:01 CET 1970"
                          "XEN_BUILD_TIME=01:00:01"
                          "XEN_BUILD_HOST="
                          "ETHERBOOT_NICS="
                          "SMBIOS_REL_DATE=01/01/1970"
                          "VGABIOS_REL_DATE=01 Jan 1970"
                          "SYSCONFIG_DIR=/tmp/etc/default"
                          (string-append "BASH_COMPLETION_DIR="
                                         (assoc-ref %outputs "out")
                                         "/etc/bash_completion.d")
                          (string-append "BOOT_DIR="
                                         (assoc-ref %outputs "out")
                                         "/boot")
                          (string-append "DEBUG_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/debug")
                          (string-append "EFI_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/efi") ; TODO lib64 ?
                          "MINIOS_UPSTREAM_URL=")
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'unpack-mini-os
          (lambda* (#:key inputs #:allow-other-keys)
            (copy-recursively (assoc-ref inputs "mini-os") "extras/mini-os")
            #t))
        (add-after 'unpack-mini-os 'patch-minios-rules
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "tools/firmware/Rules.mk"
             (("override XEN_TARGET_ARCH = x86_32")
              (string-append "override XEN_TARGET_ARCH = x86_32
override CC = " (assoc-ref inputs "cross-gcc") "/bin/i686-linux-gnu-gcc"))
             (("^CFLAGS =$")
              (string-append "CFLAGS=-I" (assoc-ref inputs "cross-libc")
                             "/include\n")))
            (substitute* "config/x86_32.mk"
             (("CFLAGS += -m32 -march=i686")
              (string-append "CFLAGS += -march=i686 -I"
                             (assoc-ref inputs "cross-libc")
                             "/include")))
            (substitute* '("tools/Makefile"
                           "tools/xenstore/Makefile"
                           "tools/xenpaging/Makefile"
			   "tools/flask/policy/Makefile.common")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(DUMP|LOG|RUN|LIB|PAGING)_DIR.*")
              "\n")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(RUN|LIB)_STORED.*")
              "\n"))
            (substitute* "tools/examples/Makefile"
             ((" install-readmes") "")
             ((" install-configs") ""))
            (substitute* "tools/pygrub/setup.py"
             (("library_dirs =")
              (string-append "runtime_library_dirs = ['"
                             (assoc-ref outputs "out")
                             "/lib'],\nlibrary_dirs =")))
            #t))
        (add-before 'configure 'patch-xen-script-directory
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* '("configure"
                           "tools/configure"
                           "docs/configure")
             (("XEN_SCRIPT_DIR=.*")
              (string-append "XEN_SCRIPT_DIR="
                             (assoc-ref outputs "out")
                             "/etc/xen/scripts")))
            #t))
        (add-before 'patch-xen-script-directory 'set-environment-up
          (lambda* (#:key make-flags #:allow-other-keys)
             (define (cross? x)
               (string-contains x "cross-i686-linux"))
             (define (filter-environment! filter-predicate
                                          environment-variable-names)
               (for-each
                (lambda (env-name)
                  (let* ((env-value (or (getenv env-name) ""))
                         (search-path (search-path-as-string->list env-value))
                         (new-search-path (filter filter-predicate
                                                  search-path))
                         (new-env-value (list->search-path-as-string
                                         new-search-path ":")))
                    (setenv env-name new-env-value)))
                environment-variable-names))
             (setenv "CROSS_C_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))
             (setenv "CROSS_CPLUS_INCLUDE_PATH" (getenv "CPLUS_INCLUDE_PATH"))
             (setenv "CROSS_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
             (filter-environment! cross?
              '("CROSS_C_INCLUDE_PATH" "CROSS_CPLUS_INCLUDE_PATH"
                "CROSS_LIBRARY_PATH"))
             (filter-environment! (lambda (e) (not (cross? e)))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
             (filter-environment! (lambda (e)
                                    (not
                                     (string-contains e
                                      "mini-os-git-checkout")))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
            (setenv "EFI_VENDOR" "guix")
	    #t))
	(add-before 'set-environment-up 'our-own-xen-0-config
	  (lambda* (#:key outputs inputs native-inputs #:allow-other-keys)
	    (let ((xen-config (assoc-ref native-inputs "xen-config")))
	      (if xen-config	     
		  (copy-file xen-config "xen/.config"))
	      #t)))
	(delete 'patch)
        (replace 'build
		 (lambda* (#:key make-flags #:allow-other-keys)
			  (invoke "mkdir" "-p" "tools/include/gnu/")
			  (invoke "touch" "tools/include/gnu/stubs-32.h")
			  (apply invoke "make" "clean" "world" make-flags)))
	(add-before 'install 'move-root-things
	  (lambda* (#:key outputs #:allow-other-keys)
		   (let* ((out (assoc-ref outputs "out"))
			  (a (string-append out "/a"))
			  (dist "./dist/install")
			  (dist-a (string-append dist a)))
		     (invoke "mkdir" "-p" dist-a)
		     (invoke "find" dist "-maxdepth" "1"
			     "!" "(" "-name" "gnu" "-or" "-name" "\\.*" ")"
			     "-exec" "mv" "-v" "{}" dist-a ";"))))
	(delete 'check)
        (replace 'install
	 (lambda* (#:key make-flags outputs #:allow-other-keys)
	  (let* ((out (assoc-ref outputs "out"))
		 (command_line "cd dist && ./install.sh /")
		 (shell "bash"))
		    (invoke shell "-c" command_line)))))))
    (inputs
     `(("acpica" ,acpica)
       ("bridge-utils" ,bridge-utils)
       ("checkpolicy" ,checkpolicy)
       ("glib" ,glib)
       ("iproute" ,iproute)
       ("libaio" ,libaio)
       ("libx11" ,libx11)
       ("libyajl" ,libyajl)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("pixman" ,pixman)
       ("util-linux" ,util-linux)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("dev86" ,dev86)
       ("bison" ,bison)
       ("cmake" ,cmake)
       ("figlet" ,figlet)
       ("flex" ,flex)
       ("gettext" ,gettext-minimal)
       ("libnl" ,libnl)
       ("mini-os"
       ,(origin
         (method git-fetch)
         (uri (git-reference
               (url "http://xenbits.xen.org/git-http/mini-os.git")
               (commit (string-append "xen-RELEASE-" version))))
         (sha256
          (base32
           "1i8pcl19n60i2m9vlg79q3nknpj209c9ic5x10wxaicx45kc107f"))
         (file-name "mini-os-git-checkout")))
       ("ocaml" ,ocaml)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python2)
       ("wget" ,wget)
       ("cross-gcc" ,(cross-gcc "i686-linux-gnu"
                                #:xbinutils (cross-binutils "i686-linux-gnu")
                                #:libc (cross-libc "i686-linux-gnu")))
       ("cross-libc" ,(cross-libc "i686-linux-gnu"))
       ("cross-libc-static" ,(cross-libc "i686-linux-gnu") "static")
       ("xen-config" ,(search-auxiliary-file "xen-0/xen.config"))))
    (home-page "https://xenproject.org/")
    (synopsis "Xen Virtual Machine Monitor Tools")
    (description "This package provides tools for the Xen Virtual Machine Monitor.")
    (license license:gpl2)
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))))

(define-public xen-0-qemu
  (package
   (name "xen-0-qemu")
    (version "4.1.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qemu.org/qemu-"
                                 version ".tar.xz"))
             (sha256
              (base32
	       "1ih9v6gxgild3m4g80ld4dr3wp9db3bpy203k73fxgc9hqhn0vk5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-tests? #f
       #:configure-flags (list "--enable-usb-redir" "--enable-opengl"
                               (string-append "--smbd="
                                              (assoc-ref %outputs "out")
                                              "/libexec/samba-wrapper")
			       "--target-list=x86_64-softmmu"
			       "--disable-werror"
			       "--audio-drv-list=alsa"
			       "--enable-xen"
			       "--enable-xen-pci-passthrough"
			       "--enable-libusb"
			       "--enable-vnc"
			       "--enable-spice"
			       "--disable-tools")
       #:make-flags '("V=1")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs (configure-flags '())
                           #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "SHELL" (which "bash"))
               (substitute* "tests/libqtest.c"
                 (("/bin/sh") (which "sh")))
               (setenv "LDFLAGS" "-lrt")
               (apply invoke
                      `("./configure"
                        ,(string-append "--cc=" (which "gcc"))
                        ,(string-append "--host-cc=" (which "gcc"))
                        "--disable-debug-info"
                        "--enable-virtfs"
                        ,(string-append "--prefix=" out)
                        ,(string-append "--sysconfdir=/etc")
                        ,@configure-flags)))))
	 (add-before 'build 'include-gnu-stub-32
	    (lambda* (#:key make-flags #:allow-other-keys)
	      (invoke "mkdir" "-p" "linux-headers/gnu")
	      (invoke "touch" "linux-headers/gnu/stubs-32.h")
	      #t))
	 (delete 'check)
	 (add-before 'install 'configure-for-install
		     (lambda* (#:key inputs outputs (configure-flags '()) #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "SHELL" (which "bash"))
               (substitute* "tests/libqtest.c"
                 (("/bin/sh") (which "sh")))
               (setenv "LDFLAGS" "-lrt")
               (apply invoke
                      `("./configure"
                        ,(string-append "--cc=" (which "gcc"))
                        ,(string-append "--host-cc=" (which "gcc"))
                        "--disable-debug-info"
                        "--enable-virtfs"
                        ,(string-append "--prefix=" out)
                        ,(string-append "--sysconfdir=/etc")
                        ,@configure-flags)))))
	 (add-after 'install 'install-info
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (when (assoc-ref inputs "texinfo")
               (let* ((out  (assoc-ref outputs "out"))
                      (dir (string-append out "/share/info")))
                 (invoke "make" "info")
                 (for-each (lambda (info)
                             (install-file info dir))
                           (find-files "." "\\.info"))))
             #t))
         (add-after 'install-info 'create-samba-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref %outputs "out"))
                    (libexec (string-append out "/libexec")))
               (call-with-output-file "samba-wrapper"
                 (lambda (port)
                   (format port "#!/bin/sh
exec smbd $@")))
               (chmod "samba-wrapper" #o755)
               (install-file "samba-wrapper" libexec))
             #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("attr" ,attr)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libaio" ,libaio)
       ("libattr" ,attr)
       ("libcap" ,libcap)
       ("libdrm" ,libdrm)
       ("libepoxy" ,libepoxy)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libseccomp" ,libseccomp)
       ("libusb" ,libusb)
       ("mesa" ,mesa)
       ("ncurses" ,ncurses)
       ("pixman" ,pixman)
       ("pulseaudio" ,pulseaudio)
       ("sdl2" ,sdl2)
       ("spice" ,spice)
       ("usbredir" ,usbredir)
       ("util-linux" ,util-linux)
       ("virglrenderer" ,virglrenderer)
       ("xen-0-libs" ,xen-0-libs) ;; XXX
       ("zlib" ,zlib)))
    (native-inputs `(("gettext" ,gettext-minimal)
                     ("glib:bin" ,glib "bin")
                     ("perl" ,perl)
                     ("flex" ,flex)
                     ("bison" ,bison)
                     ("pkg-config" ,pkg-config)
                     ("python-wrapper" ,python-minimal-wrapper)
                     ("texinfo" ,texinfo)))
    (home-page "https://www.qemu.org")
    (synopsis "Machine emulator and virtualizer")
    (description
     "QEMU is a generic machine emulator and virtualizer.

When used as a machine emulator, QEMU can run OSes and programs made for one
machine (e.g. an ARM board) on a different machine---e.g., your own PC.  By
using dynamic translation, it achieves very good performance.

When used as a virtualizer, QEMU achieves near native performances by
executing the guest code directly on the host CPU.  QEMU supports
virtualization when executing under the Xen hypervisor or using
the KVM kernel module in Linux.  When using KVM, QEMU can virtualize x86,
server and embedded PowerPC, and S390 guests.")

    (license license:gpl2)
    (supported-systems (delete "mips64el-linux" %supported-systems))))

(define-public xen-0-tools
  (package
    (name "xen-0-tools")
    (version "4.12.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://xenbits.xenproject.org/xen.git")
                    (commit (string-append "RELEASE-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
		"1lchpxmqsza6b720mlvglm8fc3j5c8kdhdhn6m7b6vk1cfd6zaz1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-stubdom"
	     "--disable-xen"
	     "--enable-tools"
	     "--disable-docs"
	     "--enable-rpath"
	     "--disable-ocamltools"
	     "--disable-qemu-traditional"
             (string-append "--with-initddir="
			    (assoc-ref %outputs "out")
                            "/etc/init.d")
	     (string-append "--with-system-qemu="
			    (assoc-ref %build-inputs "xen-0-qemu")
			    "/bin/qemu-system-x86_64")
	     (string-append "--with-system-seabios="
			    (assoc-ref %build-inputs "xen-0-qemu")
			    "/share/qemu/bios.bin")
	     (string-append "--with-system-ovmf="
			    (assoc-ref %build-inputs "xen-0-qemu")
			    "/share/qemu/ovmf_x86.bin"))
       #:make-flags (list "-j" "1"
                          "XEN_BUILD_DATE=Thu Jan  1 01:00:01 CET 1970"
                          "XEN_BUILD_TIME=01:00:01"
                          "XEN_BUILD_HOST="
                          "ETHERBOOT_NICS="
                          "SMBIOS_REL_DATE=01/01/1970"
                          "VGABIOS_REL_DATE=01 Jan 1970"
                          "SYSCONFIG_DIR=/tmp/etc/default"
                          (string-append "BASH_COMPLETION_DIR="
                                         (assoc-ref %outputs "out")
                                         "/etc/bash_completion.d")
                          (string-append "BOOT_DIR="
                                         (assoc-ref %outputs "out")
                                         "/boot")
                          (string-append "DEBUG_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/debug")
                          (string-append "EFI_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/efi")
                          "MINIOS_UPSTREAM_URL=")
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'unpack-mini-os
          (lambda* (#:key inputs #:allow-other-keys)
            (copy-recursively (assoc-ref inputs "mini-os") "extras/mini-os")
            #t))
        (add-after 'unpack-mini-os 'patch-minios-rules
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "tools/firmware/Rules.mk"
             (("override XEN_TARGET_ARCH = x86_32")
              (string-append "override XEN_TARGET_ARCH = x86_32
override CC = " (assoc-ref inputs "cross-gcc") "/bin/i686-linux-gnu-gcc"))
             (("^CFLAGS =$")
              (string-append "CFLAGS=-I" (assoc-ref inputs "cross-libc")
                             "/include\n")))
            (substitute* "config/x86_32.mk"
             (("CFLAGS += -m32 -march=i686")
              (string-append "CFLAGS += -march=i686 -I"
                             (assoc-ref inputs "cross-libc")
                             "/include")))
            (substitute* '("tools/Makefile"
                           "tools/xenstore/Makefile"
                           "tools/xenpaging/Makefile"
			   "tools/flask/policy/Makefile.common")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(DUMP|LOG|RUN|LIB|PAGING)_DIR.*")
              "\n")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(RUN|LIB)_STORED.*")
              "\n"))
            (substitute* "tools/examples/Makefile"
             ((" install-readmes") "")
             ((" install-configs") ""))
            (substitute* "tools/pygrub/setup.py"
             (("library_dirs =")
              (string-append "runtime_library_dirs = ['"
                             (assoc-ref outputs "out")
                             "/lib'],\nlibrary_dirs =")))
            #t))
	(add-after 'configure 'patch-perl-and-mkdir
	  (lambda* (#:key inputs outputs #:allow-other-keys)
	    (substitute* '("tools/hotplug/Linux/locking.sh")
			 (("perl") (which "perl"))
			 (("mkdir") (which "mkdir")))
	    (substitute* '("tools/hotplug/Linux/launch-xenstore.in" "tools/hotplug/Linux/launch-xenstore")
			 (("/bin/mkdir") (which "mkdir")))
	    (substitute* '("tools/hotplug/Linux/xen-network-common.sh")
			 (("false") (which "false")))
	    #t))
        (add-before 'configure 'patch-xen-script-directory
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* '("configure"
                           "tools/configure"
                           "docs/configure")
             (("XEN_SCRIPT_DIR=.*")
              (string-append "XEN_SCRIPT_DIR="
                             (assoc-ref outputs "out")
                             "/etc/xen/scripts")))
            #t))
        (add-before 'patch-xen-script-directory 'set-environment-up
          (lambda* (#:key make-flags #:allow-other-keys)
             (define (cross? x)
               (string-contains x "cross-i686-linux"))
             (define (filter-environment! filter-predicate
                                          environment-variable-names)
               (for-each
                (lambda (env-name)
                  (let* ((env-value (or (getenv env-name) ""))
                         (search-path (search-path-as-string->list env-value))
                         (new-search-path (filter filter-predicate
                                                  search-path))
                         (new-env-value (list->search-path-as-string
                                         new-search-path ":")))
                    (setenv env-name new-env-value)))
                environment-variable-names))
             (setenv "CROSS_C_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))
             (setenv "CROSS_CPLUS_INCLUDE_PATH" (getenv "CPLUS_INCLUDE_PATH"))
             (setenv "CROSS_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
             (filter-environment! cross?
              '("CROSS_C_INCLUDE_PATH" "CROSS_CPLUS_INCLUDE_PATH"
                "CROSS_LIBRARY_PATH"))
             (filter-environment! (lambda (e) (not (cross? e)))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
             (filter-environment! (lambda (e)
                                    (not
                                     (string-contains e
                                      "mini-os-git-checkout")))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
            (setenv "EFI_VENDOR" "guix")
	    #t))
	(add-before 'set-environment-up 'our-own-xen-0-config
	  (lambda* (#:key outputs inputs native-inputs #:allow-other-keys)
	    (let ((xen-config (assoc-ref native-inputs "xen-config")))
	      (if xen-config	     
		  (copy-file xen-config "xen/.config"))
	      #t)))
	(delete 'patch)
        (replace 'build
		 (lambda* (#:key make-flags #:allow-other-keys)
			  (invoke "mkdir" "-p" "tools/include/gnu/")
			  (invoke "touch" "tools/include/gnu/stubs-32.h")
			  (apply invoke "make" "clean" "world" make-flags)))
	(add-before 'install 'move-root-things
	  (lambda* (#:key outputs #:allow-other-keys)
		   (let* ((out (assoc-ref outputs "out"))
			  (a (string-append out "/a"))
			  (dist "./dist/install")
			  (dist-a (string-append dist a)))
		     (invoke "mkdir" "-p" dist-a)
		     (invoke "find" dist "-maxdepth" "1"
			     "!" "(" "-name" "gnu" "-or" "-name" "\\.*" ")"
			     "-exec" "mv" "-v" "{}" dist-a ";"))))
        (replace 'install
	 (lambda* (#:key make-flags outputs #:allow-other-keys)
	  (let* ((out (assoc-ref outputs "out"))
		 (command_line "cd dist && ./install.sh /")
		 (shell "bash"))
		    (invoke shell "-c" command_line)))))))
    (inputs
     `(("acpica" ,acpica)
       ("bridge-utils" ,bridge-utils)
       ("checkpolicy" ,checkpolicy)
       ("glib" ,glib)
       ("iproute" ,iproute)
       ("libaio" ,libaio)
       ("libx11" ,libx11)
       ("libyajl" ,libyajl)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("pixman" ,pixman)
       ("xen-0-qemu" ,xen-0-qemu)
       ("util-linux" ,util-linux)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("dev86" ,dev86)
       ("bison" ,bison)
       ("cmake" ,cmake)
       ("figlet" ,figlet)
       ("flex" ,flex)
       ("gettext" ,gettext-minimal)
       ("libnl" ,libnl)
       ("mini-os"
       ,(origin
         (method git-fetch)
         (uri (git-reference
               (url "http://xenbits.xen.org/git-http/mini-os.git")
               (commit (string-append "xen-RELEASE-" version))))
         (sha256
          (base32
           "1i8pcl19n60i2m9vlg79q3nknpj209c9ic5x10wxaicx45kc107f"))
         (file-name "mini-os-git-checkout")))
       ("ocaml" ,ocaml)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python2)
       ("wget" ,wget)
       ("cross-gcc" ,(cross-gcc "i686-linux-gnu"
                                #:xbinutils (cross-binutils "i686-linux-gnu")
                                #:libc (cross-libc "i686-linux-gnu")))
       ("cross-libc" ,(cross-libc "i686-linux-gnu"))
       ("cross-libc-static" ,(cross-libc "i686-linux-gnu") "static")
       ("xen-config" ,(search-auxiliary-file "xen-0/xen.config"))))
    (home-page "https://xenproject.org/")
    (synopsis "Xen Virtual Machine Monitor Tools")
    (description "This package provides tools for the Xen Virtual Machine Monitor.")
    (license license:gpl2)
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))))

(define-public xen-0-libs
  (package
    (name "xen-0-libs")
    (version "4.12.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://xenbits.xenproject.org/xen.git")
                    (commit (string-append "RELEASE-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
		"1lchpxmqsza6b720mlvglm8fc3j5c8kdhdhn6m7b6vk1cfd6zaz1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-stubdom"
	     "--disable-xen"
	     "--enable-tools"
	     "--disable-docs"
	     "--enable-rpath"
	     "--disable-ocamltools"
	     "--disable-qemu-traditional"
             (string-append "--with-initddir="
			    (assoc-ref %outputs "out")
                            "/etc/init.d")
             (string-append "--with-system-qemu="
                            (assoc-ref %build-inputs "qemu")
                            "/bin/qemu-system-i386")
             (string-append "--with-system-seabios="
                            (assoc-ref %build-inputs "seabios")
                            "/share/firmware/bios.bin")
             (string-append "--with-system-ovmf="
                            (assoc-ref %build-inputs "ovmf")
                            "/share/firmware/ovmf_ia32.bin"))
       #:make-flags (list "-j" "1"
                          "XEN_BUILD_DATE=Thu Jan  1 01:00:01 CET 1970"
                          "XEN_BUILD_TIME=01:00:01"
                          "XEN_BUILD_HOST="
                          "ETHERBOOT_NICS="
                          "SMBIOS_REL_DATE=01/01/1970"
                          "VGABIOS_REL_DATE=01 Jan 1970"
                          "SYSCONFIG_DIR=/tmp/etc/default"
                          (string-append "BASH_COMPLETION_DIR="
                                         (assoc-ref %outputs "out")
                                         "/etc/bash_completion.d")
                          (string-append "BOOT_DIR="
                                         (assoc-ref %outputs "out")
                                         "/boot")
                          (string-append "DEBUG_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/debug")
                          (string-append "EFI_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/efi")
                          "MINIOS_UPSTREAM_URL=")
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'unpack-mini-os
          (lambda* (#:key inputs #:allow-other-keys)
            (copy-recursively (assoc-ref inputs "mini-os") "extras/mini-os")
            #t))
        (add-after 'unpack-mini-os 'patch-minios-rules
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "tools/firmware/Rules.mk"
             (("override XEN_TARGET_ARCH = x86_32")
              (string-append "override XEN_TARGET_ARCH = x86_32
override CC = " (assoc-ref inputs "cross-gcc") "/bin/i686-linux-gnu-gcc"))
             (("^CFLAGS =$")
              (string-append "CFLAGS=-I" (assoc-ref inputs "cross-libc")
                             "/include\n")))
            (substitute* "config/x86_32.mk"
             (("CFLAGS += -m32 -march=i686")
              (string-append "CFLAGS += -march=i686 -I"
                             (assoc-ref inputs "cross-libc")
                             "/include")))
            (substitute* '("tools/Makefile"
                           "tools/xenstore/Makefile"
                           "tools/xenpaging/Makefile"
			   "tools/flask/policy/Makefile.common")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(DUMP|LOG|RUN|LIB|PAGING)_DIR.*")
              "\n")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(RUN|LIB)_STORED.*")
              "\n"))
            (substitute* "tools/examples/Makefile"
             ((" install-readmes") "")
             ((" install-configs") ""))
            (substitute* "tools/pygrub/setup.py"
             (("library_dirs =")
              (string-append "runtime_library_dirs = ['"
                             (assoc-ref outputs "out")
                             "/lib'],\nlibrary_dirs =")))
            #t))
        (add-before 'configure 'patch-xen-script-directory
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* '("configure"
                           "tools/configure"
                           "docs/configure")
             (("XEN_SCRIPT_DIR=.*")
              (string-append "XEN_SCRIPT_DIR="
                             (assoc-ref outputs "out")
                             "/etc/xen/scripts")))
            #t))
        (add-before 'patch-xen-script-directory 'set-environment-up
          (lambda* (#:key make-flags #:allow-other-keys)
             (define (cross? x)
               (string-contains x "cross-i686-linux"))
             (define (filter-environment! filter-predicate
                                          environment-variable-names)
               (for-each
                (lambda (env-name)
                  (let* ((env-value (or (getenv env-name) ""))
                         (search-path (search-path-as-string->list env-value))
                         (new-search-path (filter filter-predicate
                                                  search-path))
                         (new-env-value (list->search-path-as-string
                                         new-search-path ":")))
                    (setenv env-name new-env-value)))
                environment-variable-names))
             (setenv "CROSS_C_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))
             (setenv "CROSS_CPLUS_INCLUDE_PATH" (getenv "CPLUS_INCLUDE_PATH"))
             (setenv "CROSS_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
             (filter-environment! cross?
              '("CROSS_C_INCLUDE_PATH" "CROSS_CPLUS_INCLUDE_PATH"
                "CROSS_LIBRARY_PATH"))
             (filter-environment! (lambda (e) (not (cross? e)))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
             (filter-environment! (lambda (e)
                                    (not
                                     (string-contains e
                                      "mini-os-git-checkout")))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
            (setenv "EFI_VENDOR" "guix")
	    #t))
	(add-before 'set-environment-up 'our-own-xen-0-config
	  (lambda* (#:key outputs inputs native-inputs #:allow-other-keys)
	    (let ((xen-config (assoc-ref native-inputs "xen-config")))
	      (if xen-config	     
		  (copy-file xen-config "xen/.config"))
	      #t)))
	(delete 'patch)
        (replace 'build
		 (lambda* (#:key make-flags #:allow-other-keys)
			  (invoke "mkdir" "-p" "tools/include/gnu/")
			  (invoke "touch" "tools/include/gnu/stubs-32.h")
			  (apply invoke "make" "clean" "world" make-flags)))
	(add-before 'install 'move-root-things
	  (lambda* (#:key outputs #:allow-other-keys)
		   (let* ((out (assoc-ref outputs "out"))
			  (a (string-append out "/a"))
			  (dist "./dist/install")
			  (dist-a (string-append dist a)))
		     (invoke "mkdir" "-p" dist-a)
		     (invoke "find" dist "-maxdepth" "1"
			     "!" "(" "-name" "gnu" "-or" "-name" "\\.*" ")"
			     "-exec" "mv" "-v" "{}" dist-a ";"))))
        (replace 'install
	 (lambda* (#:key make-flags outputs #:allow-other-keys)
	  (let* ((out (assoc-ref outputs "out"))
		 (command_line "cd dist && ./install.sh /")
		 (shell "bash"))
		    (invoke shell "-c" command_line)))))))
    (inputs
     `(("acpica" ,acpica)
       ("bridge-utils" ,bridge-utils)
       ("checkpolicy" ,checkpolicy)
       ("glib" ,glib)
       ("iproute" ,iproute)
       ("libaio" ,libaio)
       ("libx11" ,libx11)
       ("libyajl" ,libyajl)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("ovmf" ,ovmf)
       ("pixman" ,pixman)
       ("qemu" ,qemu-minimal)
       ("seabios" ,seabios)
       ("util-linux" ,util-linux)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("dev86" ,dev86)
       ("bison" ,bison)
       ("cmake" ,cmake)
       ("figlet" ,figlet)
       ("flex" ,flex)
       ("gettext" ,gettext-minimal)
       ("libnl" ,libnl)
       ("mini-os"
       ,(origin
         (method git-fetch)
         (uri (git-reference
               (url "http://xenbits.xen.org/git-http/mini-os.git")
               (commit (string-append "xen-RELEASE-" version))))
         (sha256
          (base32
           "1i8pcl19n60i2m9vlg79q3nknpj209c9ic5x10wxaicx45kc107f"))
         (file-name "mini-os-git-checkout")))
       ("ocaml" ,ocaml)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python2)
       ("wget" ,wget)
       ("cross-gcc" ,(cross-gcc "i686-linux-gnu"
                                #:xbinutils (cross-binutils "i686-linux-gnu")
                                #:libc (cross-libc "i686-linux-gnu")))
       ("cross-libc" ,(cross-libc "i686-linux-gnu"))
       ("cross-libc-static" ,(cross-libc "i686-linux-gnu") "static")
       ("xen-config" ,(search-auxiliary-file "xen-0/xen.config"))))
    (home-page "https://xenproject.org/")
    (synopsis "Xen Virtual Machine Monitor Tools")
    (description "This package provides tools for the Xen Virtual Machine Monitor.")
    (license license:gpl2)
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))))

