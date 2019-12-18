;; xen-tools.scm 20191003,gl for an environment to build the xen-tools
;; use:
;;  guix environment -l s/xen-tools

(define-module (gnu packages xen-stubdom-m64)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages figlet)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)  
  #:use-module (gnu packages xorg)

  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  )

(define-public xen-tools
  (package
    (name "xen-tools")
    (version "4.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://xenbits.xenproject.org/xen.git")
		    #;(commit "ddccd9f")
                    (commit (string-append "stable-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
		"0xgrdzrzmc0ml471nabzd8xal3hiq3xzssyr9fhnp4qangm30ksp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-stubdom"
	     "--disable-xen"
	     "--enable-tools"
	     "--disable-docs"
	     "--enable-rpath"

	     "ac_cv_header_Python_h=yes"
	     "ac_cv_header_uuid_h=yes"
	     "ac_cv_header_uuid_uuid_h=yes"
	     "ax_cv_pthread_flags=-pthread"
	     "ac_cv_header_argp_h=yes"

	     ;; "--prefix=/usr/local"
	     ;; "--disable-qemu-traditional" ; It tries to do "git clone"
             ;; "--disable-rombios" ; would try to "git clone" via etherboot.
             ;; TODO: Re-enable stubdom (it's "more secure" to use it).
	     ;; "--disable-stubdom"  tries to "git clone" old patched newlib.
             (string-append "--with-initddir="
                            (assoc-ref %outputs "out")
                            "/etc/init.d")
            #; (string-append "--with-system-qemu="
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
                          ; QEMU_TRADITIONAL_LOC
                          ; QEMU_UPSTREAM_LOC
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
                          "MINIOS_UPSTREAM_URL="
                          ;(string-append "DISTDIR="
                          ;               (assoc-ref %outputs "out"))
)
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'unpack-mini-os
          (lambda* (#:key inputs #:allow-other-keys)
            (copy-recursively (assoc-ref inputs "mini-os") "extras/mini-os")
            #t))
        (add-after 'unpack-mini-os 'patch
          (lambda* (#:key inputs outputs #:allow-other-keys)
            #;(substitute* "tools/firmware/Rules.mk"
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
            ;; /var is not in /gnu/store , so don't try to create it.
            (substitute* '("tools/Makefile"
                           "tools/xenstore/Makefile"
                           "tools/xenpaging/Makefile")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(DUMP|LOG|RUN|LIB|PAGING)_DIR.*")
              "\n")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(RUN|LIB)_STORED.*")
              "\n"))
            ;; Prevent xen from creating /etc .
            (substitute* "tools/examples/Makefile"
             ((" install-readmes") "")
             ((" install-configs") ""))
            ;; Set rpath.
            (substitute* "tools/pygrub/setup.py"
             (("library_dirs =")
              ; TODO: extra_link_args = ['-Wl,-rpath=/opt/foo'],
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
        (add-before 'configure 'set-environment-up
          (lambda* (#:key make-flags #:allow-other-keys)
             (define (cross? x)
               (string-contains x "cross-i686-linux"))
             (define (filter-environment! filter-predicate
                                          environment-variable-names)
               (for-each
                (lambda (env-name)
                  (let* ((env-value (getenv env-name))
                         (search-path (and env-value (search-path-as-string->list env-value)))
                         (new-search-path (and env-value (filter filter-predicate
                                                  search-path)))
                         (new-env-value (and env-value (list->search-path-as-string
                                         new-search-path ":"))))
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
             ;; Guix tries to be helpful and automatically adds
             ;; mini-os-git-checkout/include to the include path,
             ;; but actually we don't want it to be there (yet).
             (filter-environment! (lambda (e)
                                    (not
                                     (string-contains e
                                      "mini-os-git-checkout")))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
            (setenv "EFI_VENDOR" "guix")
             #t))
        (replace 'build
          (lambda* (#:key make-flags #:allow-other-keys)
            (apply invoke "make" "world" make-flags))))))
    (inputs
     `(("acpica" ,acpica) ; TODO: patch iasl invocation.
       ("bridge-utils" ,bridge-utils) ; TODO: patch invocations.
       ("glib" ,glib)
       ("iproute" ,iproute) ; TODO: patch invocations.
       ("libaio" ,libaio)
       ("libx11" ,libx11)
       ("libyajl" ,libyajl)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("ovmf" ,ovmf)
       ("pixman" ,pixman)
       ("qemu" ,qemu-minimal)
       ("seabios" ,seabios)
       ("util-linux" ,util-linux) ; uuid
       ; TODO: ocaml-findlib, ocaml-nox.
       ("xz" ,xz) ; for liblzma
       ("zlib" ,zlib)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("dev86" ,dev86)
       ("bison" ,bison)
       #;("cmake" ,cmake)
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
       ("perl" ,perl)
       ; TODO: markdown
       ("pkg-config" ,pkg-config)
       ("python" ,python) ;; was: python-2, try: python v3
       ("wget" ,wget)
       #;("cross-gcc" ,(cross-gcc "i686-linux-gnu"
                                #:xbinutils (cross-binutils "i686-linux-gnu")
                                #:libc (cross-libc "i686-linux-gnu")))
       #;("cross-libc" ,(cross-libc "i686-linux-gnu")) ; header files
       #;("cross-libc-static" ,(cross-libc "i686-linux-gnu") "static")))
    (home-page "https://xenproject.org/")
    (synopsis "Xen Virtual Machine Monitor")
    (description "This package provides the Xen Virtual Machine Monitor
which is a hypervisor.")
    ;; TODO: Some files are licensed differently.  List those.
    (license license:gpl2)
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))))

xen-tools ;; guix environment -l s/xen-tools
