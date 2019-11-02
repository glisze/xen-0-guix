;; xen-0-docs.scm 20191024,gl for an environment to build the xen-tools
;; use:
;;  guix environment --load-path=here xen-0-docs
;;    guix build --load-path=here xen-0-docs

(define-module (gnu packages xen-0-docs)
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
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages selinux)  
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
;;  #:use-module (guix gexp)
  
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

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
	     "--enable-rpath"
	    #; (append-string "--localstatedir="
			    (assoc-ref %outputs "out")
			    "/tmp/var")
	    #; (append-string "--sysconfdir="
			    (assoc-ref %outputs "out")
			    "/tmp/etc")
	    #; (append-string "XEN_CONFIG_DIR="
			    (assoc-ref %outputs "out")
			    "/tmp/etc")
	    #; (append-string "XEN_DUMP_DIR"
			    (assoc-ref %outputs "out")
			    "/tmp/var/lib/dump")
	    #; (append-string "XEN_LIB_DIR="
			    (assoc-ref %outputs "out")
			    "/tmp/var/lib/xen")
	     ;; ... and more, is that the right path---but
	     ;; "--prefix=/usr/local"
	     ;"--disable-ocamltools"
	     ;"--disable-qemu-traditional" ; It tries to do "git clone"
             ;; "--disable-rombios" ; would try to "git clone" via etherboot.
             ;; TODO: Re-enable stubdom (it's "more secure" to use it).
	     ;; "--disable-stubdom"  tries to "git clone" old patched newlib.
           #;  (string-append "--with-initddir="
                            (assoc-ref %outputs "out")
                            "/etc/init.d")
	   #; "--with-system-qemu=/usr/local/bin/qemu-system-x86_64"
           #; (string-append "--with-system-qemu="
                            (assoc-ref %build-inputs "qemu")
                            "/bin/qemu-system-i386")
	   #;  "--with-system-seabios=/usr/local/share/qemu/bios.bin"
           #;  (string-append "--with-system-seabios="
                            (assoc-ref %build-inputs "seabios")
                            "/share/firmware/bios.bin")
	   #;  (string-append "--with-system-ovmf="
			    (assoc-ref %build-inputs "ovmf")
			    "/share/firmware/ovmf_ia32.bin")
	   #; "--with-system-ovmf=/usr/local/share/qemu/ovmf_x64.bin")
       
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

			  #;(string-append "DESTDIR="
					 (assoc-ref %outputs "out")
					 "/.")
			  #;(string-append "localstatedir="
					 (assoc-ref %outputs "out") "/tmp/var")
			  #;(string-append "sysconfdir="
					 (assoc-ref %outputs "out") "/tmp/etc")
			  )
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
            ;; /var is not in /gnu/store , so don't try to create it.
            (substitute* '("tools/Makefile"
                           "tools/xenstore/Makefile"
                           "tools/xenpaging/Makefile"
			   "tools/flask/policy/Makefile.common")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(DUMP|LOG|RUN|LIB|PAGING)_DIR.*")
              "\n")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(RUN|LIB)_STORED.*")
              "\n")
	     #;(("\\$\\(INSTALL_DIR\\) .*POLICY_LOADPATH.*")
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
        (add-before 'patch-xen-script-directory 'set-environment-up
          (lambda* (#:key make-flags #:allow-other-keys)
             (define (cross? x)
               (string-contains x "cross-i686-linux"))
             (define (filter-environment! filter-predicate
                                          environment-variable-names)
               (for-each
                (lambda (env-name)
                  (let* ((env-value (getenv env-name))
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
		    (invoke shell "-c" command_line)
	    ;(apply invoke "make" "install" (string-append "DESTDIR=" out) make-flags)
		    #;(apply invoke "make" "clean" "world" make-flags)
		    ))))))
    (inputs
     `(("acpica" ,acpica) ; TODO: patch iasl invocation.
       ("bridge-utils" ,bridge-utils) ; TODO: patch invocations.
       ("checkpolicy" ,checkpolicy)   ; NEW: for a builtin policy
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
       ; TODO: markdown
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("wget" ,wget)
       ("cross-gcc" ,(cross-gcc "i686-linux-gnu"
                                #:xbinutils (cross-binutils "i686-linux-gnu")
                                #:libc (cross-libc "i686-linux-gnu")))
       ("cross-libc" ,(cross-libc "i686-linux-gnu")) ; header files
       ("cross-libc-static" ,(cross-libc "i686-linux-gnu") "static")
       ("xen-config" ,(search-auxiliary-file "xen-0/xen.config"))))
    (home-page "https://xenproject.org/")
    (synopsis "Xen Virtual Machine Monitor Tools")
    (description "This package provides tools for the Xen Virtual Machine Monitor.")
    ;; TODO: Some files are licensed differently.  List those.
    (license license:gpl2)
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))))

;;xen-0-docs ;; guix environment --load-path=here xen-0-docs
