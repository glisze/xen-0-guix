;;; xen-0-qemu.scm 20191026 gl Qemu for Xen
;;; guix environment --load-path=here xen-0-qemu
;;; guix build --load-path=here xen-0-qemu
;;; suggested options: --keep-failed --keep-going

(define-module (gnu packages xen-0-qemu)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xdisorg)

  #:use-module (gnu packages)
  
  #:use-module (guix build-system gnu)  
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)  
  #:use-module (guix packages))

(define-public xen-0-qemu
  (package
   (name "xen-0-qemu")
    (version "4.1.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qemu.org/qemu-"
                                 version ".tar.xz"))
             #;(patches (search-patches "qemu-CVE-2018-16872.patch"
                                      "qemu-CVE-2019-6778.patch"))
             (sha256
              (base32
	       "1ih9v6gxgild3m4g80ld4dr3wp9db3bpy203k73fxgc9hqhn0vk5"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Running tests in parallel can occasionally lead to failures, like:
       ;; boot_sector_test: assertion failed (signature == SIGNATURE): (0x00000000 == 0x0000dead)
       #:parallel-tests? #f
       #:configure-flags (list "--enable-usb-redir" "--enable-opengl"
                               (string-append "--smbd="
                                              (assoc-ref %outputs "out")
                                              "/libexec/samba-wrapper")
                               ;;"--audio-drv-list=alsa,pa,sdl"

			       "--target-list=x86_64-softmmu" ;; minimal effor, for now
			       ;;"--static" ;; a stand alone thing, this may become
			       "--disable-werror" ;; accept warnings
			       ;;"--enable-usb-redir"
			       ;;"--enable-opengl"
			       ;; "--smbd=/usr/local/libexec/sambda-wraper"
			       "--audio-drv-list=alsa"
			       ;; Xen thing
			       "--enable-xen"
			       "--enable-xen-pci-passthrough" ;; this is what we really want
			       ;;"--enable-virtfs"
			       "--enable-libusb"
			       ;; What UI, that is a question
			       "--enable-vnc"
			       ;;"--enable-sdl" 
			       "--enable-spice"
			       ;;"--host-cc=gcc"		  
			       ;;"--cc=gcc"
			       ;;"--prefix=/usr/local"
			       ;; what is out of scope, here, now
			       "--disable-tools"
			       #;"--enable-rpath"
			       )
       ;; Make build and test output verbose to facilitate investigation upon failure.
       #:make-flags '("V=1")
       #:phases
       (modify-phases %standard-phases
	 #;(delete 'patch)	      
         (replace 'configure
           (lambda* (#:key inputs outputs (configure-flags '())
                           #:allow-other-keys)
             ;; The `configure' script doesn't understand some of the
             ;; GNU options.  Thus, add a new phase that's compatible.
             (let ((out (assoc-ref outputs "out")))
               (setenv "SHELL" (which "bash"))

               ;; While we're at it, patch for tests.
               (substitute* "tests/libqtest.c"
                 (("/bin/sh") (which "sh")))

               ;; The binaries need to be linked against -lrt.
               (setenv "LDFLAGS" "-lrt")
               (apply invoke
                      `("./configure"
                        ,(string-append "--cc=" (which "gcc"))
                        ;; Some architectures insist on using HOST_CC
                        ,(string-append "--host-cc=" (which "gcc"))
                        "--disable-debug-info" ; save build space
                        "--enable-virtfs"      ; just to be sure
                        ,(string-append "--prefix=" out)
                        ,(string-append "--sysconfdir=/etc")
                        ,@configure-flags)))))
	 (add-before 'build 'include-gnu-stub-32
	    (lambda* (#:key make-flags #:allow-other-keys)
	      (invoke "mkdir" "-p" "linux-headers/gnu")
	      (invoke "touch" "linux-headers/gnu/stubs-32.h")
	      #t))
	 (delete 'check)
         #;(add-before 'configure-for-check 'disable-test-qga
           (lambda _
             (substitute* "tests/Makefile.include"
               ;; Comment out the test-qga test, which needs /sys and
               ;; fails within the build environment.
               (("check-unit-.* tests/test-qga" all)
                (string-append "# " all)))
	 #t))
	#; (add-before 'check 'configure-for-check
		     (lambda* (#:key inputs outputs (configure-flags '()) #:allow-other-keys)
             ;; bureaucrats' delight
             ;; The `configure' script doesn't understand some of the
             ;; GNU options.  Thus, add a new phase that's compatible.
             (let ((out (assoc-ref outputs "out")))
               (setenv "SHELL" (which "bash"))

               ;; While we're at it, patch for tests.
               (substitute* "tests/libqtest.c"
                 (("/bin/sh") (which "sh")))

               ;; The binaries need to be linked against -lrt.
               (setenv "LDFLAGS" "-lrt")
               (apply invoke
                      `("./configure"
                        ,(string-append "--cc=" (which "gcc"))
                        ;; Some architectures insist on using HOST_CC
                        ,(string-append "--host-cc=" (which "gcc"))
                        "--disable-debug-info" ; save build space
                        "--enable-virtfs"      ; just to be sure
                        ,(string-append "--prefix=" out)
                        ,(string-append "--sysconfdir=/etc")
                        ,@configure-flags)))))
	(add-before 'install 'configure-for-install
		     (lambda* (#:key inputs outputs (configure-flags '()) #:allow-other-keys)
             ;; our control, our responsibility to ensure procedural integrity
             ;; The `configure' script doesn't understand some of the
             ;; GNU options.  Thus, add a new phase that's compatible.
             (let ((out (assoc-ref outputs "out")))
               (setenv "SHELL" (which "bash"))

               ;; While we're at it, patch for tests.
               (substitute* "tests/libqtest.c"
                 (("/bin/sh") (which "sh")))

               ;; The binaries need to be linked against -lrt.
               (setenv "LDFLAGS" "-lrt")
               (apply invoke
                      `("./configure"
                        ,(string-append "--cc=" (which "gcc"))
                        ;; Some architectures insist on using HOST_CC
                        ,(string-append "--host-cc=" (which "gcc"))
                        "--disable-debug-info" ; save build space
                        "--enable-virtfs"      ; just to be sure
                        ,(string-append "--prefix=" out)
                        ,(string-append "--sysconfdir=/etc")
                        ,@configure-flags)))))
	 (add-after 'install 'install-info
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Install the Info manual, unless Texinfo is missing.
             (when (assoc-ref inputs "texinfo")
               (let* ((out  (assoc-ref outputs "out"))
                      (dir (string-append out "/share/info")))
                 (invoke "make" "info")
                 (for-each (lambda (info)
                             (install-file info dir))
                           (find-files "." "\\.info"))))
             #t))
         ;; Create a wrapper for Samba. This allows QEMU to use Samba without
         ;; pulling it in as an input. Note that you need to explicitly install
         ;; Samba in your Guix profile for Samba support.
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
    (inputs                                       ; TODO: Add optional inputs.
     `(("alsa-lib" ,alsa-lib)
       ("attr" ,attr)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libaio" ,libaio)
       ("libattr" ,attr)
       ("libcap" ,libcap)           ; virtfs support requires libcap & libattr
       ("libdrm" ,libdrm)
       ("libepoxy" ,libepoxy)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libseccomp" ,libseccomp)
       ("libusb" ,libusb)                         ;USB pass-through support
       ("mesa" ,mesa)
       ("ncurses" ,ncurses)
       ;; ("pciutils" ,pciutils)
       ("pixman" ,pixman)
       ("pulseaudio" ,pulseaudio)
       ("sdl2" ,sdl2)
       ("spice" ,spice)
       ("usbredir" ,usbredir)
       ("util-linux" ,util-linux)
       ;; ("vde2" ,vde2)
       ("virglrenderer" ,virglrenderer)
       ("xen" ,xen)
       ("zlib" ,zlib)))
    (native-inputs `(("gettext" ,gettext-minimal)
                     ("glib:bin" ,glib "bin") ; gtester, etc.
                     ("perl" ,perl)
                     ("flex" ,flex)
                     ("bison" ,bison)
                     ("pkg-config" ,pkg-config)
                     ("python-wrapper" ,python-wrapper)
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

    ;; Many files are GPLv2+, but some are GPLv2-only---e.g., `memory.c'.
    (license license:gpl2)

    ;; Several tests fail on MIPS; see <http://hydra.gnu.org/build/117914>.
    (supported-systems (delete "mips64el-linux" %supported-systems))))

;;; the working configuration in use today
#; '("src/configure"
		  "--target-list=x86_64-softmmu" ;; minimal effor, for now
		  ;;"--static" ;; a stand alone thing, this may become
		  "--disable-werror" ;; accept warnings
		  "--enable-usb-redir"
		  "--enable-opengl"
		  ;; "--smbd=/usr/local/libexec/sambda-wraper"
		  "--audio-drv-list=alsa"
		  ;; Xen thing
		  "--enable-xen"
		  "--enable-xen-pci-passthrough" ;; this is what we really want
		  "--enable-virtfs"
		  "--enable-libusb"
		  ;; What UI, that is a question
		  "--enable-vnc"
		  ;;"--enable-sdl" 
		  "--enable-spice"
		  "--host-cc=gcc"		  
		  "--cc=gcc"
		  "--prefix=/usr/local"
		  ;; what is out of scope, here, now
		  "--disable-tools"
		 #;"--enable-rpath")
