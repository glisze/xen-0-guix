;;; (c) 2019 Gunter Liszewski - GPL version 3  or later, see <http://www.gnu.org/licenses/>

(define-module (gnu packages xen-0-initrd)
  #:use-module (gnu packages)
  #:use-module (gnu packages xen-0-busybox)
  ;; #:use-module (gnu packages xen-0-linux)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages compression)  
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)  
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public xen-0-initrd
  (package
    (name "xen-0-initrd")
    (version "0.0.1")
    (source #f)
    (build-system trivial-build-system)
    (native-inputs
     `(("cpio" ,cpio)
       ("gzip" ,gzip)
       ("bash" ,bash)
       ("findutils" ,findutils)))
    (inputs
     `(("xen-0-busybox" ,xen-0-busybox)
       #;("linux-x501u" ,linux-x501u)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
		   (use-modules (guix build utils))
		   
		   (let* ((busybox (assoc-ref %build-inputs "xen-0-busybox"))
			  (bash    (string-append
				    (assoc-ref %build-inputs "bash")
				    "/bin/bash"))
			  (find    (string-append
				    (assoc-ref %build-inputs "findutils")
				    "/bin/find"))
			  (cpio    (string-append
				    (assoc-ref %build-inputs "cpio")
				    "/bin/cpio"))
			  (gzip    (string-append
				    (assoc-ref %build-inputs "gzip")
				    "/bin/gzip"))
			  (out     (assoc-ref %outputs "out"))
			  (i22     (string-append out "/i22.cpio"))
			  (bin     (string-append out "/bin"))
			  (sbin    (string-append out "/sbin"))
			  (sys     (string-append out "/sys"))
			  (self    (string-append out "/proc/self"))
			  (dev     (string-append out "/dev"))
			  (init    (string-append sbin "/init")))
		     (mkdir-p out)
		     (mkdir-p bin)
		     (mkdir-p sbin)
		     (mkdir-p sys)
		     (mkdir-p self)
		     (mkdir-p dev)
		     (call-with-output-file init
		       (lambda (port)
			 (format port "#!~a/bin/sh~%" busybox)
			 (display "
mount -t sysfs -o nodev,nosuid,noexec none /sys
mount -t proc  -o nodev,nosuid,noexec none /proc
mount -t devtmpfs none /dev
"
				port)
			 (chmod port #o777)))
		     ;; and then some ...
		     (invoke bash "-c"
			     (format #f "~a ~a -name busybox -or -name sh | ~a -o -H newc -F ~a"
				     find busybox cpio i22))
		     (invoke gzip "-9" i22)
		     #t))))
    (synopsis "A minimal BUSYBOX initrd")
    (description "")
    (home-page "http://the-number.github.io/129")
    (license license:lgpl2.1+)))
