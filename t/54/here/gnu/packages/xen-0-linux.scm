;;; xen-0-linux (C) 2019 Gunter Liszewski
;;;  guix build --load-path=here linux-x501u

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
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system trivial)
;  #:use-module (guix build-system gnu)  
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)  
  #:use-module (guix download))

(define-public linux-machine-base
  (let* ((version "5.4-rc8"))
    (package
     (inherit linux-libre)
     (name "linux-machine-base")
     (version version)
     (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git")
		    (commit version)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"0c95nz2kssp20024p73409pyqdsxghv97kk8g4qjrfr8ydmkp4g1"))))
     (synopsis "Linux kernel that permits non-free things.")
     (description "A base for a machine specific kernel.")
     (license license:gpl2)
     (home-page "http://kernel.org/"))))

(define-public linux-x501u
  (let* ((machine "x501u")
	 (version "5.4-rc8"))
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
		(let ((config (assoc-ref inputs "Kconfig")))
		  (apply invoke "make" "mrproper" make-flags)
		  (if config-file
		      (begin
			(copy-file config ".config")
			(apply-invoke "make" "olddefconfig" make-flags)))
		  #t)))))))
     (inputs
      `(("Kconfig"
	 ,(search-auxiliary-file "linux-0/x501u.5.4-rc8.config")
	,@(package-inputs linux-libre))))
     (synopsis "Linux for a x501u machine")
     (description "Linux with non-free things for one particular machine model."))))

(define-public linux-firmware-x501u
  (package
    (name "linux-firmware-x501u")
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
			  (fw-dir (string-append %output "/lib/firmware/"))
			  (fw-dir-radeon (string-append fw-dir "radeon/")))
		     (mkdir-p fw-dir-radeon)
		     (for-each (lambda (file)
				 (copy-file file
					    (string-append fw-dir
							   (basename file))))
			       (map (lambda (a) (string-append source a))
				    '("/WHENCE"
				      "/LICENCE.ralink-firmware.txt" "/rt2870.bin" "/rt2860.bin"
				      "/LICENSE.dib0700" "/dvb-usb-dib0700-1.20.fw"
				      "/LICENSE.radeon")))
		     (for-each (lambda (file)
                                 (copy-file file
                                            (string-append fw-dir "/radeon"
                                                           (basename file))))
                               (find-files source
                                           (lambda (file stat)
                                             (string-contains file "radeon"))))
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

#;(define-public perf-nonfree
  (package
    (inherit perf)
    (name "perf-nonfree")
    (version (package-version linux-nonfree))
    (source (package-source linux-nonfree))
    (license (package-license linux-nonfree))))


