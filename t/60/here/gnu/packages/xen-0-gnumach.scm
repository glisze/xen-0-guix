;;; 2020 (c) Gunter Liszewski (exercise: a GNUmach to for the platform of Xen)

;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2018, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Rene Saavedra <pacoon@protonmail.com>
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

(define-module (gnu packages xen-0-gnumach)
  #:use-module ((guix licenses) #:hide (zlib))
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
;  #:use-module (gnu build hurd-boot) ;XXX: the GNUmach surely does not need this (TODO)
  #:use-module (gnu packages autotools)
;  #:use-module (gnu packages compression)
;  #:use-module (gnu packages flex) ;XXX: keep the imports to its minimum (TODO)
;  #:use-module (gnu packages gawk)
;  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages hurd)  ;XXX: xen-0-hurd
;  #:use-module (gnu packages bison)
;  #:use-module (gnu packages libdaemon)
;  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
;  #:use-module (gnu packages pkg-config)
;  #:use-module (gnu packages base)
;  #:use-module (gnu packages bash)
  #:use-module (gnu packages texinfo)
;  #:use-module (gnu packages onc-rpc)
;  #:use-module (gnu packages xorg) ; libpciaccess
  #:use-module (guix git-download)
;  #:export (hurd-system?
;            hurd-target?
;            hurd-triplet?))
  )

(define-public xen-0-gnumach
  (package
    (inherit gnumach-headers)
    (name "xen-0-gnumach")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
		      (replace 'install
			       (lambda _
				 (invoke "make" "install-data")))
		      (delete 'build))

       ;; GNU Mach supports only IA32 currently, so cheat so that we can at
       ;; least install its headers.
       ,@(if (%current-target-system)
	     '()
	     ;; See <http://lists.gnu.org/archive/html/bug-hurd/2015-06/msg00042.html>
	     ;; <http://lists.gnu.org/archive/html/guix-devel/2015-06/msg00716.html>
	     '(#:configure-flags '("--build=i586-pc-gnu"
				   "--host=i686-linux-gnu"
				   "--enable-platform=xen")))
       (add-after 'install 'produce-image
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let* ((out  (assoc-ref outputs "out"))
			   (boot (string-append out "/boot")))
		      (invoke "make" "gnumach.gz")
		      (install-file "gnumach.gz" boot)
		      #t)))
       #:tests? #f))
    (native-inputs
     `(("mig" ,mig)
       ("perl" ,perl)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("texinfo" ,texinfo-4)))
    (supported-systems (cons "i686-linux" %hurd-systems))
    (synopsis "Microkernel of the GNU system")
    (description
     "GNU Mach is the microkernel upon which a GNU Hurd system is based.")))

(define unifont
  ;; GNU Unifont, <http://gnu.org/s/unifont>.
  ;; Used the the VGA driver of the Hurd's console client.
  (origin
    (method url-fetch)
    (uri
     "http://unifoundry.com/pub/unifont-7.0.06/font-builds/unifont-7.0.06.bdf.gz")
    (sha256
     (base32
      "0p2vhnc18cnbmb39vq4m7hzv4mhnm2l0a2s7gx3ar277fwng3hys"))))

(define dde-sources
  ;; This is the current tip of the dde branch
  (let ((commit "ac1c7eb7a8b24b7469bed5365be38a968d59a136"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://git.savannah.gnu.org/git/hurd/incubator.git")
            (commit commit)))
      (sha256
       (base32
        "1vryinbg75xpydfrv9dbgfnds6knlh8l8bk2rxp32y9dc58z0692"))
      (file-name (git-file-name "dde" commit)))))
