;;; xen-0-base (c) 2019 Gunter Liszewski
;;;  guix environment --load-path=here --ad-hoc xen-0-hello

;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
;;; Copyright © 2014, 2015 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2016, 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages xen-0-hello)
  #:use-module ((guix licenses)
                #:select (gpl3+ lgpl2.0+ lgpl3+ public-domain))
  #:use-module (gnu packages)
;;;  #:use-module (gnu packages acl)
;;;  #:use-module (gnu packages algebra)
;;;  #:use-module (gnu packages bash)
;;;  #:use-module (gnu packages bison)
;;;  #:use-module (gnu packages ed)
;;;  #:use-module (gnu packages gcc)
;;;  #:use-module (gnu packages guile)
;;;  #:use-module (gnu packages multiprecision)
;;;  #:use-module (gnu packages compression)
;;;  #:use-module (gnu packages perl)
;;;  #:use-module (gnu packages linux)
;;;  #:use-module (gnu packages pcre)
;;;  #:use-module (gnu packages texinfo)
;;;  #:use-module (gnu packages hurd)
;;;  #:use-module (gnu packages pkg-config)
;;;  #:use-module (gnu packages gettext)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))
;;;  #:export (glibc
;;;            libiconv-if-needed)

;;; Commentary:
;;;
;;; Packages of the Guix-based GNU user-land software distribution.
;;;
;;; For example: using conventional language for these things here.
;;;
;;; Code:

(define-public xen-0-hello
  (package
    (name "xen-0-hello")
    (version "2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/hello/hello-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))))
    (build-system gnu-build-system)
    (synopsis "Hello, GNU world, please meet Xen: An example GNU package")
    (description
     "GNU Hello prints the message \"Hello, friends!\" and then exits.  It
serves as an example of standard GNU coding practices.  As such, it supports
command-line arguments, multiple languages, and so on.")
    (home-page "https://www.gnu.org/software/hello/")
    (license gpl3+)))

;;; Commentar:
;;;
;;; For example: a package with two outputs
;;;
;;; Code:

#;(define-public xen-0-coreutils
  (package
   (name "xen-0-coreutils")
   (version "8.30")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/coreutils/coreutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0mxhw43d4wpqmvg0l4znk1vm10fy92biyh90lzdnqjcic2lb6cg8"))))
   (build-system gnu-build-system)
   (inputs `(("acl"  ,acl)                        ; TODO: add SELinux
             ("gmp"  ,gmp)                        ;bignums in 'expr', yay!

             ;; Drop the dependency on libcap when cross-compiling since it's
             ;; not quite cross-compilable.
             ,@(if (%current-target-system)
                   '()
                   `(("libcap" ,libcap)))))  ;capability support is 'ls', etc.
   (native-inputs
    ;; Perl is needed to run tests in native builds, and to run the bundled
    ;; copy of help2man.  However, don't pass it when cross-compiling since
    ;; that would lead it to try to run programs to get their '--help' output
    ;; for help2man.
    (if (%current-target-system)
        '()
        `(("perl" ,perl))))
   (outputs '("out" "debug"))
   (arguments
    `(#:parallel-build? #f            ; help2man may be called too early
      #:phases (modify-phases %standard-phases
                 (add-before 'build 'patch-shell-references
                   (lambda _
                     ;; 'split' uses either $SHELL or /bin/sh.  Set $SHELL so
                     ;; that tests pass, since /bin/sh isn't in the chroot.
                     (setenv "SHELL" (which "sh"))

                     (substitute* (find-files "gnulib-tests" "\\.c$")
                       (("/bin/sh") (which "sh")))
                     (substitute* (find-files "tests" "\\.sh$")
                       (("#!/bin/sh") (string-append "#!" (which "sh"))))
                     #t))
                 (add-before 'check 'disable-broken-test
                   (lambda _
                     ;; This test hits the 127 character shebang limit in the build
                     ;; environment due to the way "env -S" splits arguments into
                     ;; shebangs.  Note that "env-S-script.sh" works around this
                     ;; specific issue, but "env-S.pl" is not adjusted for build
                     ;; environments with long prefixes (/tmp/guix-build-...).
                     (substitute* "Makefile"
                       (("^.*tests/misc/env-S.pl.*$") ""))
                     #t)))

      ;; Work around a cross-compilation bug whereby libcoreutils.a would
      ;; provide '__mktime_internal', which conflicts with the one in libc.a.
      ,@(if (%current-target-system)
            `(#:configure-flags '("gl_cv_func_working_mktime=yes"))
            '())))
   (synopsis "Core GNU utilities (file, text, shell)")
   (description
    "GNU Coreutils includes all of the basic command-line tools that are
expected in a POSIX system.  These provide the basic file, shell and text
manipulation functions of the GNU system.  Most of these tools offer extended
functionality beyond that which is outlined in the POSIX standard.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/coreutils/")))

#;(define-public xen-0-coreutils-minimal
  ;; Coreutils without its optional dependencies.
  (package
    (inherit xen-0-coreutils)
    (name "xen-0-coreutils-minimal")
    (outputs '("out"))
    (inputs '())))
