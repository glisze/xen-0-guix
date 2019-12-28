;;; dvb-0.scm (c) 2019 Gunter Liszewski -- A package module for GNU Guix
;;;  See below for your license

(define-module (gnu packages dvb-0)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages linux))

(define-public w-scan2
  (let ((commit "e55d718a9166e7a91f32abe0c1c89689c2ef2c23")
	(revision "1"))
    (package
      (name "w-scan2")
      (version (git-version "1.0.5" revision commit))
      (source
       (origin
	 (method git-fetch)
	 (uri (git-reference
	       (url "https://github.com/stefantalpalaru/w_scan2.git")
	       (commit commit)))
	 (file-name (git-file-name name version))
	 (sha256
	  (base32
	   "12m4ysqnqzswpkby1nzl7jq5p4xgj7iw6ll1x1mpv3kgsqw6w52j"))))
      (build-system gnu-build-system)
      (inputs
       `(("linux-headers" ,linux-libre-headers)))
      (native-inputs
       `(("autoconf" ,autoconf)
	 ("automake" ,automake)
	 ("libtool"  ,libtool)))
      (arguments
       `(#:tests? #f))
      (home-page "https://github.com/stefantalpalaru/w_scan2")
      (synopsis  "A small channel scan tool")
      (description "@code{w_scan2} is a small channel scan tool which generates
ATSC, DVB-C, DVB-S/S2, and DVB-T/T2 channel conf files. It is based on the old
@code{scan} tool from @code{linuxtv-dvb-apps-1.1.0}.

The differences are: 1. no initial tuning data needed; 2. it detects automatically
which DVB/ATSC card to use; and 3. it has many more output formats to interface with
TV software.

@code{w_scan2} is a fork of the original @code{w_scan} from
@url{http://wirbel.htpc-forum.de/w_scan/index2.html}.

More details here:
@url{https://stefanfalpalaru.wordpress.com/2016/02/04/scan-all-the-things/}.")
      (license license:gpl2))))

	

