;;; 20220902 (c) Gunter Liszewski -*- guile -*-
;;; explore more of ledger

(define-module (gnu packages xen-0-ledger)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  ;; #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  ;; #:use-module (guix build-system copy)
  ;; #:use-module (guix build-system emacs)
  ;; #:use-module (guix build-system haskell)
  ;; #:use-module (guix build-system python)
  ;; #:use-module (guix build-system glib-or-gtk)
  ;; #:use-module (guix build-system go)
  ;; #:use-module (guix build-system qt)
  ;; #:use-module (guix deprecation)
  ;; #:use-module (guix gexp)
  ;; #:use-module (guix utils)
  ;; #:use-module (srfi srfi-26)
  ;; #:use-module (gnu packages)
  ;; #:use-module (gnu packages aidc)
  ;; #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
	#:use-module (gnu packages boost)
  ;; #:use-module (gnu packages check)
  ;; #:use-module (gnu packages compression)
  ;; #:use-module (gnu packages crypto)
  ;; #:use-module (gnu packages curl)
  ;; #:use-module (gnu packages databases)
  ;; #:use-module (gnu packages docbook)
  ;; #:use-module (gnu packages documentation)
  ;; #:use-module (gnu packages dns)
  ;; #:use-module (gnu packages emacs)
  ;; #:use-module (gnu packages emacs-xyz)
  ;; #:use-module (gnu packages dbm)
  ;; #:use-module (gnu packages gettext)
  ;; #:use-module (gnu packages glib)
  ;; #:use-module (gnu packages gnome)
  ;; #:use-module (gnu packages gnupg)
  ;; #:use-module (gnu packages golang)
  ;; #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  ;; #:use-module (gnu packages gsasl)
  ;; #:use-module (gnu packages gtk)
  ;; #:use-module (gnu packages haskell-check)
  ;; #:use-module (gnu packages haskell-web)
  ;; #:use-module (gnu packages haskell-xyz)
  ;; #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages libedit)
  ;; #:use-module (gnu packages libevent)
  ;; #:use-module (gnu packages libunwind)
  ;; #:use-module (gnu packages libusb)
  ;; #:use-module (gnu packages linux)
  ;; #:use-module (gnu packages man)
  ;; #:use-module (gnu packages maths)
  ;; #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  ;; #:use-module (gnu packages ncurses)
  ;; #:use-module (gnu packages networking)
  ;; #:use-module (gnu packages pkg-config)
  ;; #:use-module (gnu packages popt)
  ;; #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  ;; #:use-module (gnu packages python-build)
  ;; #:use-module (gnu packages python-crypto)
  ;; #:use-module (gnu packages python-science)
  ;; #:use-module (gnu packages python-web)
  ;; #:use-module (gnu packages python-xyz)
  ;; #:use-module (gnu packages qt)
  ;; #:use-module (gnu packages readline)
  ;; #:use-module (gnu packages sphinx)
  ;; #:use-module (gnu packages tex)
	#:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  ;; #:use-module (gnu packages time)
  ;; #:use-module (gnu packages tls)
  ;; #:use-module (gnu packages upnp)
  ;; #:use-module (gnu packages web)
  ;; #:use-module (gnu packages xml)
  ;; #:use-module (gnu packages gnuzilla))
	)


(define-public ledger-here
  (package
    (name "ledger-here")
    (version "3.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ledger/ledger")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x6jxwss3wwzbzlwmnwb8yzjk8f9wfawif4f1b74z2qg6hc4r7f6"))
       (snippet '(begin
                   ;; Remove test that fails due to difference in
                   ;; reported error message (missing leading "./" in the
                   ;; file name); started some time after Guix commit
                   ;; 727f05e1e285aa52f5a19ec923fdc2259859b4b1
                   (delete-file "test/regress/BF3C1F82-2.test")
                   #true))))
    (build-system cmake-build-system)
    (arguments
     `(#:modules (,@%cmake-build-system-modules
                  ((guix build python-build-system) #:select (python-version)))
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build python-build-system))
       #:configure-flags
       `("-DBUILD_DOCS:BOOL=ON"
         "-DBUILD_WEB_DOCS:BOOL=ON"
         "-DUSE_PYTHON:BOOL=ON"
         "-DCMAKE_INSTALL_LIBDIR:PATH=lib")
       #:phases
       (modify-phases (@ (guix build cmake-build-system) %standard-phases)
         (add-after 'unpack 'fix-python-installation-directory
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; By default the package attempts to install its Python bindings
             ;; to the Python store directory, which obviously does not work.
             ;; Passing -DPython_SITEARCH in #:configure-flags has no effect.
             (let ((python-version (python-version (assoc-ref inputs "python")))
                   (out (assoc-ref outputs "out")))
               (substitute* "src/CMakeLists.txt"
                 (("DESTINATION \\$\\{Python_SITEARCH\\}")
                  (string-append "DESTINATION " out "/lib/python"
                                 python-version "/site-packages")))
               #t)))
         (add-before 'configure 'install-examples
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((examples (string-append (assoc-ref outputs "out")
                                            "/share/doc/ledger/examples")))
               (install-file "test/input/sample.dat" examples)
               (install-file "test/input/demo.ledger" examples))
             #t))
         (add-after 'build 'build-doc
           (lambda _ (invoke "make" "doc")))
         (add-before 'check 'check-setup
           ;; One test fails if it can't set the timezone.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZDIR"
                     (search-input-directory inputs
                                             "share/zoneinfo"))
             ;; Skip failing test BaselineTest_cmd-org.
             ;; This is a known upstream issue. See
             ;; https://github.com/ledger/ledger/issues/550
             (setenv "ARGS" "-E BaselineTest_cmd-org")
             #t)))))
    (inputs
     (list boost
           gmp
           libedit
           mpfr
           python
           utfcpp))
    (native-inputs
     (list groff texinfo tzdata-for-tests))
    (home-page "https://ledger-cli.org/")
    (synopsis "Command-line double-entry accounting program")
    (description
     "Ledger is a powerful, double-entry accounting system that is
accessed from the UNIX command-line.  This may put off some users, since
there is no flashy UI, but for those who want unparalleled reporting
access to their data there are few alternatives.

Ledger uses text files for input.  It reads the files and generates
reports; there is no other database or stored state.  To use Ledger,
you create a file of your account names and transactions, run from the
command line with some options to specify input and requested reports, and
get output.  The output is generally plain text, though you could generate
a graph or html instead.  Ledger is simple in concept, surprisingly rich
in ability, and easy to use.")
    ;; There are some extra licenses in files which do not presently get
    ;; installed when you build this package.  Different versions of the GPL
    ;; are used in the contrib and python subdirectories.  The bundled version
    ;; of utfcpp is under the Boost 1.0 license. Also the file
    ;; `tools/update_copyright_year` has an Expat license.
    (license (list license:bsd-3
                   license:asl2.0     ; src/strptime.cc
                   (license:non-copyleft
                    "file://src/wcwidth.cc"
                    "See src/wcwidth.cc in the distribution.")))))
