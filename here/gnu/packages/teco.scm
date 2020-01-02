;;; teco-0.scm (c) 2019 Gunter Liszewski -- Package module for GNU Guix
;;; GPL3

(define-module (gnu packages teco)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ncurses)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public teco
  (package
    (name "teco")
    (version "0.0.72-b0a1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/glisze/teco.git")
                    (commit "deploy/v0.0.72-b0a1")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kmnyy687dzyk1zbilgfwnanhjfgnzdpcrzh6ycxc4j6ndvsmsan"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             (for-each (lambda (file)
                         (patch-shebang file))
                       (find-files "./scripts/"))
             (invoke "sh" "-c" "echo 0.0.72-b0a1 > ./VERSION")
             (invoke "sh" "-c" "touch ./ChangeLog")
             (invoke
              "sh" "-c"
              "sed NEWS.in -e 1s,@VERSION[@],$( cat ./VERSION ), > NEWS")
             (invoke "sh" "-c" "autoreconf -vif"))))))
    (propagated-inputs
     `(("ncurses" ,ncurses)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("perl" ,perl)
       ("git" ,git)
       ("texinfo" ,texinfo)
       ("pkg-config" ,pkg-config)))
    (synopsis "Text Editor and COrrector program")
    (description
     "From the ibiblio archive of the @code{TECOC-146} program.

This @code{teco} program has only the minimal changes for it to built.
Please, use it with due care!")
    (home-page "https://glisze.github.com/teco")
    (license gpl3+)))

(define-public teco-local
  (package
    (inherit teco)
    (name "teco-local")
    (version "0.0.72-b0a1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "file://home/gunter/source-5/guix/a/teco.git")
                    (commit "deploy/v0.0.72-b0a1")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kmnyy687dzyk1zbilgfwnanhjfgnzdpcrzh6ycxc4j6ndvsmsan"))))))

