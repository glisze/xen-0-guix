#!/bin/sh
# two
exec guile -s "$0"  ${1+"$@"}
!#
(display "1..1")(newline)
(display "ok")(newline)
(write (command-line))(newline)
