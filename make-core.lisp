#!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload :uiop)
(ql:quickload :str)
(ql:quickload :cl-ppcre)
(rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))
(sb-ext:save-lisp-and-die "setup.core")
