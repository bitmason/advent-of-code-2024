#!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload :uiop)
(ql:quickload :str)
(sb-ext:save-lisp-and-die "setup.core")
