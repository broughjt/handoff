#lang info

(define collection "handoff")
(define version "0.1")

(define deps
  '("base"
    "typed-racket-lib"
    "git+https://github.com/broughjt/toml-racket?path=toml-lib#fix-floats"
    "gregor-lib"
    "tzdata"
    "basedir"))
(define build-deps '("rackunit-typed"))

(define racket-launcher-names '("handoff"))
(define racket-launcher-libraries '("handoff/main.rkt"))
