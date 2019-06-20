#lang info

(define name "softposit-herbie")
(define deps '("math-lib" "base" "softposit-rkt" ("herbie" #:version "1.3")))
(define pkg-desc "Herbie plugin for posits using the SoftPosit library")
(define version "1.0")
(define pkg-authors '("Pavel Panchekha"))

(define herbie-plugin 'softposit-herbie)
