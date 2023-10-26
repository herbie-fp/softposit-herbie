#lang info

(define name "softposit-herbie")
(define deps '("math-lib" "base" "softposit-rkt" "rival" ("herbie" #:version "2.0")))
(define pkg-desc "Herbie plugin for posits using the SoftPosit library")
(define version "1.0")
(define pkg-authors '("Pavel Panchekha"))

(define herbie-plugin 'softposit-herbie)
