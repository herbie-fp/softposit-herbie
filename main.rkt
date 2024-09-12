#lang racket

(require math/flonum
         math/bigfloat
         softposit-rkt
         herbie/plugin
         rival)

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(eprintf "Loading posits support...\n")

(define posit8-max (ordinal->posit8 (- (expt 2 (- 8 1)) 1)))
(define posit16-max (ordinal->posit16 (- (expt 2 (- 16 1)) 1)))
(define posit32-max (ordinal->posit32 (- (expt 2 (- 32 1)) 1)))

;; Max quire is difficult to compute:
;;
;; A quire is a 2s-complement signed fixed-point number with
;;  - `000...000` representing 0,
;;  - `100...000` representing NAR.
;; Unlike traditional fixed-point numbers, quires are actually symmetric.
;; For an `n`-bit posit, the associated quire has bitwidth `16 * n`
;; with a scale of `2^(16 - 8*n)`.
;;
;;   posit   quire size         max value               nearest double w/ epsilon
;;  -------|------------|----------------------|------------------------------------------|
;;   8       128         (2^(127) - 1) * 2^-48   2^79 - 2^-48   (6.044629098073146e+23)
;;   16      256         (2^(255) - 1) * 2^-112  2^143 - 2^-112  (1.1150372599265312e+43)
;;   32      512         (2^(511) - 1) * 2^-240  2^270 - 2^-240  (3.794275180128377e+81)
;;
;; Unfortunately, we don't have a good way to convert doubles to quire.
;; The libsoftposit library only has double to posit; the Racket shim
;; incorrectly composes double-posit, posit-quire conversions.
;;
(define quire8-max (quire8-fdp-add (double->quire8 0.0) posit8-max posit8-max))
(define quire8-nmax (quire8-fdp-sub (double->quire8 0.0) posit8-max posit8-max))
(define quire16-max (quire16-fdp-add (double->quire16 0.0) posit16-max posit16-max))
(define quire16-nmax (quire16-fdp-sub (double->quire16 0.0) posit16-max posit16-max))
; These crash
; (define quire32-max (quire32-fdp-add (double->quire32 0.0) posit32-max posit32-max))
; (define quire32-nmax (quire32-fdp-sub (double->quire32 0.0) posit32-max posit32-max))

(define (bf-inf->nan x)
  (let ([y (bf x)]) (if (bfinfinite? y) +nan.bf y)))

(define (double->posit8* x)
  (let ([y (double->posit8 x)])
    (if (posit8= y (posit8-nar)) (if (> x 0) posit8-max (posit8-neg posit8-max)) y)))
(define (double->posit16* x)
  (let ([y (double->posit16 x)])
    (if (posit16= y (posit16-nar)) (if (> x 0) posit16-max (posit16-neg posit16-max)) y)))
(define (double->posit32* x)
  (let ([y (double->posit32 x)])
    (if (posit32= y (posit32-nar)) (if (> x 0) posit32-max (posit32-neg posit32-max)) y)))
(define (double->quire8* x)
  (let ([y (double->quire8 x)])
    (if (infinite? (quire8->double y)) (if (> x 0) quire8-max quire8-nmax) y)))
(define (double->quire16* x)
  (let ([y (double->quire16 x)])
    (if (infinite? (quire16->double y)) (if (> x 0) quire16-max quire16-nmax) y)))
#;(define (double->quire32* x)
    (let ([y (double->quire32 x)])
      (if (infinite? (quire32->double y)) (if (> x 0) quire32-max quire32-nmax) y)))

;; Defining the representations

(define-representation (posit8 real posit8?)
                       (compose double->posit8* bigfloat->flonum)
                       (compose bf-inf->nan posit8->double)
                       (shift 7 ordinal->posit8)
                       (unshift 7 posit8->ordinal)
                       8
                       (curry posit8= (posit8-nar)))

(define-representation (posit16 real posit16?)
                       (compose double->posit16* bigfloat->flonum)
                       (compose bf-inf->nan posit16->double)
                       (shift 15 ordinal->posit16)
                       (unshift 15 posit16->ordinal)
                       16
                       (curry posit16= (posit16-nar)))

(define-representation (posit32 real posit32?)
                       (compose double->posit32* bigfloat->flonum)
                       (compose bf-inf->nan posit32->double)
                       (shift 31 ordinal->posit32)
                       (unshift 31 posit32->ordinal)
                       32
                       (curry posit32= (posit32-nar)))

;;TODO correct functions for quire (incorrect now for testing)
(define-representation (quire8 real quire8?)
                       (compose double->quire8* bigfloat->flonum)
                       (compose bf-inf->nan quire8->double)
                       (compose double->quire8 ordinal->flonum)
                       (compose flonum->ordinal quire8->double)
                       64
                       (const #f))

(define-representation (quire16 real quire16?)
                       (compose double->quire16* bigfloat->flonum)
                       (compose bf-inf->nan quire16->double)
                       (compose double->quire16 ordinal->flonum)
                       (compose flonum->ordinal quire16->double)
                       64
                       (const #f))

(define-representation (quire32 real quire32?)
                       (compose double->quire32
                                bigfloat->flonum) ; TODO: use double->quire32* when crash fixed
                       (compose bf-inf->nan quire32->double)
                       (compose double->quire32 ordinal->flonum)
                       (compose flonum->ordinal quire32->double)
                       64
                       (const #f))

;; Defining the operators

(define-operator-impl
 (+.p8 [x : posit8] [y : posit8])
 posit8
 #:spec (+ x y)
 #:fpcore (! :precision posit8 (+ x y))
 #:fl posit8-add
 #:identities (#:commutes [distribute-neg-out (+.p8 (neg.p8 a) (neg.p8 b)) (neg.p8 (+.p8 a b))]
                          [+-lft-identity (+.p8 0 a) a]
                          [+-rgt-identity (+.p8 a 0) a]
                          [unsub-neg (+.p8 a (neg.p8 b)) (-.p8 a b)]))

(define-operator-impl
 (+.p16 [x : posit16] [y : posit16])
 posit16
 #:spec (+ x y)
 #:fpcore (! :precision posit16 (+ x y))
 #:fl posit16-add
 #:identities (#:commutes [distribute-neg-out (+.p16 (neg.p16 a) (neg.p16 b)) (neg.p16 (+.p16 a b))]
                          [+-lft-identity (+.p16 0 a) a]
                          [+-rgt-identity (+.p16 a 0) a]
                          [unsub-neg (+.p16 a (neg.p16 b)) (-.p16 a b)]))

(define-operator-impl
 (+.p32 [x : posit32] [y : posit32])
 posit32
 #:spec (+ x y)
 #:fpcore (! :precision posit32 (+ x y))
 #:fl posit32-add
 #:identities (#:commutes [distribute-neg-out (+.p32 (neg.p32 a) (neg.p32 b)) (neg.p32 (+.p32 a b))]
                          [+-lft-identity (+.p32 0 a) a]
                          [+-rgt-identity (+.p32 a 0) a]
                          [unsub-neg (+.p32 a (neg.p32 b)) (-.p32 a b)]))

(define-operator-impl (neg.p8 [x : posit8])
                      posit8
                      #:spec (neg x)
                      #:fpcore (! :precision posit8 (- x))
                      #:fl posit8-neg
                      #:identities
                      (#:exact (neg.p8 x)
                               [distribute-lft-neg-in (neg.p8 (*.p8 a b)) (*.p8 (neg.p8 a) b)]
                               [distribute-rgt-neg-in (neg.p8 (* a b)) (*.p8 a (neg.p8 b))]
                               [distribute-neg-in (neg.p8 (+.p8 a b)) (+.p8 (neg.p8 a) (neg.p8 b))]
                               [distribute-neg-frac (neg.p8 (/.p8 a b)) (/.p8 (neg.p8 a) b)]
                               [distribute-neg-frac2 (neg.p8 (/.p8 a b)) (/.p8 a (neg.p8 b))]
                               [remove-double-neg (neg.p8 (neg.p8 a)) a]
                               [neg-sub0 (neg.p8 b) (-.p8 0 b)]
                               [neg-mul-1 (neg.p8 a) (*.p8 -1 a)]))

(define-operator-impl
 (neg.p16 [x : posit16])
 posit16
 #:spec (neg x)
 #:fpcore (! :precision posit16 (- x))
 #:fl posit16-neg
 #:identities (#:exact (neg.p16 x)
                       [distribute-lft-neg-in (neg.p16 (*.p16 a b)) (*.p16 (neg.p16 a) b)]
                       [distribute-rgt-neg-in (neg.p16 (*.p16 a b)) (*.p16 a (neg.p16 b))]
                       [distribute-neg-in (neg.p16 (+.p16 a b)) (+.p16 (neg.p16 a) (neg.p16 b))]
                       [distribute-neg-frac (neg.p16 (/.p16 a b)) (/.p16 (neg.p16 a) b)]
                       [distribute-neg-frac2 (neg.p16 (/.p16 a b)) (/.p16 a (neg.p16 b))]
                       [remove-double-neg (neg.p16 (neg.p16 a)) a]
                       [neg-sub0 (neg.p16 b) (-.p16 0 b)]
                       [neg-mul-1 (neg.p16 a) (*.p16 -1 a)]))

(define-operator-impl
 (neg.p32 [x : posit32])
 posit32
 #:spec (neg x)
 #:fpcore (! :precision posit32 (- x))
 #:fl posit32-neg
 #:identities (#:exact (neg.p32 x)
                       [distribute-lft-neg-in (neg.p32 (*.p32 a b)) (*.p32 (neg.p32 a) b)]
                       [distribute-rgt-neg-in (neg.p32 (*.p32 a b)) (*.p32 a (neg.p32 b))]
                       [distribute-neg-in (neg.p32 (+.p32 a b)) (+.p32 (neg.p32 a) (neg.p32 b))]
                       [distribute-neg-frac (neg.p32 (/.p32 a b)) (/.p32 (neg.p32 a) b)]
                       [distribute-neg-frac2 (neg.p32 (/.p32 a b)) (/.p32 a (neg.p32 b))]
                       [remove-double-neg (neg.p32 (neg.p32 a)) a]
                       [neg-sub0 (neg.p32 b) (-.p32 0 b)]
                       [neg-mul-1 (neg.p32 a) (*.p32 -1 a)]))

(define-operator-impl (-.p8 [x : posit8] [y : posit8])
                      posit8
                      #:spec (- x y)
                      #:fpcore (! :precision posit8 (- x y))
                      #:fl posit8-sub
                      #:identities
                      ([cancel-sign-sub (-.p8 a (*.p8 (neg b) c)) (+.p8 a (*.p8 b c))]
                       [cancel-sign-sub-inv (-.p8 a (*.p8 b c)) (+.p8 a (*.p8 (neg b) c))]
                       [+-inverses (-.p8 a a) 0]
                       [--rgt-identity (-.p8 a 0) a]
                       [sub0-neg (-.p8 0 a) (neg.p8 a)]
                       [sub-neg (-.p8 a b) (+.p8 a (neg.p8 b))]))

(define-operator-impl (-.p16 [x : posit16] [y : posit16])
                      posit16
                      #:spec (- x y)
                      #:fpcore (! :precision posit16 (- x y))
                      #:fl posit16-sub
                      #:identities
                      ([cancel-sign-sub (-.p16 a (*.p16 (neg.p16 b) c)) (+.p16 a (*.p16 b c))]
                       [cancel-sign-sub-inv (-.p16 a (*.p16 b c)) (+.p16 a (*.p16 (neg.p16 b) c))]
                       [+-inverses (-.p16 a a) 0]
                       [--rgt-identity (-.p16 a 0) a]
                       [sub0-neg (-.p16 0 a) (neg.p16 a)]
                       [sub-neg (-.p16 a b) (+.p16 a (neg.p16 b))]))

(define-operator-impl (-.p32 [x : posit32] [y : posit32])
                      posit32
                      #:spec (- x y)
                      #:fpcore (! :precision posit32 (- x y))
                      #:fl posit32-sub
                      #:identities
                      ([cancel-sign-sub (-.p32 a (*.p32 (neg.p32 b) c)) (+.p32 a (*.p32 b c))]
                       [cancel-sign-sub-inv (-.p32 a (*.p32 b c)) (+.p32 a (*.p32 (neg b) c))]
                       [+-inverses (-.p32 a a) 0]
                       [--rgt-identity (-.p32 a 0) a]
                       [sub0-neg (-.p32 0 a) (neg.p32 a)]
                       [sub-neg (-.p32 a b) (+.p32 a (neg.p32 b))]))

(define-operator-impl (*.p8 [x : posit8] [y : posit8])
                      posit8
                      #:spec (* x y)
                      #:fpcore (! :precision posit8 (* x y))
                      #:fl posit8-mul
                      #:identities
                      (#:commutes [distribute-lft-neg-out (*.p8 (neg.p8 x) y) (neg.p8 (*.p8 x y))]
                                  [distribute-rgt-neg-out (*.p8 x (neg.p8 y)) (neg.p8 (*.p8 x y))]
                                  [mul0-lft (*.p8 0 a) 0]
                                  [mul0-rgt (*.p8 a 0) 0]
                                  [*-lft-identity (*.p8 1 a) a]
                                  [*-rgt-identity (*.p8 a 1) a]
                                  [mul-1-neg (*.p8 -1 a) (neg.p8 a)]
                                  [*-un-lft-identity a (*.p8 1 a)]
                                  [sqr-neg (*.p8 (neg.p8 x) (neg x)) (*.p8 x x)]))

(define-operator-impl (*.p16 [x : posit16] [y : posit16])
                      posit16
                      #:spec (* x y)
                      #:fpcore (! :precision posit16 (* x y))
                      #:fl posit16-mul
                      #:identities
                      (#:commutes [distribute-lft-neg-out (*.p16 (neg.p16 x) y) (neg.p16 (*.p16 x y))]
                                  [distribute-rgt-neg-out (*.p16 x (neg.p16 y)) (neg.p16 (*.p16 x y))]
                                  [mul0-lft (*.p16 0 a) 0]
                                  [mul0-rgt (*.p16 a 0) 0]
                                  [*-lft-identity (*.p16 1 a) a]
                                  [*-rgt-identity (*.p16 a 1) a]
                                  [mul-1-neg (*.p16 -1 a) (neg.p16 a)]
                                  [*-un-lft-identity a (*.p16 1 a)]
                                  [sqr-neg (*.p16 (neg.p16 x) (neg.p16 x)) (*.p16 x x)]))

(define-operator-impl (*.p32 [x : posit32] [y : posit32])
                      posit32
                      #:spec (* x y)
                      #:fpcore (! :precision posit32 (* x y))
                      #:fl posit32-mul
                      #:identities
                      (#:commutes [distribute-lft-neg-out (*.p32 (neg.p32 x) y) (neg.p32 (*.p32 x y))]
                                  [distribute-rgt-neg-out (*.p32 x (neg.p32 y)) (neg.p32 (*.p32 x y))]
                                  [mul0-lft (*.p32 0 a) 0]
                                  [mul0-rgt (*.p32 a 0) 0]
                                  [*-lft-identity (*.p32 1 a) a]
                                  [*-rgt-identity (*.p32 a 1) a]
                                  [mul-1-neg (*.p32 -1 a) (neg.p32 a)]
                                  [*-un-lft-identity a (*.p32 1 a)]
                                  [sqr-neg (*.p32 (neg.p32 x) (neg.p32 x)) (*.p32 x x)]))

(define-operator-impl (/.p8 [x : posit8] [y : posit8])
                      posit8
                      #:spec (/ x y)
                      #:fpcore (! :precision posit8 (/ x y))
                      #:fl posit8-div
                      #:identities ([distribute-frac-neg (/.p8 (neg.p8 x) y) (neg.p8 (/.p8 x y))]
                                    [distribute-frac-neg2 (/.p8 x (neg.p8 y)) (neg.p8 (/.p8 x y))]
                                    [div0 (/.p8 0 a) 0]
                                    [*-inverses (/.p8 a a) 1]
                                    [/-rgt-identity (/.p8 a 1) a]))

(define-operator-impl (/.p16 [x : posit16] [y : posit16])
                      posit16
                      #:spec (/ x y)
                      #:fpcore (! :precision posit16 (/ x y))
                      #:fl posit16-div
                      #:identities ([distribute-frac-neg (/.p16 (neg.p16 x) y) (neg.p16 (/.p16 x y))]
                                    [distribute-frac-neg2 (/.p16 x (neg.p16 y)) (neg.p16 (/.p16 x y))]
                                    [div0 (/.p16 0 a) 0]
                                    [*-inverses (/.p16 a a) 1]
                                    [/-rgt-identity (/.p16 a 1) a]))

(define-operator-impl (/.p32 [x : posit32] [y : posit32])
                      posit32
                      #:spec (/ x y)
                      #:fpcore (! :precision posit32 (/ x y))
                      #:fl posit32-div
                      #:identities ([distribute-frac-neg (/.p32 (neg.p32 x) y) (neg.p32 (/.p32 x y))]
                                    [distribute-frac-neg2 (/.p32 x (neg.p32 y)) (neg.p32 (/.p32 x y))]
                                    [div0 (/.p32 0 a) 0]
                                    [*-inverses (/.p32 a a) 1]
                                    [/-rgt-identity (/.p32 a 1) a]))

(define-operator-impl (sqrt.p8 [x : posit8])
                      posit8
                      #:spec (sqrt x)
                      #:fpcore (! :precision posit8 (sqrt x))
                      #:fl posit8-sqrt)

(define-operator-impl (sqrt.p16 [x : posit16])
                      posit16
                      #:spec (sqrt x)
                      #:fpcore (! :precision posit16 (sqrt x))
                      #:fl posit16-sqrt)

(define-operator-impl (sqrt.p32 [x : posit32])
                      posit32
                      #:spec (sqrt x)
                      #:fpcore (! :precision posit32 (sqrt x))
                      #:fl posit32-sqrt)

(define-operator-impl (==.p8 [x : posit8] [y : posit8]) bool #:spec (== x y) #:fl posit8=)

(define-operator-impl (==.p16 [x : posit16] [y : posit16]) bool #:spec (== x y) #:fl posit16=)

(define-operator-impl (==.p32 [x : posit32] [y : posit32]) bool #:spec (== x y) #:fl posit32=)

(define-operator-impl (!=.p8 [x : posit8] [y : posit8]) bool #:spec (!= x y) #:fl (negate posit8=))

(define-operator-impl (!=.p16 [x : posit16] [y : posit16])
                      bool
                      #:spec (!= x y)
                      #:fl (negate posit16=))

(define-operator-impl (!=.p32 [x : posit32] [y : posit32])
                      bool
                      #:spec (!= x y)
                      #:fl (negate posit32=))

(define-operator-impl (<.p8 [x : posit8] [y : posit8]) bool #:spec (< x y) #:fl posit8<)

(define-operator-impl (<.p16 [x : posit16] [y : posit16]) bool #:spec (< x y) #:fl posit16<)

(define-operator-impl (<.p32 [x : posit32] [y : posit32]) bool #:spec (< x y) #:fl posit32<)

(define-operator-impl (>.p8 [x : posit8] [y : posit8]) bool #:spec (> x y) #:fl posit8>)

(define-operator-impl (>.p16 [x : posit16] [y : posit16]) bool #:spec (> x y) #:fl posit16>)

(define-operator-impl (>.p32 [x : posit32] [y : posit32]) bool #:spec (> x y) #:fl posit32>)

(define-operator-impl (<=.p8 [x : posit8] [y : posit8]) bool #:spec (<= x y) #:fl posit8<=)

(define-operator-impl (<=.p16 [x : posit16] [y : posit16]) bool #:spec (<= x y) #:fl posit16<=)

(define-operator-impl (<=.p32 [x : posit32] [y : posit32]) bool #:spec (<= x y) #:fl posit32<=)

(define-operator-impl (>=.p8 [x : posit8] [y : posit8]) bool #:spec (>= x y) #:fl posit8>=)

(define-operator-impl (>=.p16 [x : posit16] [y : posit16]) bool #:spec (>= x y) #:fl posit16>=)

(define-operator-impl (>=.p32 [x : posit32] [y : posit32]) bool #:spec (>= x y) #:fl posit32>=)

;; Posit/float implementations

(define-operator-impl (binary64->posit8 [x : binary64])
                      posit8
                      #:spec x
                      #:fpcore (! :precision posit8 (cast x))
                      #:fl double->posit8)

(define-operator-impl (binary64->posit16 [x : binary64])
                      posit16
                      #:spec x
                      #:fpcore (! :precision posit16 (cast x))
                      #:fl double->posit16)

(define-operator-impl (binary64->posit32 [x : binary64])
                      posit32
                      #:spec x
                      #:fpcore (! :precision posit32 (cast x))
                      #:fl double->posit32)

(define-operator-impl (posit8->binary64 [x : posit8])
                      binary64
                      #:spec x
                      #:fpcore (! :precision binary64 (cast x))
                      #:fl posit8->double)

(define-operator-impl (posit16->binary64 [x : posit16])
                      binary64
                      #:spec x
                      #:fpcore (! :precision binary64 (cast x))
                      #:fl posit16->double)

(define-operator-impl (posit32->binary64 [x : posit32])
                      binary64
                      #:spec x
                      #:fpcore (! :precision binary64 (cast x))
                      #:fl posit32->double)

;; Quire/float implementations

(define-operator-impl (binary64->quire8 [x : binary64])
                      quire8
                      #:spec x
                      #:fpcore (! :precision quire8 (cast x))
                      #:fl double->quire8)

(define-operator-impl (binary64->quire16 [x : binary64])
                      quire16
                      #:spec x
                      #:fpcore (! :precision quire16 (cast x))
                      #:fl double->quire16)

(define-operator-impl (binary64->quire32 [x : binary64])
                      quire32
                      #:spec x
                      #:fpcore (! :precision quire32 (cast x))
                      #:fl double->quire32)

(define-operator-impl (quire8->binary64 [x : quire8])
                      binary64
                      #:spec x
                      #:fpcore (! :precision binary64 (cast x))
                      #:fl quire8->double)

(define-operator-impl (quire16->binary64 [x : quire16])
                      binary64
                      #:spec x
                      #:fpcore (! :precision binary64 (cast x))
                      #:fl quire16->double)

(define-operator-impl (quire32->binary64 [x : quire32])
                      binary64
                      #:spec x
                      #:fpcore (! :precision binary64 (cast x))
                      #:fl quire32->double)

;; Quire/posit fused ops

;; Quire/posit fused impl
(define-operator-impl (quire8-mul-add [x : quire8] [y : posit8] [z : posit8])
                      quire8
                      #:spec (+ x (* y z))
                      #:fpcore (! :precision quire8 (fdp x y z))
                      #:fl quire8-fdp-add)

(define-operator-impl (quire16-mul-add [x : quire16] [y : posit16] [z : posit16])
                      quire16
                      #:spec (+ x (* y z))
                      #:fpcore (! :precision quire16 (fdp x y z))
                      #:fl quire16-fdp-add)

(define-operator-impl (quire32-mul-add [x : quire32] [y : posit32] [z : posit32])
                      quire32
                      #:spec (+ x (* y z))
                      #:fpcore (! :precision quire32 (fdp x y z))
                      #:fl quire32-fdp-add)

(define-operator-impl (quire8-mul-sub [x : quire8] [y : posit8] [z : posit8])
                      quire8
                      #:spec (- x (* y z))
                      #:fpcore (! :precision quire8 (fdm x y z))
                      #:fl quire8-fdp-sub)

(define-operator-impl (quire16-mul-sub [x : quire16] [y : posit16] [z : posit16])
                      quire16
                      #:spec (- x (* y z))
                      #:fpcore (! :precision quire16 (fdm x y z))
                      #:fl quire16-fdp-sub)

(define-operator-impl (quire32-mul-sub [x : quire32] [y : posit32] [z : posit32])
                      quire32
                      #:spec (- x (* y z))
                      #:fpcore (! :precision quire32 (fdm x y z))
                      #:fl quire32-fdp-sub)

;; Quire/posit impl

(define-operator-impl (quire8->posit8 [x : quire8])
                      posit8
                      #:spec x
                      #:fpcore (! :precision posit8 (cast x))
                      #:fl quire8->posit8)

(define-operator-impl (quire16->posit16 [x : quire16])
                      posit16
                      #:spec x
                      #:fpcore (! :precision posit16 (cast x))
                      #:fl quire16->posit16)

(define-operator-impl (quire32->posit32 [x : quire32])
                      posit32
                      #:spec x
                      #:fpcore (! :precision posit32 (cast x))
                      #:fl quire32->posit32)

(define-operator-impl (posit8->quire8 [x : posit8])
                      quire8
                      #:spec x
                      #:fpcore (! :precision quire8 (cast x))
                      #:fl posit8->quire8)

(define-operator-impl (posit16->quire16 [x : posit16])
                      quire16
                      #:spec x
                      #:fpcore (! :precision quire16 (cast x))
                      #:fl posit16->quire16)

(define-operator-impl (posit32->quire32 [x : posit32])
                      quire32
                      #:spec x
                      #:fpcore (! :precision quire32 (cast x))
                      #:fl posit32->quire32)

;; Defining the rules

(define-ruleset commutativity.p16
                (arithmetic simplify posit)
                #:type ([a posit16] [b posit16])
                [+-commutative.p16 (+.p16 a b) (+.p16 b a)]
                [*-commutative.p16 (*.p16 a b) (*.p16 b a)])

; Posit conversions
(define-ruleset insert-p16
                (arithmetic posit)
                #:type ([a binary64])
                [insert-posit16 a (posit16->binary64 (binary64->posit16 a))])

(define-ruleset remove-p16
                (arithmetic simplify posit)
                #:type ([a binary64])
                [remove-posit16 (posit16->binary64 (binary64->posit16 a)) a])

;; TODO: Multiply add to mulAdd

;; TODO: We only cast back to posit after quire operations because herbie can't handle
;; non-double output right now (similar situtation for posits)
(define-ruleset q16-arithmetic
                (arithmetic posit)
                #:type ([a posit16] [b posit16] [c posit16] [q quire16])
                [introduce-quire a (quire16->posit16 (posit16->quire16 a))]
                [insert-quire-add
                 (+.p16 (quire16->posit16 q) a)
                 (quire16->posit16 (quire16-mul-add q a (binary64->posit16 1.0)))]
                [insert-quire-sub
                 (-.p16 (quire16->posit16 q) a)
                 (quire16->posit16 (quire16-mul-sub q a (binary64->posit16 1.0)))]
                [insert-quire-fdp-add
                 (+.p16 (quire16->posit16 q) (*.p16 a b))
                 (quire16->posit16 (quire16-mul-add q a b))]
                [insert-quire-fdp-sub
                 (-.p16 (quire16->posit16 q) (*.p16 a b))
                 (quire16->posit16 (quire16-mul-sub q a b))])
