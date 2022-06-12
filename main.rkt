#lang racket

(require math/flonum math/bigfloat softposit-rkt herbie/plugin rival)

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

;; TODO: not actually correct, it's bigger than this but hard to compute
(define quire8-max (quire8-fdp-add (double->quire8 0.0) posit8-max posit8-max))
(define quire8-nmax (quire8-fdp-sub (double->quire8 0.0) posit8-max posit8-max))
(define quire16-max (quire16-fdp-add (double->quire16 0.0) posit16-max posit16-max))
(define quire16-nmax (quire16-fdp-sub (double->quire16 0.0) posit16-max posit16-max))
; These crash
; (define quire32-max (quire32-fdp-add (double->quire32 0.0) posit32-max posit32-max))
; (define quire32-nmax (quire32-fdp-sub (double->quire32 0.0) posit32-max posit32-max))

(define (bf-inf->nan x) (let ([y (bf x)]) (if (bfinfinite? y) +nan.bf y)))

(define (double->posit8* x)
  (let ([y (double->posit8 x)])
    (if (posit8= y (posit8-nar))
        (if (> x 0) posit8-max (posit8-neg posit8-max))
        y)))
(define (double->posit16* x)
  (let ([y (double->posit16 x)])
    (if (posit16= y (posit16-nar))
        (if (> x 0) posit16-max (posit16-neg posit16-max))
        y)))
(define (double->posit32* x)
  (let ([y (double->posit32 x)])
    (if (posit32= y (posit32-nar))
        (if (> x 0) posit32-max (posit32-neg posit32-max))
        y)))
(define (double->quire8* x)
  (let ([y (double->quire8 x)])
    (if (infinite? (quire8->double y))
        (if (> x 0) quire8-max quire8-nmax)
        y)))
(define (double->quire16* x)
  (let ([y (double->quire16 x)])
    (if (infinite? (quire16->double y))
        (if (> x 0) quire16-max quire16-nmax)
        y)))
#;(define (double->quire32* x)
  (let ([y (double->quire32 x)])
    (if (infinite? (quire32->double y))
        (if (> x 0) quire32-max quire32-nmax)
        y)))

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
  (compose double->quire32 bigfloat->flonum) ; TODO: use double->quire32* when crash fixed
  (compose bf-inf->nan quire32->double)
  (compose double->quire32 ordinal->flonum)
  (compose flonum->ordinal quire32->double)
  64
  (const #f))

;; Defining the operators

(define-operator-impl (+ +.p8 posit8 posit8) posit8
  [fl posit8-add])

(define-operator-impl (+ +.p16 posit16 posit16) posit16
  [fl posit16-add])

(define-operator-impl (+ +.p32 posit32 posit32) posit32
  [fl posit32-add])

(define-operator-impl (neg neg.p8 posit8) posit8
  [fl posit8-neg])

(define-operator-impl (neg neg.p16 posit16) posit16
  [fl posit16-neg])

(define-operator-impl (neg neg.p32 posit32) posit32
  [fl posit32-neg])

(define-operator-impl (- -.p8 posit8 posit8) posit8
  [fl posit8-sub])

(define-operator-impl (- -.p16 posit16 posit16) posit16
  [fl posit16-sub])

(define-operator-impl (- -.p32 posit32 posit32) posit32
  [fl posit32-sub])

(define-operator-impl (* *.p8 posit8 posit8) posit8
  [fl posit8-mul])

(define-operator-impl (* *.p16 posit16 posit16) posit16
  [fl posit16-mul])

(define-operator-impl (* *.p32 posit32 posit32) posit32
  [fl posit32-mul])

(define-operator-impl (/ /.p8 posit8 posit8) posit8
  [fl posit8-div])

(define-operator-impl (/ /.p16 posit16 posit16) posit16
  [fl posit16-div])

(define-operator-impl (/ /.p32 posit32 posit32) posit32
  [fl posit32-div])

(define-operator-impl (sqrt sqrt.p8 posit8) posit8
  [fl posit8-sqrt])

(define-operator-impl (sqrt sqrt.p16 posit16) posit16
  [fl posit16-sqrt])

(define-operator-impl (sqrt sqrt.p32 posit32) posit32
  [fl posit32-sqrt])

(define-operator-impl (== ==.p8 posit8 posit8) bool
  [fl posit8=])

(define-operator-impl (== ==.p16 posit16 posit16) bool
  [fl posit16=])

(define-operator-impl (== ==.p32 posit32 posit32) bool
  [fl posit32=])

(define-operator-impl (!= !=.p8 posit8 posit8) bool
  [fl (negate posit8=)])

(define-operator-impl (!= !=.p16 posit16 posit16) bool
  [fl (negate posit16=)])

(define-operator-impl (!= !=.p32 posit32 posit32) bool
  [fl (negate posit32=)])

(define-operator-impl (< <.p8 posit8 posit8) bool
  [fl posit8<])

(define-operator-impl (< <.p16 posit16 posit16) bool
  [fl posit16<])

(define-operator-impl (< <.p32 posit32 posit32) bool
  [fl posit32<])

(define-operator-impl (> >.p8 posit8 posit8) bool
  [fl posit8>])

(define-operator-impl (> >.p16 posit16 posit16) bool
  [fl posit16>])

(define-operator-impl (> >.p32 posit32 posit32) bool
  [fl posit32>])

(define-operator-impl (<= <=.p8 posit8 posit8) bool
  [fl posit8<=])

(define-operator-impl (<= <=.p16 posit16 posit16) bool
  [fl posit16<=])

(define-operator-impl (<= <=.p32 posit32) bool
  [fl posit32<=])

(define-operator-impl (>= >=.p8 posit8 posit8) bool
  [fl posit8>=])

(define-operator-impl (>= >=.p16 posit16 posit16) bool
  [fl posit16>=])

(define-operator-impl (>= >=.p32 posit32 posit32) bool
  [fl posit32>=])

;; Posit/float implementations

(define-operator-impl (cast binary64->posit8 binary64) posit8
  [fl double->posit8])

(define-operator-impl (cast binary64->posit16 binary64) posit16
  [fl double->posit16])

(define-operator-impl (cast binary64->posit32 binary64) posit32
  [fl double->posit32])

(define-operator-impl (cast posit8->binary64 posit8) binary64
  [fl posit8->double])

(define-operator-impl (cast posit16->binary64 posit16) binary64
  [fl posit16->double])

(define-operator-impl (cast posit32->binary64 posit32) binary64
  [fl posit32->double])

;; Quire/float implementations 

(define-operator-impl (cast binary64->quire8 binary64) quire8
  [fl double->quire8])

(define-operator-impl (cast binary64->quire16 binary64) quire16
  [fl double->quire16])

(define-operator-impl (cast binary64->quire32 binary64) quire32
  [fl double->quire32])

(define-operator-impl (cast quire8->binary64 quire8) binary64
  [fl quire8->double])

(define-operator-impl (cast quire16->binary64 quire16) binary64
  [fl quire16->double])

(define-operator-impl (cast quire16->binary64 quire16) binary64
  [fl quire16->double])

(define (bffdp x y z) (bf+ x (bf* y z)))
(define (bffdm x y z) (bf- x (bf* y z)))

;; Quire/posit fused ops

(define-operator (fdp real real real) real
  [bf bffdp] [ival (λ (x y z) (ival-add x (ival-mult y z)))] [nonffi (λ (x y z) (+ (* x y) z))])

(define-operator (fdm real real real) real
  [bf bffdm] [ival (λ (x y z) (ival-sub x (ival-mult y z)))] [nonffi (λ (x y z) (- (* x y) z))])

;; Quire/posit fused impl

(define-operator-impl (fdp quire8-mul-add quire8 posit8 posit8) quire8
  [fl quire8-fdp-add])

(define-operator-impl (fdp quire16-mul-add quire16 posit16 posit16) quire16
  [fl quire16-fdp-add])

(define-operator-impl (fdp quire32-mul-add quire32 posit32 posit32) quire32
  [fl quire32-fdp-add])

(define-operator-impl (fdm quire8-mul-sub quire8 posit8 posit8) quire8
  [fl quire8-fdp-sub] [inherit 'mul-sub])

(define-operator-impl (fdm quire16-mul-sub quire16 posit16 posit16) quire16
  [fl quire16-fdp-sub] [inherit 'mul-sub])

(define-operator-impl (fdm quire32-mul-sub quire32 posit32 posit32) quire32
  [fl quire32-fdp-sub])

;; Quire/posit impl

(define-operator-impl (cast quire8->posit8 quire8) posit8
  [fl quire8->posit8])

(define-operator-impl (cast quire16->posit16 quire16) posit16
  [fl quire16->posit16])

(define-operator-impl (cast quire32->posit32 quire32) posit32
  [fl quire32->posit32])

(define-operator-impl (cast posit8->quire8 posit8) quire8
  [fl posit8->quire8])

(define-operator-impl (cast posit16->quire16 posit16) quire16
  [fl posit16->quire16])

(define-operator-impl (cast posit32->quire32 posit32) quire32
  [fl posit32->quire32])

;; Defining the rules

(define-ruleset commutativity.p16 (arithmetic simplify posit)
  #:type ([a posit16] [b posit16])
  [+-commutative.p16     (+.p16 a b)               (+.p16 b a)]
  [*-commutative.p16     (*.p16 a b)               (*.p16 b a)])

; Posit conversions
(define-ruleset insert-p16 (arithmetic posit)
  #:type ([a binary64])
  [insert-posit16 a (posit16->binary64 (binary64->posit16 a))])

(define-ruleset remove-p16 (arithmetic simplify posit)
  #:type ([a binary64])
  [remove-posit16 (posit16->binary64 (binary64->posit16 a)) a])

;; TODO: Multiply add to mulAdd

;; TODO: We only cast back to posit after quire operations because herbie can't handle
;; non-double output right now (similar situtation for posits)
(define-ruleset q16-arithmetic (arithmetic posit)
  #:type ([a posit16] [b posit16] [c posit16] [q quire16])
  [introduce-quire      a               (quire16->posit16 (posit16->quire16 a))]
  [insert-quire-add     (+.p16 (quire16->posit16 q) a)
                        (quire16->posit16 (quire16-mul-add q a (binary64->posit16 1.0)))]
  [insert-quire-sub     (-.p16 (quire16->posit16 q) a)
                        (quire16->posit16 (quire16-mul-sub q a (binary64->posit16 1.0)))]
  [insert-quire-fdp-add (+.p16 (quire16->posit16 q) (*.p16 a b))
                        (quire16->posit16 (quire16-mul-add q a b))]
  [insert-quire-fdp-sub (-.p16 (quire16->posit16 q) (*.p16 a b))
                        (quire16->posit16 (quire16-mul-sub q a b))])
