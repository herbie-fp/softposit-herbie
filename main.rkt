#lang racket

(require math/flonum math/bigfloat softposit-rkt herbie/plugin rival)

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define ((inv-comparator test) . args)
  (for/or ([left args] [right (cdr args)])
    (not (test left right))))

(define ((infix-joiner x) . args)
  (string-join args x))

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(eprintf "Loading posits support...\n")

;; Defining the representations

(define-representation (posit8 real posit8?)
  (compose double->posit8 bigfloat->flonum)
  (compose bf posit8->double)
  (shift 7 ordinal->posit8)
  (unshift 7 posit8->ordinal)
  8
  (curry posit8= (posit8-nar)))

(define-representation (posit16 real posit16?)
  (compose double->posit16 bigfloat->flonum)
  (compose bf posit16->double)
  (shift 15 ordinal->posit16)
  (unshift 15 posit16->ordinal)
  16
  (curry posit16= (posit16-nar)))

(define-representation (posit32 real posit32?)
  (compose double->posit32 bigfloat->flonum)
  (compose bf posit32->double)
  (shift 31 ordinal->posit32)
  (unshift 31 posit32->ordinal)
  32
  (curry posit32= (posit32-nar)))

;;TODO correct functions for quire (incorrect now for testing)
(define-representation (quire8 real quire8?)
  (compose double->quire8 bigfloat->flonum)
  (compose bf quire8->double)
  (compose double->quire8 ordinal->flonum)
  (compose flonum->ordinal quire8->double)
  64
  (const #f))

(define-representation (quire16 real quire16?)
  (compose double->quire16 bigfloat->flonum)
  (compose bf quire16->double)
  (compose double->quire16 ordinal->flonum)
  (compose flonum->ordinal quire16->double)
  64
  (const #f))

(define-representation (quire32 real quire32?)
  (compose double->quire32 bigfloat->flonum)
  (compose bf quire32->double)
  (compose double->quire32 ordinal->flonum)
  (compose flonum->ordinal quire32->double)
  64
  (const #f))

;; Defining the operators

(define-operator (+ +.p8 posit8 posit8) posit8
  [fl posit8-add])
(define-operator (+ +.p16 posit16 posit16) posit16
  [fl posit16-add])

(define-operator (+ +.p32 posit32 posit32) posit32
  [fl posit32-add])

(define-operator (- neg.p8 posit8) posit8
  [fl posit8-neg])

(define-operator (- neg.p16 posit16) posit16
  [fl posit16-neg])

(define-operator (- neg.p32 posit32) posit32
  [fl posit32-neg])

(define-operator (- -.p8 posit8 posit8) posit8
  [fl posit8-sub])

(define-operator (- -.p16 posit16 posit16) posit16
  [fl posit16-sub])

(define-operator (- -.p32 posit32 posit32) posit32
  [fl posit32-sub])

(define-operator (* *.p8 posit8 posit8) posit8
  [fl posit8-mul])

(define-operator (* *.p16 posit16 posit16) posit16
  [fl posit16-mul])

(define-operator (* *.p32 posit32 posit32) posit32
  [fl posit32-mul])

(define-operator (/ /.p8 posit8 posit8) posit8
  [fl posit8-div])

(define-operator (/ /.p16 posit16 posit16) posit16
  [fl posit16-div])

(define-operator (/ /.p32 posit32 posit32) posit32
  [fl posit32-div])

(define-operator (sqrt sqrt.p8 posit8) posit8
  [fl posit8-sqrt])

(define-operator (sqrt sqrt.p16 posit16) posit16
  [fl posit16-sqrt])

(define-operator (sqrt sqrt.p32 posit32) posit32
  [fl posit32-sqrt])

(define-operator (== ==.p8 posit8 posit8) bool
  [itype 'posit8] [otype 'bool] ; Override number of arguments
  [fl (comparator posit8=)])

(define-operator (== ==.p16 posit16 posit16) bool
  [itype 'posit16] [otype 'bool] ; Override number of arguments
  [fl (comparator posit16=)])

(define-operator (== ==.p32 posit32 posit32) bool
  [itype 'posit32] [otype 'bool] ; Override number of arguments
  [fl (comparator posit32=)])

(define-operator (!= !=.p8 posit8 posit8) bool
  [itype 'posit8] [otype 'bool] ; Override number of arguments
  [fl (inv-comparator posit8=)])

(define-operator (!= !=.p16 posit16 posit16) bool
  [itype 'posit16] [otype 'bool] ; Override number of arguments
  [fl (inv-comparator posit16=)])

(define-operator (!= !=.p32 posit32 posit32) bool
  [itype 'posit32] [otype 'bool] ; Override number of arguments
  [fl (inv-comparator posit32=)])

(define-operator (< <.p8 posit8 posit8) bool
  [itype 'posit8] [otype 'bool] ; Override number of arguments
  [fl (comparator posit8<)])

(define-operator (< <.p16 posit16 posit16) bool
  [itype 'posit16] [otype 'bool] ; Override number of arguments
  [fl (comparator posit16<)])

(define-operator (< <.p32 posit32 posit32) bool
  [itype 'posit32] [otype 'bool] ; Override number of arguments
  [fl (comparator posit32<)])

(define-operator (> >.p8 posit8 posit8) bool
  [itype 'posit8] [otype 'bool] ; Override number of arguments
  [fl (comparator posit8>)])

(define-operator (> >.p16 posit16 posit16) bool
  [itype 'posit16] [otype 'bool] ; Override number of arguments
  [fl (comparator posit16>)])

(define-operator (> >.p32 posit32 posit32) bool
  [itype 'posit32] [otype 'bool] ; Override number of arguments
  [fl (comparator posit32>)])

(define-operator (<= <=.p8 posit8 posit8) bool
  [itype 'posit8] [otype 'bool] ; Override number of arguments
  [fl (comparator posit8<=)])

(define-operator (<= <=.p16 posit16 posit16) bool
  [itype 'posit16] [otype 'bool] ; Override number of arguments
  [fl (comparator posit16<=)])

(define-operator (<= <=.p32 posit32) bool
  [itype 'posit32] [otype 'bool] ; Override number of arguments
  [fl (comparator posit32<=)])

(define-operator (>= >=.p8 posit8 posit8) bool
  [itype 'posit8] [otype 'bool] ; Override number of arguments
  [fl (comparator posit8>=)])

(define-operator (>= >=.p16 posit16 posit16) bool
  [itype 'posit16] [otype 'bool] ; Override number of arguments
  [fl (comparator posit16>=)])

(define-operator (>= >=.p32 posit32 posit32) bool
  [itype 'posit32] [otype 'bool] ; Override number of arguments
  [fl (comparator posit32>=)])

;; Posit/float conversions

(define-real-operator binary64->posit8
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator binary64->posit16
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator binary64->posit32
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator posit8->binary64
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator posit16->binary64
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator posit32->binary64
  [bf identity] [ival identity] [nonffi identity])

;; Posit/float implementations

(define-operator (binary64->posit8 binary64->posit8 binary64) posit8
  [fl double->posit8])

(define-operator (binary64->posit16 binary64->posit16 binary64) posit16
  [fl double->posit16])

(define-operator (binary64->posit32 binary64->posit32 binary64) posit32
  [fl double->posit32])

(define-operator (posit8->binary64 posit8->binary64 posit8) binary64
  [fl posit8->double])

(define-operator (posit16->binary64 posit16->binary64 posit16) binary64
  [fl posit16->double])

(define-operator (posit32->binary64 posit32->binary64 posit32) binary64
  [fl posit32->double])

;; Quire/float conversions

(define-real-operator binary64->quire8
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator binary64->quire16
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator binary64->quire32
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator quire8->binary64
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator quire16->binary64
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator quire32->binary64
  [bf identity] [ival identity] [nonffi identity])

;; Quire/float implementations 

(define-operator (binary64->quire8 binary64->quire8 binary64) quire8
  [fl double->quire8])

(define-operator (binary64->quire16 binary64->quire16 binary64) quire16
  [fl double->quire16])

(define-operator (binary64->quire32 binary64->quire32 binary64) quire32
  [fl double->quire32])

(define-operator (quire8->binary64 quire8->binary64 quire8) binary64
  [fl quire8->double])

(define-operator (quire16->binary64 quire16->binary64 quire16) binary64
  [fl quire16->double])

(define-operator (quire16->binary64 quire16->binary64 quire16) binary64
  [fl quire16->double])

(define (bffdp x y z) (bf+ x (bf* y z)))
(define (bffdm x y z) (bf- x (bf* y z)))

;; Quire/posit fused ops

(define-real-operator quire8-mul-add
  [bf bffdp] [ival (λ (x y z) (ival-add (ival-mult x y) z))] [nonffi (λ (x y z) (+ (* x y) z))])

(define-real-operator quire16-mul-add
  [bf bffdp] [ival (λ (x y z) (ival-add (ival-mult x y) z))] [nonffi (λ (x y z) (+ (* x y) z))])

(define-real-operator quire32-mul-add
  [bf bffdp] [ival (λ (x y z) (ival-add (ival-mult x y) z))] [nonffi (λ (x y z) (+ (* x y) z))])

(define-real-operator quire8-mul-sub
  [bf bffdm] [ival (λ (x y z) (ival-sub (ival-mult x y) z))] [nonffi (λ (x y z) (- (* x y) z))])

(define-real-operator quire16-mul-sub
  [bf bffdm] [ival (λ (x y z) (ival-sub (ival-mult x y) z))] [nonffi (λ (x y z) (- (* x y) z))])

(define-real-operator quire32-mul-sub
  [bf bffdm] [ival (λ (x y z) (ival-sub (ival-mult x y) z))] [nonffi (λ (x y z) (- (* x y) z))])

;; Quire/posit fused impl

(define-operator (quire8-mul-add quire8-mul-add quire8 posit8 posit8) quire8
  [fl quire8-fdp-add])

(define-operator (quire16-mul-add quire16-mul-add quire16 posit16 posit16) quire16
  [fl quire16-fdp-add])

(define-operator (quire32-mul-add quire32-mul-add quire32 posit32 posit32) quire32
  [fl quire32-fdp-add])

(define-operator (quire8-mul-sub quire8-mul-sub quire8 posit8 posit8) quire8
  [fl quire8-fdp-sub])

(define-operator (quire16-mul-sub quire16-mul-sub quire16 posit16 posit16) quire16
  [fl quire16-fdp-sub])

(define-operator (quire32-mul-sub quire32-mul-sub quire32 posit32 posit32) quire32
  [fl quire32-fdp-sub])

;; Quire/posit conversions

(define-real-operator quire8->posit8
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator quire16->posit16
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator quire32->posit32
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator posit8->quire8
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator posit16->quire16
  [bf identity] [ival identity] [nonffi identity])

(define-real-operator posit32->quire32
  [bf identity] [ival identity] [nonffi identity])

;; Quire/posit impl

(define-operator (quire8->posit8 quire8->posit8 quire8) posit8
  [fl quire8->posit8])

(define-operator (quire16->posit16 quire16->posit16 quire16) posit16
  [fl quire16->posit16])

(define-operator (quire32->posit32 quire32->posit32 quire32) posit32
  [fl quire32->posit32])

(define-operator (posit8->quire8 posit8->quire8 posit8) quire8
  [fl posit8->quire8])

(define-operator (posit16->quire16 posit16->quire16 posit16) quire16
  [fl posit16->quire16])

(define-operator (posit32->quire32 posit32->quire32 posit32) quire32
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