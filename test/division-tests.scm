(import (scheme base) (scheme write) )
(define *tests-run* 0)
(define *tests-passed* 0)

(define-syntax test
  (syntax-rules ()
    ((test name expect expr)
     (test expect expr))
    ((test expect expr)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (write *tests-run*)
       (display ". ")
       (display 'expr )
       (let ((res expr))
         ;(write-char #\space)
         ;(display (make-string (max 0 (- 72 (string-length str))) #\.))
					;(flush-output)
         (cond
          ((equal? res expect)
           (set! *tests-passed* (+ *tests-passed* 1))
           (display " [PASS]\n"))
          (else
           (display " [FAIL]\n")
           (display "    expected ") (write expect)
           (display " but got ") (write res) (newline))))))))

(define-syntax test-assert
  (syntax-rules ()
    ((test-assert expr) (test #t expr))))

(define (test-begin . name)
  (display "Test begin: ")
  (display (car name))
  (newline))

(define (test-end . any)
  (write *tests-passed*)
  (display " out of ")
  (write *tests-run*)
  (display " passed (")
  (write (* (/ *tests-passed* *tests-run*) 100))
  (display "%)")
  (newline))

(define (ceiling-quotient a b) (ceiling (/ a b)))
(define (ceiling-remainder a b) (- a (* b (ceiling (/ a b)))))
(define (floor-quotient a b) (floor (/ a b)))
(define (floor-remainder a b) (- a (* b (floor (/ a b)))))
(define (truncate-quotient a b) (truncate (/ a b)))
(define (truncate-remainder a b) (- a (* b (truncate (/ a b)))))
(define (round-quotient a b) (round (/ a b)))
(define (round-remainder a b) (- a (* b (round (/ a b)))))


(test-begin "division")

;; These verify the basic rounding operators floor, ceiling, truncate
;; and round, on which the other tests depend.

(test-begin "rounding")

(test 4.0 (floor 4.3))
(test 5.0 (ceiling 4.3))
(test 4.0 (truncate 4.3))
(test 4.0 (round 4.3))
(test -5.0 (floor -4.3))
(test -4.0 (ceiling -4.3))
(test -4.0 (truncate -4.3))
(test -4.0 (round -4.3))
(test 3.0 (floor 3.5))
(test 4.0 (ceiling 3.5))
(test 3.0 (truncate 3.5))
(test 4.0 (round 3.5))
(test -4.0 (floor -3.5))
(test -3.0 (ceiling -3.5))
(test -3.0 (truncate -3.5))
(test -4.0 (round -3.5))
(test 3 (floor (/ 1300000000000000000000 400000000000000000000)))
(test 4 (ceiling (/ 1300000000000000000000 400000000000000000000)))
(test 3 (truncate (/ 1300000000000000000000 400000000000000000000)))
(test 3 (round (/ 1300000000000000000000 400000000000000000000)))
(test -4 (floor (/ -1300000000000000000000 400000000000000000000)))
(test -3 (ceiling (/ -1300000000000000000000 400000000000000000000)))
(test -3 (truncate (/ -1300000000000000000000 400000000000000000000)))
(test -3 (round (/ -1300000000000000000000 400000000000000000000)))
(test 650000000000000000000 (floor (/ 1300000000000000000001 2)))
(test 650000000000000000001 (ceiling (/ 1300000000000000000001 2)))
(test 650000000000000000000 (truncate (/ 1300000000000000000001 2)))
(test 650000000000000000000 (round (/ 1300000000000000000001 2)))
(test 650000000000000000001 (floor (/ 1300000000000000000003 2)))
(test 650000000000000000002 (ceiling (/ 1300000000000000000003 2)))
(test 650000000000000000001 (truncate (/ 1300000000000000000003 2)))
(test 650000000000000000002 (round (/ 1300000000000000000003 2)))
(test -650000000000000000001 (floor (/ -1300000000000000000001 2)))
(test -650000000000000000000 (ceiling (/ -1300000000000000000001 2)))
(test -650000000000000000000 (truncate (/ -1300000000000000000001 2)))
(test -650000000000000000000 (round (/ -1300000000000000000001 2)))
(test -650000000000000000002 (floor (/ -1300000000000000000003 2)))
(test -650000000000000000001 (ceiling (/ -1300000000000000000003 2)))
(test -650000000000000000001 (truncate (/ -1300000000000000000003 2)))
(test -650000000000000000002 (round (/ -1300000000000000000003 2)))
(test 4 (round 7/2))
(test 7 (round 7))

(test-end)

(test-begin "trivial zero divisor")

;; All operators are the same when the quotient is zero.

(test 0 (ceiling-quotient 0 4))
(test 0 (ceiling-quotient 0 -4))

(test 0 (ceiling-remainder 0 4))
(test 0 (ceiling-remainder 0 -4))

(test 0 (floor-quotient 0 4))
(test 0 (floor-quotient 0 -4))

(test 0 (floor-remainder 0 4))
(test 0 (floor-remainder 0 -4))

(test 0 (truncate-quotient 0 4))
(test 0 (truncate-quotient 0 -4))

(test 0 (truncate-remainder 0 4))
(test 0 (truncate-remainder 0 -4))

(test 0 (round-quotient 0 4))
(test 0 (round-quotient 0 -4))

(test 0 (round-remainder 0 4))
(test 0 (round-remainder 0 -4))

;; (test 0 (euclidean-quotient 0 4))
;; (test 0 (euclidean-quotient 0 -4))

;; (test 0 (euclidean-remainder 0 4))
;; (test 0 (euclidean-remainder 0 -4))

;; (test 0 (balanced-quotient 0 4))
;; (test 0 (balanced-quotient 0 -4))

;; (test 0 (balanced-remainder 0 4))
;; (test 0 (balanced-remainder 0 -4))

(test-end)

(test-begin "trivial one dividend")

;; The remainder is always zero when dividing by one.

(test 13 (ceiling-quotient 13 1))
(test -13 (ceiling-quotient -13 1))

(test 0 (ceiling-remainder 13 1))
(test 0 (ceiling-remainder -13 1))

(test 13 (floor-quotient 13 1))
(test -13 (floor-quotient -13 1))

(test 0 (floor-remainder 13 1))
(test 0 (floor-remainder -13 1))

(test 13 (truncate-quotient 13 1))
(test -13 (truncate-quotient -13 1))

(test 0 (truncate-remainder 13 1))
(test 0 (truncate-remainder -13 1))

(test 13 (round-quotient 13 1))
(test -13 (round-quotient -13 1))

(test 0 (round-remainder 13 1))
(test 0 (round-remainder -13 1))

;; (test 13 (euclidean-quotient 13 1))
;; (test -13 (euclidean-quotient -13 1))

;; (test 0 (euclidean-remainder 13 1))
;; (test 0 (euclidean-remainder -13 1))

;; (test 13 (balanced-quotient 13 1))
;; (test -13 (balanced-quotient -13 1))

;; (test 0 (balanced-remainder 13 1))
;; (test 0 (balanced-remainder -13 1))

(test-end)

(test-begin "fixnum division")

;; Ceiling rounds towards positive infinity.

(test 4 (ceiling-quotient 13 4))
(test -3 (ceiling-quotient -13 4))
(test -3 (ceiling-quotient 13 -4))
(test 4 (ceiling-quotient -13 -4))

(test -3 (ceiling-remainder 13 4))
(test -1 (ceiling-remainder -13 4))
(test 1 (ceiling-remainder 13 -4))
(test 3 (ceiling-remainder -13 -4))

;; Floor rounds towards negative infinity.

(test 3 (floor-quotient 13 4))
(test -4 (floor-quotient -13 4))
(test -4 (floor-quotient 13 -4))
(test 3 (floor-quotient -13 -4))

(test 1 (floor-remainder 13 4))
(test 3 (floor-remainder -13 4))
(test -3 (floor-remainder 13 -4))
(test -1 (floor-remainder -13 -4))

;; Truncate rounds towards zero - the magnitudes never change
;; regardless of the signs.

(test 3 (truncate-quotient 13 4))
(test -3 (truncate-quotient -13 4))
(test -3 (truncate-quotient 13 -4))
(test 3 (truncate-quotient -13 -4))

(test 1 (truncate-remainder 13 4))
(test -1 (truncate-remainder -13 4))
(test 1 (truncate-remainder 13 -4))
(test -1 (truncate-remainder -13 -4))

;; Round rounds towards the nearest integer - it's equivalent to floor
;; when signs are the same, and equivalent to ceiling when the signs
;; differ.

(test 3 (round-quotient 13 4))
(test -3 (round-quotient -13 4))
(test -3 (round-quotient 13 -4))
(test 3 (round-quotient -13 -4))

(test 1 (round-remainder 13 4))
(test -1 (round-remainder -13 4))
(test 1 (round-remainder 13 -4))
(test -1 (round-remainder -13 -4))

;; Euclidean rounds such that the remainder is always in the interval
;; [0, divisor) - i.e. the remainder is always non-negative.  It's
;; equivalent to floor if the divisor is negative, and ceiling
;; otherwise.

;; (test 3 (euclidean-quotient 13 4))
;; (test -4 (euclidean-quotient -13 4))
;; (test -3 (euclidean-quotient 13 -4))
;; (test 4 (euclidean-quotient -13 -4))

;; (test 1 (euclidean-remainder 13 4))
;; (test 3 (euclidean-remainder -13 4))
;; (test 1 (euclidean-remainder 13 -4))
;; (test 3 (euclidean-remainder -13 -4))

;; Balanced differs from truncate only in the 0.5 remainder border
;; case in the next test group.

;; (test 3 (balanced-quotient 13 4))
;; (test -3 (balanced-quotient -13 4))
;; (test -3 (balanced-quotient 13 -4))
;; (test 3 (balanced-quotient -13 -4))

;; (test 1 (balanced-remainder 13 4))
;; (test -1 (balanced-remainder -13 4))
;; (test 1 (balanced-remainder 13 -4))
;; (test -1 (balanced-remainder -13 -4))

(test-end)

(test-begin "one half remainder")

;; Testing the 0.5 remainder border cases.  Ceiling, floor and
;; truncate and euclidean don't change.

(test 7 (ceiling-quotient 13 2))
(test -6 (ceiling-quotient -13 2))
(test -6 (ceiling-quotient 13 -2))
(test 7 (ceiling-quotient -13 -2))

(test -1 (ceiling-remainder 13 2))
(test -1 (ceiling-remainder -13 2))
(test 1 (ceiling-remainder 13 -2))
(test 1 (ceiling-remainder -13 -2))

(test 6 (floor-quotient 13 2))
(test -7 (floor-quotient -13 2))
(test -7 (floor-quotient 13 -2))
(test 6 (floor-quotient -13 -2))

(test 1 (floor-remainder 13 2))
(test 1 (floor-remainder -13 2))
(test -1 (floor-remainder 13 -2))
(test -1 (floor-remainder -13 -2))

(test 6 (truncate-quotient 13 2))
(test -6 (truncate-quotient -13 2))
(test -6 (truncate-quotient 13 -2))
(test 6 (truncate-quotient -13 -2))

(test 1 (truncate-remainder 13 2))
(test -1 (truncate-remainder -13 2))
(test 1 (truncate-remainder 13 -2))
(test -1 (truncate-remainder -13 -2))

;; (test 6 (euclidean-quotient 13 2))
;; (test -7 (euclidean-quotient -13 2))
;; (test -6 (euclidean-quotient 13 -2))
;; (test 7 (euclidean-quotient -13 -2))

;; (test 1 (euclidean-remainder 13 2))
;; (test 1 (euclidean-remainder -13 2))
;; (test 1 (euclidean-remainder 13 -2))
;; (test 1 (euclidean-remainder -13 -2))

;; For consistency with the default rounding mode specified by the
;; IEEE floating point standard, round rounds to even when exactly
;; half-way between two integers.

(test 6 (round-quotient 13 2))
(test -6 (round-quotient -13 2))
(test -6 (round-quotient 13 -2))
(test 6 (round-quotient -13 -2))

(test 1 (round-remainder 13 2))
(test -1 (round-remainder -13 2))
(test 1 (round-remainder 13 -2))
(test -1 (round-remainder -13 -2))

;; Balanced rounds up when exactly half-way between two integers.

;; (test 7 (balanced-quotient 13 2))
;; (test -6 (balanced-quotient -13 2))
;; (test -7 (balanced-quotient 13 -2))
;; (test 6 (balanced-quotient -13 -2))

;; (test -1 (balanced-remainder 13 2))
;; (test -1 (balanced-remainder -13 2))
;; (test -1 (balanced-remainder 13 -2))
;; (test -1 (balanced-remainder -13 -2))

(test-end)

(test-begin "exactness")

(define-syntax values->list
  (syntax-rules ()
    ((values->list expr) (call-with-values (lambda () expr) list))))

;; (test '(4.0 -3.0) (values->list (ceiling/ 13.0 4)))
;; (test '(4.0 -3.0) (values->list (ceiling/ 13 4.0)))
;; (test '(4.0 -3.0) (values->list (ceiling/ 13.0 4.0)))

(test 4.0 (ceiling-quotient 13.0 4))
(test 4.0 (ceiling-quotient 13 4.0))
(test 4.0 (ceiling-quotient 13.0 4.0))

(test -3.0 (ceiling-remainder 13.0 4))
(test -3.0 (ceiling-remainder 13 4.0))
(test -3.0 (ceiling-remainder 13.0 4.0))

;; (test '(3.0 1.0) (values->list (floor/ 13.0 4)))
;; (test '(3.0 1.0) (values->list (floor/ 13 4.0)))
;; (test '(3.0 1.0) (values->list (floor/ 13.0 4.0)))

(test 3.0 (floor-quotient 13.0 4))
(test 3.0 (floor-quotient 13 4.0))
(test 3.0 (floor-quotient 13.0 4.0))

(test 1.0 (floor-remainder 13.0 4))
(test 1.0 (floor-remainder 13 4.0))
(test 1.0 (floor-remainder 13.0 4.0))

;; (test '(3.0 1.0) (values->list (truncate/ 13.0 4)))
;; (test '(3.0 1.0) (values->list (truncate/ 13 4.0)))
;; (test '(3.0 1.0) (values->list (truncate/ 13.0 4.0)))

(test 3.0 (truncate-quotient 13.0 4))
(test 3.0 (truncate-quotient 13 4.0))
(test 3.0 (truncate-quotient 13.0 4.0))

(test 1.0 (truncate-remainder 13.0 4))
(test 1.0 (truncate-remainder 13 4.0))
(test 1.0 (truncate-remainder 13.0 4.0))

;; (test '(3.0 1.0) (values->list (round/ 13.0 4)))
;; (test '(3.0 1.0) (values->list (round/ 13 4.0)))
;; (test '(3.0 1.0) (values->list (round/ 13.0 4.0)))

(test 3.0 (round-quotient 13.0 4))
(test 3.0 (round-quotient 13 4.0))
(test 3.0 (round-quotient 13.0 4.0))

(test 1.0 (round-remainder 13.0 4))
(test 1.0 (round-remainder 13 4.0))
(test 1.0 (round-remainder 13.0 4.0))

;; (test '(3.0 1.0) (values->list (euclidean/ 13.0 4)))
;; (test '(3.0 1.0) (values->list (euclidean/ 13 4.0)))
;; (test '(3.0 1.0) (values->list (euclidean/ 13.0 4.0)))

;; (test 3.0 (euclidean-quotient 13.0 4))
;; (test 3.0 (euclidean-quotient 13 4.0))
;; (test 3.0 (euclidean-quotient 13.0 4.0))

;; (test 1.0 (euclidean-remainder 13.0 4))
;; (test 1.0 (euclidean-remainder 13 4.0))
;; (test 1.0 (euclidean-remainder 13.0 4.0))

;; (test '(3.0 1.0) (values->list (balanced/ 13.0 4)))
;; (test '(3.0 1.0) (values->list (balanced/ 13 4.0)))
;; (test '(3.0 1.0) (values->list (balanced/ 13.0 4.0)))

;; (test 3.0 (balanced-quotient 13.0 4))
;; (test 3.0 (balanced-quotient 13 4.0))
;; (test 3.0 (balanced-quotient 13.0 4.0))

;; (test 1.0 (balanced-remainder 13.0 4))
;; (test 1.0 (balanced-remainder 13 4.0))
;; (test 1.0 (balanced-remainder 13.0 4.0))

(test-end)

(test-begin "bignum division")

;; A repeat of the fixnum division tests above, using bignums to test
;; bignum division.

(test 4 (ceiling-quotient 1300000000000000000000 400000000000000000000))
(test -3 (ceiling-quotient -1300000000000000000000 400000000000000000000))
(test -3 (ceiling-quotient 1300000000000000000000 -400000000000000000000))
(test 4 (ceiling-quotient -1300000000000000000000 -400000000000000000000))

(test -300000000000000000000
      (ceiling-remainder 1300000000000000000000 400000000000000000000))
(test -100000000000000000000
      (ceiling-remainder -1300000000000000000000 400000000000000000000))
(test 100000000000000000000
      (ceiling-remainder 1300000000000000000000 -400000000000000000000))
(test 300000000000000000000
      (ceiling-remainder -1300000000000000000000 -400000000000000000000))

(test 3 (floor-quotient 1300000000000000000000 400000000000000000000))
(test -4 (floor-quotient -1300000000000000000000 400000000000000000000))
(test -4 (floor-quotient 1300000000000000000000 -400000000000000000000))
(test 3 (floor-quotient -1300000000000000000000 -400000000000000000000))

(test 100000000000000000000
      (floor-remainder 1300000000000000000000 400000000000000000000))
(test 300000000000000000000
      (floor-remainder -1300000000000000000000 400000000000000000000))
(test -300000000000000000000
      (floor-remainder 1300000000000000000000 -400000000000000000000))
(test -100000000000000000000
      (floor-remainder -1300000000000000000000 -400000000000000000000))

(test 3 (truncate-quotient 1300000000000000000000 400000000000000000000))
(test -3 (truncate-quotient -1300000000000000000000 400000000000000000000))
(test -3 (truncate-quotient 1300000000000000000000 -400000000000000000000))
(test 3 (truncate-quotient -1300000000000000000000 -400000000000000000000))

(test 100000000000000000000
      (truncate-remainder 1300000000000000000000 400000000000000000000))
(test -100000000000000000000
      (truncate-remainder -1300000000000000000000 400000000000000000000))
(test 100000000000000000000
      (truncate-remainder 1300000000000000000000 -400000000000000000000))
(test -100000000000000000000
      (truncate-remainder -1300000000000000000000 -400000000000000000000))

(test 3 (round-quotient 1300000000000000000000 400000000000000000000))
(test -3 (round-quotient -1300000000000000000000 400000000000000000000))
(test -3 (round-quotient 1300000000000000000000 -400000000000000000000))
(test 3 (round-quotient -1300000000000000000000 -400000000000000000000))

(test 100000000000000000000
      (round-remainder 1300000000000000000000 400000000000000000000))
(test -100000000000000000000
      (round-remainder -1300000000000000000000 400000000000000000000))
(test 100000000000000000000
      (round-remainder 1300000000000000000000 -400000000000000000000))
(test -100000000000000000000
      (round-remainder -1300000000000000000000 -400000000000000000000))

;; (test 3 (euclidean-quotient 1300000000000000000000 400000000000000000000))
;; (test -4 (euclidean-quotient -1300000000000000000000 400000000000000000000))
;; (test -3 (euclidean-quotient 1300000000000000000000 -400000000000000000000))
;; (test 4 (euclidean-quotient -1300000000000000000000 -400000000000000000000))

;; (test 100000000000000000000
;;       (euclidean-remainder 1300000000000000000000 400000000000000000000))
;; (test 300000000000000000000
;;       (euclidean-remainder -1300000000000000000000 400000000000000000000))
;; (test 100000000000000000000
;;       (euclidean-remainder 1300000000000000000000 -400000000000000000000))
;; (test 300000000000000000000
;;       (euclidean-remainder -1300000000000000000000 -400000000000000000000))

;; (test 3 (balanced-quotient 1300000000000000000000 400000000000000000000))
;; (test -3 (balanced-quotient -1300000000000000000000 400000000000000000000))
;; (test -3 (balanced-quotient 1300000000000000000000 -400000000000000000000))
;; (test 3 (balanced-quotient -1300000000000000000000 -400000000000000000000))

;; (test 100000000000000000000
;;       (balanced-remainder 1300000000000000000000 400000000000000000000))
;; (test -100000000000000000000
;;       (balanced-remainder -1300000000000000000000 400000000000000000000))
;; (test 100000000000000000000
;;       (balanced-remainder 1300000000000000000000 -400000000000000000000))
;; (test -100000000000000000000
;;       (balanced-remainder -1300000000000000000000 -400000000000000000000))

(test-end)

(test-begin "remainder infinity")

;; (test-error (remainder +inf.0 +nan.0))
;; (test-error (remainder -inf.0 +nan.0))
;; (test-error (remainder +inf.0 2.0))
;; (test-error (remainder -inf.0 2.0))
;; (test-error (remainder +inf.0 2/1))
;; (test-error (remainder -inf.0 2/1))
;; (test-error (remainder +inf.0 2))
;; (test-error (remainder -inf.0 2))

;; (test-error (remainder +nan.0 +inf.0))
;; (test-error (remainder +nan.0 -inf.0))
;; (test-error (remainder 2.0 +inf.0))
;; (test-error (remainder 2.0 -inf.0))
;; (test-error (remainder 2/1 +inf.0))
;; (test-error (remainder 2/1 -inf.0))
;; (test-error (remainder 2 +inf.0))
;; (test-error (remainder 2 -inf.0))

(test-end "remainder infinity")

(test-end)
