(import (scheme r5rs))
(define-syntax foo
  (syntax-rules ()
    ((_ (a ...) (b ...) ...) '((a  (b ...) ) ...))))

(display (foo (1 2 3) (4 4) (5 5) ))
(newline)

(define-syntax foo
  (syntax-rules ()
    ((_ (a b ...) ...) '(((a  b ) ...) ...))))

(display (foo ((1 2 3) (4 4) (5 5)) ))
(newline)

(define-syntax foo
  (syntax-rules ()
    ((_ a b ...) '((a  b ) ...))))

(display (foo (1 2 3) (4 4) (5 5) ))
(newline)

;; Too many ellipsis

;; (define-syntax foo
;;   (syntax-rules ()
;;     ((_  a b ...) '(( ((a b) ...)  ...)))))

;; (display (foo (1 2 3) (4 4 4) (5 5 5) ))

;; Too few ellipsis

;; (define-syntax foo
;;   (syntax-rules ()
;;     ((_ a ...) a)))
;; (foo 1)

