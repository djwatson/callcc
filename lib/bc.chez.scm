(import (srfi :1))
(import (srfi :2))
(import (srfi :9))
(import (srfi :69))
(import (srfi :28))
(import (srfi :26))
(import (srfi :37))
(import (srfi :151))

(define-syntax cond-expand
  (syntax-rules (and or not else chez)
    ((cond-expand) (syntax-error "Unfulfilled cond-expand"))
    ((cond-expand (else body ...))
     (begin body ...))
    ((cond-expand ((and) body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
       (req1
         (cond-expand
           ((and req2 ...) body ...)
           more-clauses ...))
       more-clauses ...))
    ((cond-expand ((or) body ...) more-clauses ...)
     (cond-expand more-clauses ...))
    ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
       (req1
        (begin body ...))
       (else
        (cond-expand
           ((or req2 ...) body ...)
           more-clauses ...))))
    ((cond-expand ((not req) body ...) more-clauses ...)
     (cond-expand
       (req
         (cond-expand more-clauses ...))
       (else body ...)))
    ((cond-expand (chez body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (feature-id body ...) more-clauses ...)
     (cond-expand more-clauses ...))))

(define chez-hash-table-copy hash-table-copy)
(define (hash-table-copy table)
  (chez-hash-table-copy table #t))

(include "util.scm")
(include "qq.scm")
(include "match.scm")
(include "format.scm")
(include "library-manager.scm")
(include "expand.scm")
(include "passes.scm")
(include "fix-letrec.scm")

(define (list-set! list k obj)
  (if (= k 0)
      (set-car! list obj)
      (list-set! (cdr list) (- k 1) obj)))

(define (write-u8 c p)
  (put-u8 p c))

(define write-bytevector
  (case-lambda
    ((bv) (write-bytevector bv (current-output-port)))
    ((bv port) (put-bytevector port bv))
    ((bv port start) (write-bytevector (%subbytevector1 bv start) port))
    ((bv port start end)
     (write-bytevector (%subbytevector bv start end) port))))
(define (open-output-bytevector)
  (let-values (((p extract) (open-bytevector-output-port)))
    (define pos 0)
    (define buf #vu8())
    (define (read! target target-start count)
      (when (zero? (- (bytevector-length buf) pos))
        (set! buf (bytevector-append buf (extract))))  ;resets p
      (let ((count (min count (- (bytevector-length buf) pos))))
        (bytevector-copy! buf pos
                             target target-start count)
        (set! pos (+ pos count))
        count))
    (define (write! bv start count)
      (put-bytevector p bv start count)
      (set! pos (+ pos count))
      count)
    (define (get-position)
      pos)
    (define (set-position! new-pos)
      (set! pos new-pos))
    (define (close)
      (close-port p))
    ;; It's actually an input/output port, but only
    ;; get-output-bytevector should ever read from it. If it was just
    ;; an output port then there would be no good way for
    ;; get-output-bytevector to read the data. -weinholt
    (make-custom-binary-input/output-port
     "bytevector" read! write! get-position set-position! close)))

(define (get-output-bytevector port)
  ;; R7RS says "It is an error if port was not created with
  ;; open-output-bytevector.", so we can safely assume that the port
  ;; was created by open-output-bytevector. -weinholt
  (set-port-position! port 0)
  (let ((bv (get-bytevector-all port)))
    (if (eof-object? bv)
        #vu8()
        bv)))
(define (bytevector-append . bvs)
  (call-with-bytevector-output-port
    (lambda (p)
      (for-each (lambda (bv) (put-bytevector p bv)) bvs))))

(define (open-binary-output-file f) (open-file-output-port f (file-options no-fail) ))
(define arithmetic-shift (lambda (i c) (bitwise-arithmetic-shift i c)))

(define (write-double d p)
  (define bv (make-bytevector 8))
  (bytevector-ieee-double-native-set! bv 0 d)
  (put-bytevector p bv))
(display (cdr (command-line)))
(define (get-double-as-u64 x) 0)

(include "bc.scm")

;(for-each (lambda (x) (compile-file x #t)) (cdr (command-line)))


