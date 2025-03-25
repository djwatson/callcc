(import (scheme base) (scheme write))

;; TODO: Things where from > to should probably not return an error but just return OK?
;; CHIBI does this correctly.

;; Test copy-ish procedures
(define-syntax test-error
  (syntax-rules ()
    ((_ a) (test-error 'a a))
    ((_ name a)
     (let ((err (guard (cxp (#t #t)) a #f))) 
       (unless err (display "SHOULD HAVE FAILED:") (display name) (newline) (display 'a) (newline))))))
(define-syntax test-ok
  (syntax-rules ()
    ((_ a) (test-ok 'a a))
    ((_ name a)
     (let ((err (guard (cxp (#t #t)) a #f))) 
       (when err (display "FAIL:") (display name) (newline) (display 'a) (newline))))))

(define-syntax test-copyish
  (syntax-rules ()
      ((_ func obj)
       (test-copyish-int 'func func obj))))

(define (test-copyish-int name func obj)
  (test-error name (func obj 2 0))
  (test-error name (func obj -1 0))
  (test-ok name (func obj 0 3))
  (test-error name (func obj 0 5))
  (test-error name (func obj 10 15))
  (test-ok name (func obj 0 0))
  (test-ok name (func obj 2 2))
  (test-ok name (func obj 3 3))
  (test-error name (func obj 1/2 2))
  (test-error name (func obj 0 2/3))
  (test-error name (func obj 4 4)))

(define-syntax test-copyish!
  (syntax-rules ()
    ((_ func to obj) (test-copyish! 'func func to obj))
    ((_ name func to obj)
     (test-copyish!-int name func to obj))))

(define (test-copyish!-int name func to obj)
  (test-error name (func to 0 obj 2 0))
  (test-error name (func to 0 obj -1 0))
  (test-ok name (func to 0 obj 0 3))
  (test-error name (func to 0 obj 0 5))
  (test-error name (func to 0 obj 10 15))
  (test-ok name (func to 0 obj 0 0))
  (test-error name (func to 1/2 obj 0 0))
  (test-error name (func to 0 obj 1/2 2))
  (test-error name (func to 0 obj 0 2/3))
  (test-ok name (func to 0 obj 2 2))
  (test-ok name (func to 0 obj 3 3))
  (test-error name (func to 0 obj 4 4))

  (test-error name (func #f 0 obj 0))
  (test-error name (func to 2 obj 0)))


(test-copyish string->utf8 "foo")
(test-copyish utf8->string #u8(102 111 111))
(test-copyish! bytevector-copy! #u8(102 111 111) #u8(102 111 111 ))
(test-ok (bytevector-copy! #u8() 0 #u8( )))
(test-copyish bytevector-copy #u8(102 111 111))
(test-ok (bytevector-copy #u8()))
(test-copyish substring "foo")
(test-copyish (lambda (obj . args)
		(apply write-string obj (open-output-string) args)) "foo")
(test-copyish (lambda (obj . args)
		(apply write-bytevector obj (open-output-bytevector) args)) #u8(102 111 111 ))
(test-copyish (lambda (obj . args)
		(apply read-bytevector!  obj (open-input-bytevector #u8(10 20 30)) args)) #u8(0 0 0))
(test-copyish (lambda (obj . args)
		(apply read-bytevector!  obj (open-input-bytevector #u8(10 20 30)) args)) #u8(0 0 0))
(test-copyish! string-copy! "foo" "foo")
(test-ok (string-copy! "" 0 ""))
(test-copyish string-copy "foo")
(test-ok (string-copy ""))
(test-copyish! vector-copy! #('a 'b 'c) #(1 2 3))
(test-ok (vector-copy! #() 0 #()))
(test-copyish (lambda (obj . args)
		(apply vector-fill! obj 1 args)) #(1 2 3))
(test-copyish vector-copy #(1 2 3))
(test-ok (vector-copy #()))
(test-copyish vector->string #(#\a #\b #\c))
(test-copyish (lambda (obj . vals)
		(apply string-fill! obj #\a vals)) "foo")
(test-copyish string->list "foo")
