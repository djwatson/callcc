;; These tests are only valid if chibi-scheme is compiled with Unicode
;; support (SEXP_USE_UTF8_STRINGS).
(import (scheme base) (scheme write) (scheme r5rs) (scheme read) (scheme inexact))

(define (test-begin . o)
  (display "Test begin: ")
  (display o)
  (newline))

(define (test-end . o) #f)
  (define-syntax test
    (syntax-rules ()
      ((test name expected expr)
       (test expected expr))
      ((test expected expr)
       (let ((res expr))
         (cond
          ((not (or (equal? expr expected)
		    (and  (inexact? expr) (inexact? expected)
			 (approx-equal? expr expected 1e-5))))
           (display "FAIL: ")
           (write 'expr)
           (display ": expected ")
           (write expected)
           (display " but got ")
           (write res)
           (newline)))))))



(test-begin "unicode")

(test #\Р (string-ref "Русский" 0))
(test #\и (string-ref "Русский" 5))
(test #\й (string-ref "Русский" 6))

(test 7 (string-length "Русский"))

(test #\日 (string-ref "日本語" 0))
(test #\本 (string-ref "日本語" 1))
(test #\語 (string-ref "日本語" 2))

(test 3 (string-length "日本語"))

(test '(#\日 #\本 #\語) (string->list "日本語"))
(test "日本語" (list->string '(#\日 #\本 #\語)))

(test "日本" (substring "日本語" 0 2))
(test "本語" (substring "日本語" 1 3))

(test "日-語"
      (let ((s (substring "日本語" 0 3)))
        (string-set! s 1 #\-)
        s))

(test "日本人"
      (let ((s (substring "日本語" 0 3)))
        (string-set! s 2 #\人)
        s))

(test "字字字" (make-string 3 #\字))

(test "字字字"
      (let ((s (make-string 3)))
        (string-fill! s #\字)
        s))



(test "in-string"
      '(#\日 #\本 #\語)
      (string->list "日本語"))

(test "in-string-reverse"
      '(#\語 #\本 #\日)
      (reverse (string->list "日本語")))

(test-end)
