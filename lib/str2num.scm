;; What a headache.  Pretty close the chez implementation,
;; with plenty of insperation from the larceny implementation.
;; Used numeric-tests.scm to test conformance.
;; Should be r7rs compliant, but there's probably bugs.

(define-syntax make-state
  (syntax-rules ()
    ((_ name ch (str len off args ...)
	final
	body ...)
     (define (name str len off args ...)
       (if (= off len)
	   final
	   (let ((ch (char-downcase (string-ref str off))))
	     body
	     ...))))))

;; TODO: this is probably slow.  Can push digit10 stuff directly.
;; Or even duplicate all states, as the spec does, and get something
;; closer to what re2c or flex would generate.
;;
;; As is, all the prefix, radix, and the multiple complex states are all merged,
;; vs. the 100+ states generated by re2c.
(define (digit r c)
  (let ((v (cond
	    ((char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0)))
	    ((char<=? #\a c #\f) (- (char->integer c) 87))
	    (else 36))))
    (and (< v r) v)))

(make-state prefix0 ch (str len off)
	    #f
	    (case ch
	      ((#\#) (prefix1 str len (+ 1 off)))
	      (else (num0 str len off 10 #f))))

(make-state prefix1 ch (str len off) ;; saw #
	    #f
	    (case ch
	      ((#\b) (prefix2 str len (+ 1 off) 2 #f))
	      ((#\o) (prefix2 str len (+ 1 off) 8 #f))
	      ((#\d) (prefix2 str len (+ 1 off) 10 #f))
	      ((#\x) (prefix2 str len (+ 1 off) 16 #f))
	      ((#\i) (prefix4 str len (+ 1 off) inexact))
	      ((#\e) (prefix4 str len (+ 1 off) exact))
	      (else #f)))

(make-state prefix2 ch (str len off r ex) ;; saw radix
	    #f
	    (case ch
	      ((#\#) (prefix3 str len (+ 1 off) r ex))
	      (else (num0 str len off r ex))))

(make-state prefix3 ch (str len off r ex) ;; saw # after radix
	    #f
	    (case ch
	      ((#\i) (num0 str len (+ 1 off) r inexact))
	      ((#\e) (num0 str len (+ 1 off) r exact))
	      (else #f)))

(make-state prefix4 ch (str len off ex) ;; saw exactness
	    #f
	    (case ch
	      ((#\#) (prefix5 str len (+ 1 off) ex))
	      (else (num0 str len off 10 ex))))

(make-state prefix5 ch (str len off ex) ;; saw # after exactness
	    #f
	    (case ch
	      ((#\b) (num0 str len (+ 1 off) 2 ex))
	      ((#\o) (num0 str len (+ 1 off) 8 ex))
	      ((#\d) (num0 str len (+ 1 off) 10 ex))
	      ((#\x) (num0 str len (+ 1 off) 16 ex))
	      (else #f)))

(make-state num0 ch (str len off r ex)  ;; saw prefix
	    #f
	    (cond
	     ((eq? #\- ch) (num1 str len (+ 1 off) #f r ex 0 -))
	     ((eq? #\+ ch) (num1 str len (+ 1 off) #f r ex 0 +))
	     ((eq? #\. ch) (and (eq? r 10) (float0 str len (+ 1 off) #f ex #f)))
	     ((digit r ch) (num2 str len off #f r ex 0 #f))
	     (else #f)))

(make-state num1 ch (str len off x1 r ex sum sign)  ;; saw sign
	    #f
	    (cond
	     ((digit r ch) (num2 str len (+ 1 off) x1 r ex (+ (digit r ch) (* sum r)) sign))
	     ((eq? #\. ch) (and (eq? r 10) (float0 str len (+ 1 off) x1 ex sign)))
	     ((eq? #\i ch) (num3 str len (+ 1 off)  x1 r ex sum sign))
	     ((eq? #\n ch) (nan0 str len (+ 1 off)  x1 r ex sum sign))
	     (else #f)))

(make-state num2 ch (str len off x1 r ex sum sign)  ;; saw num
	    (and (not x1)
		 ((or sign +) (if (eq? ex inexact) (inexact sum) sum)))
	    (cond
	     ((digit r ch) (num2 str len (+ 1 off) x1 r ex (+ (digit r ch) (* sum r)) sign))
	     ((eq? #\/ ch) (ratio0 str len (+ 1 off) x1 r ex sign sum))
	     ((eq? #\. ch) (and (eq? r 10) (float1 str len (+ 1 off) x1 ex sum sign 0 10)))
	     ((eq? #\e ch) (and (eq? r 10) (exp0 str len (+ 1 off) x1 ((or sign +) sum))))
	     (else (complex0 str len off x1 r ex sign (if (eq? ex inexact) (inexact sum) sum)))))

(make-state num3 ch (str len off x1 r ex sum sign)  ;; saw i after sign
	    (make-rectangular (or x1 0) (sign 1))
	    (cond
	     ((eq? #\n ch) (inf0 str len (+ 1 off) x1 r ex sum sign))
	     (else #f)))

(make-state ratio0 ch (str len off x1 r ex sign nom)
	    #f
	    (cond
	     ((digit r ch) (ratio1 str len off x1 r ex sign nom 0))
	     (else #f)))

(make-state ratio1 ch (str len off x1 r ex sign nom denom)
	    (and (not x1)
		 (let ((d (if ex (ex denom) denom)))
		   (and (not (zero? d)) (/ ((or sign +) nom) d))))
	    (cond
	     ((digit r ch) (ratio1 str len (+ 1 off) x1 r ex sign  nom (+ (digit r ch) (* denom r))))
	     (else (complex0 str len off x1 r ex sign
			     (let ((d (if ex (ex denom) denom)))
			       (and (not (zero? d)) (/ ((or sign +) nom) d)))))))

(make-state inf0 ch (str len off x1 r ex sum sign)  ;; saw sign
	    #f
	    (cond
	     ((eq? #\f ch) (inf1 str len (+ 1 off) x1 r ex sum sign))
	     (else #f)))

(make-state inf1 ch (str len off x1 r ex sum sign)  ;; saw sign
	    #f
	    (cond
	     ((eq? #\. ch) (inf2 str len (+ 1 off) x1 r ex sum sign))
	     (else #f)))

(make-state inf2 ch (str len off x1 r ex sum sign)  ;; saw sign
	    #f
	    (cond
	     ((eq? #\0 ch) (inf3 str len (+ 1 off) x1 r ex sum sign))
	     (else #f)))

(make-state inf3 ch (str len off x1 r ex sum sign)  ;; saw sign
	    (and  (not x1) (not (eq? ex exact)) (sign +inf.0))
	    (cond
	     (else (complex0 str len off x1 r ex sign (and (not (eq? ex exact)) +inf.0)))))

(make-state nan0 ch (str len off x1 r ex sum sign)  ;; saw sign
	    #f
	    (cond
	     ((eq? #\a ch) (nan1 str len (+ 1 off) x1 r ex sum sign))
	     (else #f)))

(make-state nan1 ch (str len off x1 r ex sum sign)  ;; saw sign
	    #f
	    (cond
	     ((eq? #\n ch) (nan2 str len (+ 1 off) x1 r ex sum sign))
	     (else #f)))

(make-state nan2 ch (str len off x1 r ex sum sign)  ;; saw sign
	    #f
	    (cond
	     ((eq? #\. ch) (nan3 str len (+ 1 off) x1 r ex sum sign))
	     (else #f)))

(make-state nan3 ch (str len off x1 r ex sum sign)  ;; saw sign
	    #f
	    (cond
	     ((eq? #\0 ch) (nan4 str len (+ 1 off) x1 r ex sum sign))
	     (else #f)))

(make-state nan4 ch (str len off x1 r ex sum sign)  ;; saw sign
	    (and (not x1) (not (eq? ex exact)) (sign +nan.0))
	    (cond
	     (else (complex0 str len off x1 r ex sign (and (not (eq? ex exact)) +nan.0)))))

(make-state float0 ch (str len off x1 ex sign )  
	    #f
	    (cond
	     ((digit 10 ch) (float1 str len off x1 ex 0 sign 0 10))
	     (else #f)))

(make-state float1 ch (str len off x1 ex sum sign frac base)  
	    (and (not x1) ((or sign +) ((or ex inexact) (+ sum frac))))
	    (cond
	     ((digit 10 ch) (float1 str len (+ 1 off) x1 ex sum sign (+ (inexact (/ (inexact (digit 10 ch)) base)) frac) (* base 10)))
	     ((eq? #\e ch) (exp0 str len (+ 1 off) x1 ((or sign +) (+ sum frac))))
	     (else (complex0 str len off x1 10 ex sign ((or ex inexact) (+ sum frac))))))

(make-state exp0 ch (str len off x1 sum)
	    #f
	    (cond
	     ((digit 10 ch) (exp2 str len off x1 sum + 0))
	     ((eq? #\+ ch) (exp1 str len (+ 1 off) x1 sum +))
	     ((eq? #\- ch) (exp1 str len (+ 1 off) x1 sum -))
	     (else #f)))

(make-state exp1 ch (str len off x1 sum sign)
	    #f
	    (cond
	     ((digit 10 ch) (exp2 str len off x1 sum sign 0))
	     (else #f)))

;; TODO correct sign fix? test complex?
(make-state exp2 ch (str len off x1 sum sign exponent)
	    (and (not x1) (inexact (* sum (expt 10 (sign exponent)))))
	    (cond
	     ((digit 10 ch) (if (> exponent 100) +inf.0
				(exp2 str len (+ 1 off) x1 sum sign  (+ (digit 10 ch) (* 10 exponent)))))
	     (else (complex0 str len off x1 10 #f sign (inexact (* sum (expt 10 (sign exponent))))))))

(make-state complex0 ch (str len off x1 r ex sign num)
	    #f
	    (and num
		 (let ((sign2 (or sign +)))
		   (case ch
		     ((#\i) (and
			     (= (+ 1 off) len)
			     num
			     sign
			     (if (eqv? 0 num)
				 (or x1 0)
				 (make-rectangular (or x1 0) (sign num)))))
		     ((#\-) (num1 str len (+ 1 off) (sign2 num) r ex 0 -))
		     ((#\+)  (num1 str len (+ 1 off) (sign2 num) r ex 0 +))
		     ((#\@) (let ((angle (num1 str len (+ 1 off) #f r ex 0 +)))
			      (and num
				   (number? angle)
				   (= 0 (imag-part angle))
				   (make-polar (sign2 num) angle))))
		     (else #f)))))

(define string->number
  (case-lambda
   ((str)  (prefix0 str (string-length str) 0))
   ((str base) (prefix2 str (string-length str) 0 base #f))))
