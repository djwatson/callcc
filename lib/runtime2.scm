(import (scheme r5rs) (prefix (flow sys) sys:) (scheme case-lambda) (scheme base))

(include "lib/memory_layout.scm")

;;;;;;;;math
(define (negative? p)
  (< p 0))

(define (positive? p)
  (> p 0))
(define (abs p)
  (if (negative? p)
      (- p)
      p))

(define (numerator a)
  (cond
   ((inexact? a) (inexact (numerator (exact a))))
   ((ratnum? a) (sys:FOREIGN_CALL "SCM_NUMERATOR" a))
   (else a)))
(define (denominator x)
  (cond
   ((inexact? x) (inexact (denominator (exact x))))
   ((ratnum? x) (sys:FOREIGN_CALL "SCM_DENOMINATOR" x))
   (else 1)))

(define (sin f)
  (sys:FOREIGN_CALL "SCM_SIN" (inexact f)))
(define (cos f)
  (sys:FOREIGN_CALL "SCM_COS" (inexact f)))
(define (asin f)
  (sys:FOREIGN_CALL "SCM_ASIN" (inexact f)))
(define (acos f)
  (sys:FOREIGN_CALL "SCM_ACOS" (inexact f)))
(define (tan f)
  (sys:FOREIGN_CALL "SCM_TAN" (inexact f)))
(define atan
  (case-lambda
    ((num) (sys:FOREIGN_CALL "SCM_ATAN" (inexact num)))
    ((num1 num2)
     (let ((res (sys:FOREIGN_CALL "SCM_ATAN" (/ (inexact num1) (inexact num2)))))
       (if (< num2 0)
	   (if (or (negative? num1) (eqv? -inf.0 (/ 1.0 num1))) ;; hack to check for -0.0
	       (- res 3.14159265358979)
	       (+ res 3.14159265358979))
	   res)))))
(define (sqrt x)
  (if (negative? x)
      (make-rectangular 0.0 (sqrt (abs x)))
      (sys:FOREIGN_CALL "SCM_SQRT" (inexact x))))
(define (floor-quotient a b)
  (let ((q (/ a b))
	(qq (quotient a b)))
    ;; TODO don't use / and quotient both
    (if (and (< q 0) (not (integer? q)))
        (- qq 1)
        qq)))
(define (floor x)
  (cond
   ((flonum? x) (sys:FOREIGN_CALL "SCM_FLOOR" x))
   ((ratnum? x) (floor-quotient (numerator x) (denominator x)))
   (else x)))
(define (floor x)
  (cond
   ((flonum? x) (sys:FOREIGN_CALL "SCM_FLOOR" x))
   ((ratnum? x) (floor-quotient (numerator x) (denominator x)))
   (else x)))
(define (ceiling x)
  (cond
   ((flonum? x) (sys:FOREIGN_CALL "SCM_CEILING" x))
   ((ratnum? x)
    (let-values (((q r) (floor/ (numerator x) (denominator x))))
      (if (zero? r)
	  q
	  (+ q 1))))
   (else x)))

;; complex
(define (make-polar r angle)
  (make-rectangular  (* r (cos angle)) (* r (sin angle))))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (truncate x)
  (if (negative? x)
      (ceiling x)
      (floor x)))
(define (floor/ a b)
  (let* ((div (floor-quotient a b))
	 (rem (- a (* b div))))
    (values div rem)))
(define (truncate/ a b)
  (let* ((div (truncate (quotient a b)))
	 (rem (- a (* b div))))
    (values div rem)))
(define (rationalize x e)
  ;; Implementation by Alan Bawden.
  (define (simplest-rational x y)
    (define (simplest-rational-internal x y)
      ;; Assumes 0 < X < Y
      (let ((fx (floor x))
            (fy (floor y)))
        (cond ((not (< fx x))
               fx)
              ((= fx fy)
               (+ fx
                  (/ (simplest-rational-internal
                      (/ (- y fy))
                      (/ (- x fx))))))
              (else
               (+ 1 fx)))))
    ;; do some juggling to satisfy preconditions
    ;; of simplest-rational-internal.
    (cond ((< y x)
           (simplest-rational y x))
          ((not (< x y))
           (cond ((rational? x)
                  x)
                 ((and (flonum? x) (not (finite? x)))
                  (if (and (flonum? e) (or (nan? e) (= x e)))
                      +nan.0
                      x))
                 (else
                  (assertion-violation 'rationalize
                                       "Expected a real number"
                                       x e))))
          ((positive? x)
           (simplest-rational-internal x y))
          ((negative? y)
           (- (simplest-rational-internal (- y)
                                          (- x))))
          (else
           (if (and (exact? x) (exact? y))
               0
               0.0))))
  (simplest-rational (- x e) (+ x e)))

(define (exp num) (sys:FOREIGN_CALL "SCM_EXP" (inexact num)))

(define log
  (case-lambda
    ((num) (sys:FOREIGN_CALL "SCM_LOG" (inexact num)))
    ((num base) (/ (sys:FOREIGN_CALL "SCM_LOG" (inexact num))
			 (sys:FOREIGN_CALL "SCM_LOG" (inexact base))))))
(define (reducer f init args)
  (let loop ((init init) (args args))
    (if (pair? args)
	(loop (f init (car args)) (cdr args))
	init)))

(define (base+ a b)
  (sys:ADD a b))
(define +
  (case-lambda
   (() 0)
   ((a) a)
   ((a b) (base+ a b))
   ((a b c) (base+ (base+ a b) c))
   (rest (reducer base+ 0 rest))))

(define (base- a b)
  (sys:SUB a b))
(define -
  (case-lambda
   (() 0)
   ((a) (* -1 a))
   ((a b) (base- a b))
   ((a b c) (base- (base- a b) c))
   (rest (reducer base- 0 rest))))

(define (base* a b)
  (sys:MUL a b))
(define *
  (case-lambda
   (() 1)
   ((a) a)
   ((a b) (base* a b))
   ((a b c) (base* (base* a b) c))
   (rest (reducer base* 0 rest))))

(define (base/ a b)
  (sys:DIV a b))
(define /
  (case-lambda
   ((a) (base/ 1 a))
   ((a b) (base/ a b))
   ((a b c) (base/ (base/ a b) c))
   ((a . rest) (reducer base/ a rest))))

(define (comparer f args)
  (let loop ((args args))
    (if (and (pair? args) (pair? (cdr args)))
	(if (f (car args) (cadr args))
	    (loop (cdr args))
	    #f)
	#t)))

(define <
  (case-lambda
   ((a b) (base< a b))
   (rest
    (comparer (lambda (a b) (base< a b)) rest))))
(define >
  (case-lambda
   ((a b) (base> a b))
   (rest
    (comparer (lambda (a b) (base> a b)) rest))))
(define <=
  (case-lambda
   ((a b) (base<= a b))
   (rest
    (comparer (lambda (a b) (base<= a b)) rest))))
(define >=
  (case-lambda
   ((a b) (base>= a b))
   (rest
    (comparer (lambda (a b) (base>= a b)) rest))))
(define =
  (case-lambda
   ((a b) (base= a b))
   (rest
    (comparer (lambda (a b) (base= a b)) rest))))

(define (base< a b)
  (sys:LT a b))
(define (base<= a b)
  (sys:LTE a b))
(define (base> a b)
  (sys:GT a b))
(define (base>= a b)
  (sys:GTE a b))
(define (base= a b)
  (sys:NUM_EQ a b))


(define (error msg . args)
  (display "Error:" (current-error-port))
  (display msg (current-error-port))
  (display " " (current-error-port))
  (display args (current-error-port))
  (newline (current-error-port))
  (0))
(define apply
  (case-lambda
    ((fun args)
     (let* ((len (length args)))
       (unless (procedure? fun)
	 (error "Applying to not a procedure:" fun))
       (unless (list? args)
	 (error "Apply to non-list" args))
       ;; sys:APPLY must always be in tail position.
       (case len
	 ((0) (fun))
	 ((1) (fun (car args)))
	 ((2) (fun (car args) (cadr args)))
	 ((3) (fun (car args) (cadr args) (caddr args)))
	 ((4) (fun (car args) (cadr args) (caddr args) (cadddr args)))
	 (else
	  (let ((stack-args (cddr (cdddr args))))
	    (let loop ((stack-args stack-args) (i 0))
	      (when (pair? stack-args)
		(sys:FOREIGN_CALL "SCM_WRITE_SHADOW_STACK" i (car stack-args))
		  (loop (cdr stack-args) (+ i 1))))
	    (sys:APPLY (+ 1 len) fun (car args) (cadr args) (caddr args) (cadddr args) (car (cddddr args))))))))
    ((fun . lst)
     (let* ((rlst (reverse lst))
	    (unused (unless (list? (car rlst))
		      (error "Apply to non-list" (car rlst))))
	    (firstargs (reverse (cdr rlst)))
	    (args (append2 firstargs (car rlst))))
       (apply fun args)))))

(define (make-rectangular real imag)
  (sys:FOREIGN_CALL "SCM_MAKE_RECTANGULAR" real imag))

(define (real-part x) (sys:FOREIGN_CALL "SCM_REAL_PART" x))
(define (imag-part x) (sys:FOREIGN_CALL "SCM_IMAG_PART" x))

;; TODO: use bitops
(define (expt num exp)
  (let* ((fl (or (inexact? num) (inexact? exp)))
	 (num (if fl (inexact num) num))
	 (exp (if fl (inexact exp) exp))
	 (start (if fl 1.0 1)))
    (if (> exp 0)
	(let loop ((ret start) (num num) (exp exp))
	  (if (= exp 0)
	      ret
	      (loop (if (odd? exp) (* ret num) ret) (* num num) (quotient exp 2))))
	(let loop ((n start) (cnt exp))
	  (if (= cnt 0) n
	      (loop (/ n num) (+ cnt 1)))))))

(define (square x) (* x x))

(define (remainder a b)
  (sys:MOD a b))
(define (modulo x y)
  (let ((z (remainder x y)))
    (if (negative? y)
	(if (positive? z) (+ z y) z)
	(if (negative? z) (+ z y) z))))

(define min
  (case-lambda
   ((a b)
    (let ((res (if (< a b) a b)))
      (if (or (inexact? a) (inexact? b))
	  (inexact res) res)))
   (args
    (let loop ((args args))
      (if (eq? (length args) 1)
	  (car args)
	  (let* ((a (car args))
		 (b (cadr args))
		 (m (if (> a b) b a))
		 (i (if (or (inexact? a) (inexact? b)) (inexact m) m)))
	    (loop (cons i (cddr args)))))))))

(define max
  (case-lambda
   ((a b)
    (let ((res (if (> a b) a b)))
      (if (or (inexact? a) (inexact? b))
	  (inexact res) res)))
   (args (let loop ((args args))
	   (if (eq? (length args) 1)
	       (car args)
	       (let* ((a (car args))
		      (b (cadr args))
		      (m (if (< a b) b a))
		      (i (if (or (inexact? a) (inexact? b)) (inexact m) m)))
		 (loop (cons i (cddr args)))))))))

(define gcd
  (case-lambda
   (() 0)
   ((a) a)
   ((a b)
    (if (= b 0)
      (abs a)
      (gcd b (remainder a b))))
   (args (let lp ((x (car args)) (ls (cdr args)))
        (if (null? ls) x (lp (gcd x (car ls)) (cdr ls)))))))

(define lcm
  (case-lambda
   (() 1)
   ((a) a)
   ((a b) (abs (quotient (* a b) (gcd a b))))
   (args (let lp ((x (car args)) (ls (cdr args)))
        (if (null? ls) x (lp (lcm x (car ls)) (cdr ls)))))))

(define (odd? x)
  (= 1 (modulo x 2)))

(define (even? x)
  (= 0 (modulo x 2)))
(define (quotient a b) (sys:FOREIGN_CALL "SCM_QUOTIENT" a b))


(define (not a) (if a #f #t))
(define (call-with-current-continuation x)
  (let* ((winds *here*)
	 (res (sys:FOREIGN_CALL "SCM_CALLCC" x)))
    (unless (eq? *here* winds)
      (reroot! winds))
    res))
(define call/cc call-with-current-continuation)

(define (car a)
					;(unless (pair? a) (error "Trying to car not a pair" a))
  (sys:FOREIGN_CALL "SCM_CAR" a))
(define (cdr a)
  ;(unless (pair? a) (error "Trying to cdr not a pair" a))
  (sys:FOREIGN_CALL "SCM_CDR" a))
(define (cddr a)
  (cdr (cdr a)))
(define (cdar a)
  (cdr (car a)))
(define (caar a)
  (car (car a)))
(define (cadr a)
  (car (cdr a)))
(define (set-car! n v) (sys:FOREIGN_CALL "SCM_SETCAR" n v))
(define (set-cdr! n v) (sys:FOREIGN_CALL "SCM_SETCDR" n v))

(define (cons  n a) (sys:FOREIGN_CALL "SCM_CONS" n a))
(define (vector-length n) (sys:FOREIGN_CALL "SCM_VECTOR_LENGTH" n))
(define make-vector
  (case-lambda
   ((len) (sys:FOREIGN_CALL "SCM_MAKE_VECTOR" len))
   ((len obj)
    (let ((vec (sys:FOREIGN_CALL "SCM_MAKE_VECTOR" len)))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(vector-set! vec i obj))
      vec))))
(define (vector-ref v i) (sys:FOREIGN_CALL "SCM_VECTOR_REF" v i))
(define (vector-set! v i val) (sys:FOREIGN_CALL "SCM_VECTOR_SET" v i val))
(define (vector-set-fast! v i val) (sys:FOREIGN_CALL "SCM_VECTOR_SET_FAST" v i val))
(define display
  (case-lambda
   ((n) (display n (current-output-port)))
   ((n port)
    (cond
     ((string? n) (do ((i 0 (+ i 1)))
		      ((= (string-length n) i))
		    (write-char (string-ref n i) port)))
     ((number? n) (display (number->string n) port))
     ((char? n) (write-char n port))
     ((vector? n) (display "#" port) (display (vector->list n) port))
     ((bytevector? n) (display "#u8(" port)
      (do ((i 0 (+ i 1)))
	  ((= i (bytevector-length n)))
	(unless (= i 0)
	  (display " " port))
	(display (bytevector-u8-ref n i) port))
      (display ")" port))
     ((null? n) (display "()" port))
     ;; A pile of stuff just copied from chibi's implementation.
     ((symbol? n)
      (let* ((str (symbol->string n))
	     (bars (or (= 0 (string-length str))
		       (equal? "." str)
		       (char-numeric? (string-ref str 0))
		       (and (> (string-length str) 1)
			    (and (or (eq? #\+ (string-ref str 0))
				     (eq? #\- (string-ref str 0)))
				 (or (char-numeric? (string-ref str 1))
				     (eq? #\. (string-ref str 1))
				     (eq? #\i (string-ref str 1))
				     (and (> (string-length str) 3)
					  (eq? #\n (char-downcase (string-ref str 1)))
					  (eq? #\a (char-downcase (string-ref str 2)))
					  (eq? #\n (char-downcase (string-ref str 3)))))))))
	     (valid (let loop ((i 0))
		      (cond
		       ((= i (string-length str)) #t)
		       ((<= (char->integer (string-ref str i)) (char->integer #\space)) #f)
		       ((memq (string-ref str i ) '(#\( #\) #\" #\| #\newline #\return #\space #\tab #\; #\\ #\# #\,)) #f)
		       (else (loop (+ i 1)))))))
	(if (and (not bars) valid)
	    (display str port)
	    (begin
	      (display "|" port)
	      (do ((i 0 (+ i 1)))
		  ((= i (string-length str)))
		(let ((c (string-ref str i)))
		  (when (memq c '(#\| #\\)) (display "\\" port))
		  (display c port)))
	      (display "|" port)))))
     ((record? n) (display "#<record>" port))
     ((procedure? n) (display "#<closure>" port))
     ((boolean? n) (if n (display "#t" port) (display "#f" port)))
     ;; TODO undefined
     ((pair? n)
      (display "(" port)
      (let loop ((n n))
	(if (not (pair? n)) (begin (display ". " port) (display n port))
	    (begin (display (car n) port) 
		   (if (not (null? (cdr n)))
		       (begin
			 (display " " port)
			 (loop (cdr n)))))))
      (display ")" port))
     (else (sys:FOREIGN_CALL "SCM_DISPLAY" n (port-fd port)) (0)))
    )))

(define (check-shared? shared x port)
  (let ((seen (hash-table-ref/default (cdr shared) x #f)))
    (cond
     ((integer? seen) (display "#" port) (display seen port) (display "#" port) #t)
     (seen (display "#" port) (display (car shared) port)
	   (hash-table-set! (cdr shared) x (car shared))
	   (set-car! shared (+ (car shared) 1))
	   (display "=" port)
	   #f)
     (else #f))))

(define (extract-shared-objects x cyclic-only?)
  (let ((seen (make-hash-table eq?)))
    ;; find shared references
    (let find ((x x))
      (cond ;; only interested in pairs, vectors and records
       ((or (pair? x) (vector? x))
        ;; increment the count
        (hash-table-update!/default seen x (lambda (n) (+ n 1)) 0)
        ;; walk if this is the first time
        (cond
         ((> (hash-table-ref seen x) 1))
         ((pair? x)
	  (find (car x))
	  (find (cdr x)))
         ((vector? x)
	  (do ((i 0 (+ i 1)))
	      ((= i (vector-length x)))
	    (find (vector-ref x i)))))
        ;; delete if this shouldn't count as a shared reference
        (if (and cyclic-only?
                 (<= (hash-table-ref/default seen x 0) 1))
	    (hash-table-delete! seen x)))))
    ;; extract shared references
    (let ((res (make-hash-table eq?)))
      (hash-table-walk
       seen
       (lambda (k v) (if (> v 1) (hash-table-set! res k #t))))
      res)))

(define (write-all arg port type)
  (let ((shared (cons 0 (extract-shared-objects arg (not (eq? type 'shared))))))
    (let write ((arg arg) (port port))
      (cond
       ((null? arg) (display "()" port))
       ((pair? arg)
	(unless (check-shared? shared arg port)
	  (write-char #\( port)
	  (write (car arg) port)
	  (let write-cdr ((arg (cdr arg)))
	    (cond
	     ((and (pair? arg) (not (hash-table-ref/default (cdr shared) arg #f)))
	      (display " " port)
	      (write (car arg) port )
	      (write-cdr (cdr arg)))
	     ((null? arg)
	      (display ")" port))
	     (else
	      (display  " . " port)
	      (write arg port )
	      (display ")" port))))))
       ((vector? arg)
	(unless (check-shared? shared arg port)
	  (display "#" port)
	  (write (vector->list arg) port)))
       ((char? arg)
	(cond
	 ((char=? #\newline arg) (display "#\\newline" port))
	 ((char=? #\tab arg) (display "#\\tab" port))
	 ((char=? #\space arg) (display "#\\space" port))
	 ((char=? #\return arg) (display "#\\return" port))
	 (else (display "#\\" port) (display arg port))))
       ((string? arg)
	(display "\"" port) 
	(for-each 
	 (lambda (chr) 
	   (cond
	    ((char=? #\" chr) (display "\\\"" port))
	    ((char=? #\\ chr) (display "\\\\" port))
	    (else (display chr port))))
	 (string->list arg))
	(display "\"" port))
       (else 
	(display arg port))))))

(define write
  (case-lambda
    ((arg) (write arg (current-output-port)))
    ((arg port) (write-all arg port 'write))))

(define write-simple
  (case-lambda
    ((arg) (write-simple arg (current-output-port)))
    ((arg port) (write-all arg port 'simple))))

(define write-shared
  (case-lambda
    ((arg) (write-shared arg (current-output-port)))
    ((arg port) (write-all arg port 'shared))))

(define (zero? x)
  (= x 0))
(define newline
  (case-lambda
   (() (display "\n"))
   ((port) (display "\n" port))))
(define (length x)
  (let loop ((n 0) (x x))
    (if (null? x)
	n
	(loop (+ n 1) (cdr x)))))
(define (vector-map proc . vecs)
  (let* ((len (apply min (map vector-length vecs)))
	 (vec (make-vector len)))
    (do ((i 0 (+ i 1)))
	((= i len) vec)
      (vector-set!
       vec i
       (apply proc (map (lambda (x) (vector-ref x i)) vecs))))))

;; strings
(define (string-map proc . strs)
  (let* ((len (apply min (map string-length strs)))
	 (str (make-string len)))
    (do ((i 0 (+ i 1)))
	((= i len) str)
      (string-set!
       str i
       (apply proc (map (lambda (x) (string-ref x i)) strs))))))
(define (string-length n) (sys:FOREIGN_CALL "SCM_STRING_LENGTH" n))
(define make-string
  (case-lambda
   ((n) (sys:FOREIGN_CALL "SCM_MAKE_STRING" n #f))
   ((n fill) (sys:FOREIGN_CALL "SCM_MAKE_STRING" n fill))))
(define string-copy!
  (case-lambda
   ((to at from) (string-copy! to at from 0 (string-length from)))
   ((to at from start) (string-copy! to at from start (string-length from)))
   ((to at from start end)
    (let ((to-len (string-length to))
	  (from-len (string-length from)))
      (unless (<= 0 at to-len) (error "to string-copy!" start to-len))
      (unless (and (fixnum? start) (fixnum? end) (fixnum? at)) (error "fix string-copy!" start))
      (unless (or (< -1 start from-len)
		  (= start end)) (error "len string-copy!" start from-len))
      (unless (<= 0 end from-len) (error "end string-copy!" end from-len))
      (when (> start end) (error "start end string-copy!" start end))
      (unless (and (or (< -1 at to-len)
		       (= start end))
		   (<= (+ at (- end start))
		       to-len))
	(error "bad string-copy! at" at start end))
      (sys:FOREIGN_CALL "SCM_STRING_CPY" to at from start end)))))
(define (string-append2 a b)
  (let* ((lena (string-length a))
	 (lenb (string-length b))
	 (newstr (make-string (+ lena lenb))))
    (sys:FOREIGN_CALL "SCM_STRING_CPY" newstr 0 a 0 lena)
    (sys:FOREIGN_CALL "SCM_STRING_CPY" newstr lena b 0 lenb)
    ;; (string-copy! newstr 0 a 0 lena)
    ;; (string-copy! newstr lena b 0 lenb)
    newstr))

(define string-append
  (case-lambda
   ((a b)
    (string-append2 a b))
   (strs
    (let* ((totallen (apply + (map string-length strs)))
	   (newstr (make-string totallen)))
      (let loop ((strs strs) (place 0))
	(if (not (null? strs))
	    (let* ((cur_str (car strs))
 		   (cur_len (string-length cur_str)))
	      (sys:FOREIGN_CALL "SCM_STRING_CPY" newstr place (car strs) 0 cur_len)
	      ;(string-copy! newstr place (car strs) 0 cur_len)
	      (loop (cdr strs) (+ place cur_len)))))
      newstr))))

;; ;;; IO


;; ;;; types
(define (boolean? x) (boolean? x))
(define (char? x) (char? x))
(define (null? x) (null? x))
(define (number? x) (or (fixnum? x) (flonum? x) (bignum? x) (ratnum? x) (compnum? x)))
(define (pair? x) (pair? x))
(define (procedure? x) (procedure? x))
(define (string? x) (string? x))
(define (symbol? x) (symbol? x))
(define (vector? x) (vector? x))
(define (bytevector? x) (bytevector? x))
(define (flonum? x) (sys:FOREIGN_CALL "SCM_IS_FLONUM" x))
(define (complex? x) (number? x))
(define (real? x) (and (number? x) (not (compnum? x))))
(define (rational? x) (and (number? x) (not (compnum? x)) (not (and (flonum? x) (or (sys:FOREIGN_CALL "SCM_ISNAN" x) (sys:FOREIGN_CALL "SCM_ISINF" x))))))

(define (fixnum? x) (fixnum? x))
(define (bignum? x) (sys:FOREIGN_CALL "SCM_IS_BIGNUM" x))
(define (ratnum? x) (sys:FOREIGN_CALL "SCM_IS_RATNUM" x))
(define (compnum? x) (sys:FOREIGN_CALL "SCM_IS_COMPNUM" x))
(define (integer? x) (or (fixnum? x) (bignum? x) (and (ratnum? x) (= 1 (denominator x))) (and (flonum? x) (not (nan? x)) (not (infinite? x)) (= 1 (denominator (exact x))))))
(define (exact-integer? x) (fixnum? x))
(define (exact? x) (or (fixnum? x) (bignum? x) (ratnum? x) (and (compnum? x) (exact? (real-part x)) (exact? (imag-part x)))))
(define (inexact? x) (or (flonum? x)
			 (and (compnum? x) (or (inexact? (real-part x)) (inexact? (imag-part x))))))
(define (nan? x) (or (and (flonum? x) (sys:FOREIGN_CALL "SCM_ISNAN" x))
			   (and (compnum? x) (or (nan? (real-part x)) (nan? (imag-part x))))))
(define (infinite? x) (or (and (flonum? x) (sys:FOREIGN_CALL "SCM_ISINF" x))
			  (and (compnum? x) (or (infinite? (real-part x)) (infinite? (imag-part x))))))
(define (finite? num)
  (or (not (number? num))
      (not (infinite? num))))

(define boolean=?
  (case-lambda
    ((a b) (eq? a b))
    (rest
     (comparer eq? rest))))
(define symbol=?
  (case-lambda
    ((a b) (eq? a b))
    (rest
     (comparer eq? rest))))
(define (exact-integer-sqrt s)
  (unless (and (exact? s)
	       (positive? s))
    (error "not exact" s))
  (if (bignum? s)
      (let ((res (sys:FOREIGN_CALL "SCM_BIGNUM_SQRT" s)))
	(values res (- s (* res res))))
      (if (<= s 1)
	  (values s s)
	  (let* ((x0 (quotient s 2))
		 (x1 (quotient (+ x0 (quotient s x0)) 2)))
	    (let loop ((x0 x0) (x1 x1))
	      (if (< x1 x0)
		  (loop x1 (quotient (+ x1 (quotient s x1)) 2))
		  (values x0 (- s (* x0 x0)))))))))
(define (inexact->exact a)
  (sys:FOREIGN_CALL "SCM_EXACT" a))
(define (exact x) (inexact->exact x))
(define (exact->inexact a)
  (if (fixnum? a)
      (sys:FOREIGN_CALL "SCM_INEXACT" a)
      a))
(define (inexact x) (exact->inexact x))

;; List
(define (append2 a b)
  (let loop ((a a) (b b))
    (if (null? a)
	b
	(cons (car a) (loop (cdr a) b)))))

(define append
  (case-lambda
   ((a b)
    (append2 a b))
   ((a b c) (append a (append b c)))
   ((a b c d) (append a (append b (append c d))))
   (lsts (if (null? lsts) '()
	     (let loop ((lsts lsts))
	       (if (null? (cdr lsts))
		   (car lsts)
		   (let copy ((node (car lsts)))
		     (if (pair? node)
			 (cons (car node) (copy (cdr node)))
			 (loop (cdr lsts))))))))))

(define (reverse lst)
  (let loop ((lst lst) (res '()))
    (if (pair? lst)
	(loop (cdr lst) (cons (car lst) res))
	res)))
(define (list-ref lst n)
  (let loop ((lst lst) (n n))
    (if (zero? n)
	(car lst)
	(loop (cdr lst) (- n 1)))))
(define (list-tail lst k)
  (let loop ((lst lst) (k k))
    (if (> k 0)
	(loop (cdr lst) (- k 1))
	lst)))
(define (list-set! list k obj)
  (if (= k 0)
      (set-car! list obj)
      (list-set! (cdr list) (- k 1) obj)))
(define (list-copy lst)
  (if (pair? lst)
      (cons (car lst) (list-copy (cdr lst)))
      lst))
(define (cons* first . rest)
  (let recur ((x first) (rest rest))
    (if (pair? rest)
        (cons x (recur (car rest) (cdr rest)))
        x)))

(define list
  (case-lambda
   ((a) (cons a '()))
   ((a b) (cons a (cons b '())))
   ((a b c) (cons a (cons b (cons c '()))))
   ((a b c d) (cons a (cons b (cons c (cons d '())))))
   (rest rest)))

(define (list? x)
  (let loop ((fast x) (slow x))
    (or (null? fast)
	(and (pair? fast)
	     (let ((fast (cdr fast)))
	       (or (null? fast)
		   (and (pair? fast)
			(let ((fast (cdr fast))
			      (slow (cdr slow)))
			  (and (not (eq? fast slow))
			       (loop fast slow))))))))))
(define (memv obj list)
  (let loop ((list list))
    (if (null? list) #f
	(if (eqv? obj (car list)) 
	    list
	    (loop (cdr list))))))
(define (memq obj list)
  (let loop ((list list))
    (if (null? list) #f
	(if (eq? obj (car list)) 
	    list
	    (loop (cdr list))))))
(define member
  (case-lambda
   ((obj list) (member obj list equal?))
   ((obj list cmp)
    (let loop ((list list))
      (if (null? list) #f
	  (if (cmp obj (car list)) 
	      list
	      (loop (cdr list))))))))

(define (assq obj1 alist1)
  (let loop ((obj obj1) (alist alist1))
    (if (null? alist) #f
	(begin
	  (if (eq? (caar alist) obj) 
	      (car alist)
	      (loop obj (cdr alist)))))))
(define (assv obj1 alist1)
  (let loop ((obj obj1) (alist alist1))
    (if (null? alist) #f
	(begin
	  (if (eqv? (caar alist) obj) 
	      (car alist)
	      (loop obj (cdr alist)))))))
(define assoc 
  (case-lambda
    ((obj1 alist1 compare)
     (let loop ((obj obj1) (alist alist1))
       (if (null? alist) #f
	   (begin
	     (if (compare (caar alist) obj) 
		 (car alist)
		 (loop obj (cdr alist)))))))
    ((obj alist)
     (assoc obj alist equal?))))

(define for-each
  (case-lambda
   ((proc lst)
    (unless (list? lst) (error "circular for-each"))
    (let loop ((proc proc) (lst lst))
      (unless (null? lst)
	(proc (car lst))
	(loop proc (cdr lst)))))
   ((proc lst1 lst2)
    (unless (or (list? lst1) (list? lst2)) (error "circular for-each"))
    (let loop ((proc proc) (lst1 lst1) (lst2 lst2))
      (if (and  (not (null? lst1)) (not (null? lst2)))
	  (begin
	    (proc (car lst1) (car lst2))
	    (loop proc (cdr lst1) (cdr lst2))))))
   ((proc . lsts)
    ;TODO (unless (any list? lsts) (error "circular for-each"))
    (let loop ((lsts lsts))
      (let ((hds (let loop2 ((lsts lsts))
		   (if (null? lsts)
		       '()
		       (let ((x (car lsts)))
			 (and (not (null? x))
			      (let ((r (loop2 (cdr lsts))))
				(and r (cons (car x) r)))))))))
	(if hds (begin
		  (apply proc hds)
		  (loop
		   (let loop3 ((lsts lsts))
		     (if (null? lsts)
			 '()
			 (cons (cdr (car lsts)) (loop3 (cdr lsts)))))))))))))

(define string-for-each
  (case-lambda
    ((proc str)
     (let ((len (string-length str)))
       (do ((i 0 (+ i 1)) (pos 0 (+ pos 1)))
	   ((= i len))
	 (proc (string-ref str pos)))))
    ((proc . strs)
     (let ((len (apply min (map string-length strs))))
       (do ((i 0 (+ i 1)))
	   ((= i len))
	 (apply proc (map (lambda (x) (string-ref x i)) strs)))))))

(define (vector-for-each proc . vecs)
  (let ((len (apply min (map vector-length vecs))))
    (do ((i 0 (+ i 1)))
	((= i len))
      (apply proc (map (lambda (x) (vector-ref x i)) vecs)))))

(define map
  (case-lambda
   ((f lst)
    (let loop ((f f) (lst lst))
      (if (null? lst) '()
	  (cons (f (car lst)) (loop f (cdr lst))))))
   ((f lst1 lst2)
    (let loop ((f f) (lst1 lst1) (lst2 lst2))
      (if (or (null? lst2) (null? lst1)) '()
	  (cons (f (car lst1) (car lst2)) (loop f (cdr lst1) (cdr lst2))))))
   (lst (let loop ((lsts (cons (cadr lst) (cddr lst))))
	  (let ((hds (let loop2 ((lsts lsts))
		       (if (null? lsts)
			   '()
			   (let ((x (car lsts)))
			     (and (not (null? x))
				  (let ((r (loop2 (cdr lsts))))
				    (and r (cons (car x) r)))))))))
	    (if hds
		(cons
		 (apply (car lst) hds)
		 (loop
		  (let loop3 ((lsts lsts))
		    (if (null? lsts)
			'()
			(cons (cdr (car lsts)) (loop3 (cdr lsts)))))))
		'()))))))



;; r5rs-equal?
(define (equal? a b)
  (cond
   ((eqv? a b) #t)
   ((and (null? a) (null? b)) #t)
   ((and (string? a) (string? b)) (string=? a b))
   ((and (bytevector? a) (bytevector? b)) (bytevector=? a b))
   ((and (symbol? a) (symbol? b)) (string=? (symbol->string a) (symbol->string b)))
   ((and (vector? a) (vector? b)) (equal? (vector->list a) (vector->list b)))
   ((and (pair? a) (pair? b)
	 (equal? (car a) (car b))
	 (equal? (cdr a) (cdr b))) #t)
   (else #f)))
(define (eqv? a b)
  (eqv? a b))

(define (eq? a b)
  (eq? a b))

;; CXR

(define (caddr e) (car (cddr e))) 
(define (cdddr e) (cdr (cddr e))) 
(define (caaar e) (car (caar e)))
(define (cdaar e) (cdr (caar e)))
(define (caadr e) (car (cadr e)))
(define (cdadr e) (cdr (cadr e)))
(define (cadar e) (car (cdar e)))
(define (cddar e) (cdr (cdar e)))

(define (caaddr e) (car (caddr e))) 
(define (cdaddr e) (cdr (caddr e))) 
(define (cadddr e) (car (cdddr e))) 
(define (cddddr e) (cdr (cdddr e))) 
(define (caaaar e) (car (caaar e)))
(define (cdaaar e) (cdr (caaar e)))
(define (cadaar e) (car (cdaar e)))
(define (cddaar e) (cdr (cdaar e)))
(define (caaadr e) (car (caadr e)))
(define (cdaadr e) (cdr (caadr e)))
(define (cadadr e) (car (cdadr e)))
(define (cddadr e) (cdr (cdadr e)))
(define (caadar e) (car (cadar e)))
(define (cdadar e) (cdr (cadar e)))
(define (caddar e) (car (cddar e)))
(define (cdddar e) (cdr (cddar e)))

;;;;;; vector
(define vector
  (case-lambda
   ((a) (let ((v (make-vector 1)))
	  (vector-set-fast! v 0 a)
	  v))
   ((a b)
    (let ((v (make-vector 2)))
      (vector-set-fast! v 0 a)
      (vector-set-fast! v 1 b)
      v))
   ((a b c)
    (let ((v (make-vector 3)))
      (vector-set-fast! v 0 a)
      (vector-set-fast! v 1 b)
      (vector-set-fast! v 2 c)
      v))
   ((a b c d)
    (let ((v (make-vector 4)))
      (vector-set-fast! v 0 a)
      (vector-set-fast! v 1 b)
      (vector-set-fast! v 2 c)
      (vector-set-fast! v 3 d)
      v))
   ((a b c d e)
    (let ((v (make-vector 5)))
      (vector-set-fast! v 0 a)
      (vector-set-fast! v 1 b)
      (vector-set-fast! v 2 c)
      (vector-set-fast! v 3 d)
      (vector-set-fast! v 4 e)
      v))
   ((a b c d e f g h i j k)
    (let ((v (make-vector 11)))
      (vector-set-fast! v 0 a)
      (vector-set-fast! v 1 b)
      (vector-set-fast! v 2 c)
      (vector-set-fast! v 3 d)
      (vector-set-fast! v 4 e)
      (vector-set-fast! v 5 f)
      (vector-set-fast! v 6 g)
      (vector-set-fast! v 7 h)
      (vector-set-fast! v 8 i)
      (vector-set-fast! v 9 j)
      (vector-set-fast! v 10 k)
      v))
   (vals
    (list->vector vals))))

(define vector->list
  (case-lambda
    ((vec) (vector->list vec 0 (vector-length vec)))
    ((vec start) (vector->list vec start (vector-length vec)))
    ((vec start end)
     (unless (fixnum? start) (error "vector->list" start))
     (unless (or (< -1 start (vector-length vec))
		 (= start end)) (error "Bad start vector->list" start (vector-length vec)))
     (unless (<= 0 end (vector-length vec)) (error "Bad end vector->list" end) (vector-length vec))
     (when (> start end) (error "Bad end vector->list" end))
     (let loop ((l (- end 1)) 
		(lst '()))
       (if (not (< l start))
	   (loop (- l 1) (cons (vector-ref vec l) lst))
	   lst)))))

(define string->vector
  (case-lambda
    ((string) (string->vector string 0 (string-length string)))
    ((string start) (string->vector string start (string-length string)))
    ((string start end)
     (unless (fixnum? start) (error "string->vector" start))
     (unless (or (< -1 start (string-length string))
		 (= start end)) (error "Bad start string->vector" start))
     (unless (<= 0 end (string-length string)) (error "Bad end string->vector" end))
     (when (> start end) (error "Bad end string->vector" end))
     (let ((v (make-vector (- end start))))
       (do ((i start (+ i 1))
	    (vpos 0 (+ vpos 1)))
	   ((= i end))
	 (vector-set! v vpos (string-ref string i)))
       v))))

(define vector->string
  (case-lambda
    ((vec) (vector->string vec 0 (vector-length vec)))
    ((vec start) (vector->string vec start (vector-length vec)))
    ((vec start end)
     (unless (and (fixnum? start)
		  (fixnum? end)) (error "vector->string" start))
     (unless (or (< -1 start (vector-length vec))
		 (= start end)) (error "Bad start vector->string" start))
     (unless (<= 0 end (vector-length vec)) (error "Bad end vector->string" end))
     (when (> start end) (error "Bad end vector->string" end))
     (let ((str (make-string (- end start))))
       (do ((i start (+ i 1))
	    (spos 0 (+ spos 1)))
	   ((= i end))
	 (string-set! str spos (vector-ref vec i)))
       str))))

(define vector-copy
  (case-lambda
    ((vec) (vector-copy vec 0 (vector-length vec)))
    ((vec start) (vector-copy vec start (vector-length vec)))
    ((vec start end)
     (unless (and (fixnum? start)
		  (fixnum? end)) (error "vector-copy" start))
     (unless (or (< -1 start (vector-length vec))
		 (= start end)) (error "Bad start vector-copy" start))
     (unless (<= 0 end (vector-length vec)) (error "Bad end vector-copy" end))
     (when (> start end) (error "Bad end vector-copy" end))
     (let ((v (make-vector (- end start))))
       (do ((from start (+ from 1))
	    (to 0 (+ to 1)))
	   ((= from end))
	 (vector-set! v to (vector-ref vec from)))
       v))))

(define (vector-append . vecs)
  (let* ((len (apply + (map vector-length vecs)))
	 (v (make-vector len)))
    (let loop ((pos 0) (vecs vecs))
      (when (pair? vecs)
	(vector-copy! v pos (car vecs))
	(loop (+ pos (vector-length (car vecs))) (cdr vecs))))
    v))

(define vector-copy!
  (case-lambda
    ((to at from) (vector-copy! to at from 0 (vector-length from)))
    ((to at from start) (vector-copy! to at from start (vector-length from)))
    ((to at from start end)
     (unless (and (fixnum? start)
		  (fixnum? end)
		  (fixnum? at)) (error "vector-copy!" start))
     (unless (or (< -1 start (vector-length from))
		 (= start end)) (error "Bad start vector-copy!" start))
     (unless (<= 0 end (vector-length from)) (error "Bad end vector-copy!" end))
     (when (> start end) (error "Bad end vector-copy!" end))
     (if (>= start at)
	 (do ((i start (+ i 1))
	      (out at (+ out 1)))
	     ((= i end) to)
	   (vector-set! to out (vector-ref from i)))
	 (do ((in end (- in 1))
	      (out (+ at (- end start)) (- out 1)))
	     ((= in start) to)
	   (vector-set! to (- out 1) (vector-ref from (- in 1))))))))

(define vector-fill!
  (case-lambda
    ((vec fill ) (vector-fill! vec fill 0 (vector-length vec)))
    ((vec fill start) (vector-fill! vec fill start (vector-length vec)))
    ((vec fill start end)
     (unless (fixnum? start) (error "vector-fill!" start))
     (unless (or (< -1 start (vector-length vec))
		 (= start end)) (error "Bad start vector-fill!" start))
     (unless (<= 0 end (vector-length vec)) (error "Bad end vector-fill!" end))
     (when (> start end) (error "Bad end vector-fill!" end))
     (do ((i start (+ i 1)))
	 ((= i end))
       (vector-set! vec i fill))
     vec)))

(define (list->vector lst)
  (let* ((len (length lst))
	 (v (make-vector len)))
    (do ((i 0 (+ i 1))
	 (p lst (cdr p)))
	((= i len) v)
      (vector-set-fast! v i (car p)))))

;;;;;;;;; char stuff
(define (integer->char i)
  (sys:INTEGER_CHAR i))

(define (char->integer i)
  (sys:CHAR_INTEGER i))
(define char=?
  (case-lambda
   ((a b) (eq? a b))
   (rest
    (comparer eq? rest))))
(define char>?
  (case-lambda
    ((a b) (> (char->integer a) (char->integer b)))
    (rest
     (comparer char>? rest))))
(define char<?
  (case-lambda
    ((a b) (< (char->integer a) (char->integer b)))
    (rest
     (comparer (lambda (a b) (char<? a b)) rest))))
(define char>=?
  (case-lambda
    ((a b) (>= (char->integer a) (char->integer b)))
    (rest
     (comparer (lambda (a b) (char>=? a b)) rest))))
(define char<=?
  (case-lambda
    ((a b) (<= (char->integer a) (char->integer b)))
    (rest
     (comparer (lambda (a b) (char<=? a b)) rest))))
(define char-ci=?
  (case-lambda
    ((a b)
     (unless (and (char? a) (char? b)) (error "not chars:" a b))
     (eq? (char-downcase a) (char-downcase b)))
    (rest
     (comparer (lambda (a b) (char-ci=? a b)) rest))))
(define char-ci>?
  (case-lambda
    ((a b)
     (unless (and (char? a) (char? b)) (error "not chars:" a b))
     (char>? (char-downcase a) (char-downcase b)))
    (rest
     (comparer (lambda (a b) (char-ci>? a b)) rest))))
(define char-ci<?
  (case-lambda
    ((a b)
     (unless (and (char? a) (char? b)) (error "not chars:" a b))
     (char<? (char-downcase a) (char-downcase b)))
    (rest
     (comparer (lambda (a b) (char-ci<? a b)) rest))))
(define char-ci>=?
  (case-lambda
    ((a b)
     (unless (and (char? a) (char? b)) (error "not chars:" a b))
     (char>=? (char-downcase a) (char-downcase b)))
    (rest
     (comparer (lambda (a b) (char-ci>=? a b)) rest))))
(define char-ci<=?
  (case-lambda
    ((a b)
     (unless (and (char? a) (char? b)) (error "not chars:" a b))
     (char<=? (char-downcase a) (char-downcase b)))
    (rest
     (comparer (lambda (a b) (char-ci<=? a b)) rest))))
(define (char-alphabetic? c)
  (sys:FOREIGN_CALL "SCM_CHAR_ALPHABETIC" c))
(define (char-numeric? c)
  (sys:FOREIGN_CALL "SCM_CHAR_NUMERIC" c))
(define (char-upper-case? c)
  (sys:FOREIGN_CALL "SCM_CHAR_UPPERCASE" c))
(define (char-lower-case? c)
  (sys:FOREIGN_CALL "SCM_CHAR_LOWERCASE" c))
(define (char-whitespace? c)
  (sys:FOREIGN_CALL "SCM_CHAR_WHITESPACE" c))

(define (char-downcase c)
  (sys:FOREIGN_CALL "SCM_DOWNCASE" c))
(define (char-foldcase c) (sys:FOREIGN_CALL "SCM_FOLDCASE" c))
(define (char-upcase c) (sys:FOREIGN_CALL "SCM_UPCASE" c))


(define (digit-value ch)
  (unless (char? ch) (error "not a char: "c))
  (let ((n (char->integer ch)))
    (let lp ((lo 0) (hi (- (vector-length zeros) 1)))
      (if (> lo hi)
          #f
          (let* ((mid (+ lo (quotient (- hi lo) 2)))
                 (mid-zero (char->integer (vector-ref zeros mid))))
            (cond
             ((<= mid-zero n (+ mid-zero 9))
              (- n mid-zero))
             ((< n mid-zero)
              (lp lo (- mid 1)))
             (else
              (lp (+ mid 1) hi))))))))
;; Zeros taken from chibi
(define zeros
    '#(#\x0030                ;DIGIT ZERO
       #\x0660                ;ARABIC-INDIC DIGIT ZERO
       #\x06F0                ;EXTENDED ARABIC-INDIC DIGIT ZERO
       #\x07C0                ;NKO DIGIT ZERO
       #\x0966                ;DEVANAGARI DIGIT ZERO
       #\x09E6                ;BENGALI DIGIT ZERO
       #\x0A66                ;GURMUKHI DIGIT ZERO
       #\x0AE6                ;GUJARATI DIGIT ZERO
       #\x0B66                ;ORIYA DIGIT ZERO
       #\x0BE6                ;TAMIL DIGIT ZERO
       #\x0C66                ;TELUGU DIGIT ZERO
       #\x0CE6                ;KANNADA DIGIT ZERO
       #\x0D66                ;MALAYALAM DIGIT ZERO
       #\x0E50                ;THAI DIGIT ZERO
       #\x0ED0                ;LAO DIGIT ZERO
       #\x0F20                ;TIBETAN DIGIT ZERO
       #\x1040                ;MYANMAR DIGIT ZERO
       #\x1090                ;MYANMAR SHAN DIGIT ZERO
       #\x17E0                ;KHMER DIGIT ZERO
       #\x1810                ;MONGOLIAN DIGIT ZERO
       #\x1946                ;LIMBU DIGIT ZERO
       #\x19D0                ;NEW TAI LUE DIGIT ZERO
       #\x1A80                ;TAI THAM HORA DIGIT ZERO
       #\x1A90                ;TAI THAM THAM DIGIT ZERO
       #\x1B50                ;BALINESE DIGIT ZERO
       #\x1BB0                ;SUNDANESE DIGIT ZERO
       #\x1C40                ;LEPCHA DIGIT ZERO
       #\x1C50                ;OL CHIKI DIGIT ZERO
       #\xA620                ;VAI DIGIT ZERO
       #\xA8D0                ;SAURASHTRA DIGIT ZERO
       #\xA900                ;KAYAH LI DIGIT ZERO
       #\xA9D0                ;JAVANESE DIGIT ZERO
       #\xAA50                ;CHAM DIGIT ZERO
       #\xABF0                ;MEETEI MAYEK DIGIT ZERO
       #\xFF10                ;FULLWIDTH DIGIT ZERO
       #\x104A0               ;OSMANYA DIGIT ZERO
       #\x11066               ;BRAHMI DIGIT ZERO
       #\x1D7CE               ;MATHEMATICAL BOLD DIGIT ZERO
       #\x1D7D8               ;MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO
       #\x1D7E2               ;MATHEMATICAL SANS-SERIF DIGIT ZERO
       #\x1D7EC               ;MATHEMATICAL SANS-SERIF BOLD DIGIT ZERO
       #\x1D7F6               ;MATHEMATICAL MONOSPACE DIGIT ZERO
       ))
;;;;;;;;; string
(define (strcmp eq? f a b eq lt gt)
  (let loop ((pos 0) (rema (string-length a)) (remb (string-length b)))
    (cond
     ((and (= rema 0 ) (= remb 0))  eq)
     ((= rema 0)  lt)
     ((= remb 0)  gt)
     ((eq? (string-ref a pos) (string-ref b pos))
      (loop (+ pos 1) (- rema 1) (- remb 1)))
     (else
      (f (string-ref a pos) (string-ref b pos))))))

(define-syntax define-strcmp
  (syntax-rules (strcmp)
    ((_ name (strcmp eq? cmp? a b eq lt gt))
     (define name
       (case-lambda
	((a b) (strcmp eq? cmp? a b eq lt gt))
	(rest
	 (comparer name rest)))))))
(define-strcmp string<? (strcmp char=? char<? a b #f #t #f))
(define-strcmp string>? (strcmp char=? char>? a b #f #f #t))
(define-strcmp string<=? (strcmp char=? char<=? a b #t #t #f))
(define-strcmp string>=? (strcmp char=? char>=? a b #t #f #t))
(define-strcmp string=? (strcmp char=? char=? a b #t #f #f))
(define-strcmp string-ci<?  (strcmp char-ci=? char-ci<? a b #f #t #f))
(define-strcmp string-ci>?  (strcmp char-ci=? char-ci>? a b #f #f #t))
(define-strcmp string-ci<=?  (strcmp char-ci=? char-ci<=? a b #t #t #f))
(define-strcmp string-ci>=?  (strcmp char-ci=? char-ci>=? a b #t #f #t))
(define-strcmp string-ci=?  (strcmp char-ci=? char-ci=? a b #t #f #f))
(define (string-downcase s)
  (let loop ((out '()) (in (string->list s)))
    (cond
     ((null? in) (list->string (reverse out)))
     ((= (char->integer (car in)) #x0130)
      (loop (cons (integer->char #x0307) (cons (integer->char #x0069) out)) (cdr in)))
     (else (loop (cons (char-downcase (car in)) out) (cdr in))))))
(define (string-upcase s)
  (let loop ((out '()) (in (string->list s)))
    (cond
     ((null? in) (list->string (reverse out)))
     ((assv (char->integer (car in)) uppercase-special) =>
      (lambda (x)
	(loop (append (reverse (map integer->char (cadr x))) out) (cdr in))))
     (else (loop (cons (char-upcase (car in)) out) (cdr in))))))
(define (string-foldcase s)
  (string-downcase (string-upcase s)))
(define uppercase-special
'(
(#x00DF (#x0053 #x0053 ))
(#xFB00 (#x0046 #x0046 ))
(#xFB01 (#x0046 #x0049 ))
(#xFB02 (#x0046 #x004C ))
(#xFB03 (#x0046 #x0046 #x0049 ))
(#xFB04 (#x0046 #x0046 #x004C ))
(#xFB05 (#x0053 #x0054 ))
(#xFB06 (#x0053 #x0054 ))
(#x0587 (#x0535 #x0552 ))
(#xFB13 (#x0544 #x0546 ))
(#xFB14 (#x0544 #x0535 ))
(#xFB15 (#x0544 #x053B ))
(#xFB16 (#x054E #x0546 ))
(#xFB17 (#x0544 #x053D ))
(#x0149 (#x02BC #x004E ))
(#x0390 (#x0399 #x0308 #x0301 ))
(#x03B0 (#x03A5 #x0308 #x0301 ))
(#x01F0 (#x004A #x030C ))
(#x1E96 (#x0048 #x0331 ))
(#x1E97 (#x0054 #x0308 ))
(#x1E98 (#x0057 #x030A ))
(#x1E99 (#x0059 #x030A ))
(#x1E9A (#x0041 #x02BE ))
(#x1F50 (#x03A5 #x0313 ))
(#x1F52 (#x03A5 #x0313 #x0300 ))
(#x1F54 (#x03A5 #x0313 #x0301 ))
(#x1F56 (#x03A5 #x0313 #x0342 ))
(#x1FB6 (#x0391 #x0342 ))
(#x1FC6 (#x0397 #x0342 ))
(#x1FD2 (#x0399 #x0308 #x0300 ))
(#x1FD3 (#x0399 #x0308 #x0301 ))
(#x1FD6 (#x0399 #x0342 ))
(#x1FD7 (#x0399 #x0308 #x0342 ))
(#x1FE2 (#x03A5 #x0308 #x0300 ))
(#x1FE3 (#x03A5 #x0308 #x0301 ))
(#x1FE4 (#x03A1 #x0313 ))
(#x1FE6 (#x03A5 #x0342 ))
(#x1FE7 (#x03A5 #x0308 #x0342 ))
(#x1FF6 (#x03A9 #x0342 ))
(#x1F80 (#x1F08 #x0399 ))
(#x1F81 (#x1F09 #x0399 ))
(#x1F82 (#x1F0A #x0399 ))
(#x1F83 (#x1F0B #x0399 ))
(#x1F84 (#x1F0C #x0399 ))
(#x1F85 (#x1F0D #x0399 ))
(#x1F86 (#x1F0E #x0399 ))
(#x1F87 (#x1F0F #x0399 ))
(#x1F88 (#x1F08 #x0399 ))
(#x1F89 (#x1F09 #x0399 ))
(#x1F8A (#x1F0A #x0399 ))
(#x1F8B (#x1F0B #x0399 ))
(#x1F8C (#x1F0C #x0399 ))
(#x1F8D (#x1F0D #x0399 ))
(#x1F8E (#x1F0E #x0399 ))
(#x1F8F (#x1F0F #x0399 ))
(#x1F90 (#x1F28 #x0399 ))
(#x1F91 (#x1F29 #x0399 ))
(#x1F92 (#x1F2A #x0399 ))
(#x1F93 (#x1F2B #x0399 ))
(#x1F94 (#x1F2C #x0399 ))
(#x1F95 (#x1F2D #x0399 ))
(#x1F96 (#x1F2E #x0399 ))
(#x1F97 (#x1F2F #x0399 ))
(#x1F98 (#x1F28 #x0399 ))
(#x1F99 (#x1F29 #x0399 ))
(#x1F9A (#x1F2A #x0399 ))
(#x1F9B (#x1F2B #x0399 ))
(#x1F9C (#x1F2C #x0399 ))
(#x1F9D (#x1F2D #x0399 ))
(#x1F9E (#x1F2E #x0399 ))
(#x1F9F (#x1F2F #x0399 ))
(#x1FA0 (#x1F68 #x0399 ))
(#x1FA1 (#x1F69 #x0399 ))
(#x1FA2 (#x1F6A #x0399 ))
(#x1FA3 (#x1F6B #x0399 ))
(#x1FA4 (#x1F6C #x0399 ))
(#x1FA5 (#x1F6D #x0399 ))
(#x1FA6 (#x1F6E #x0399 ))
(#x1FA7 (#x1F6F #x0399 ))
(#x1FA8 (#x1F68 #x0399 ))
(#x1FA9 (#x1F69 #x0399 ))
(#x1FAA (#x1F6A #x0399 ))
(#x1FAB (#x1F6B #x0399 ))
(#x1FAC (#x1F6C #x0399 ))
(#x1FAD (#x1F6D #x0399 ))
(#x1FAE (#x1F6E #x0399 ))
(#x1FAF (#x1F6F #x0399 ))
(#x1FB3 (#x0391 #x0399 ))
(#x1FBC (#x0391 #x0399 ))
(#x1FC3 (#x0397 #x0399 ))
(#x1FCC (#x0397 #x0399 ))
(#x1FF3 (#x03A9 #x0399 ))
(#x1FFC (#x03A9 #x0399 ))
(#x1FB2 (#x1FBA #x0399 ))
(#x1FB4 (#x0386 #x0399 ))
(#x1FC2 (#x1FCA #x0399 ))
(#x1FC4 (#x0389 #x0399 ))
(#x1FF2 (#x1FFA #x0399 ))
(#x1FF4 (#x038F #x0399 ))
(#x1FB7 (#x0391 #x0342 #x0399 ))
(#x1FC7 (#x0397 #x0342 #x0399 ))
(#x1FF7 (#x03A9 #x0342 #x0399 ))
))
(define (string-ref str idx) (sys:FOREIGN_CALL "SCM_STRING_REF" str idx))
(define (string-set! str idx val) (sys:FOREIGN_CALL "SCM_STRING_SET" str idx val))
(define string->list
  (case-lambda
    ((str) (string->list str 0 (string-length str)))
    ((str start) (string->list str start (string-length str)))
    ((str start end)
     (unless (and (fixnum? start)
		  (fixnum? end)) (error "string->list" start))
     (unless (or (< -1 start (string-length str))
		 (= start end)) (error "Bad start string->list" start))
     (unless (<= 0 end (string-length str)) (error "Bad end string->list" end))
     (when (> start end) (error "Bad end string->list" end))
     (let string->list-loop ((pos start) (buf '()))
       (if (= pos end)
	   (reverse buf)
	   (string->list-loop (+ pos 1) (cons (string-ref str pos) buf)))))))
(define string-fill!
  (case-lambda
    ((string fill) (string-fill! string fill 0 (string-length string)))
    ((string fill start) (string-fill! string fill start (string-length string)))
    ((string fill start end)
     (unless (fixnum? start) (error "string->fill!" start))
     (unless (or (< -1 start (string-length string))
		 (= start end)) (error "Bad start string->fill!" start))
     (unless (<= 0 end (string-length string)) (error "Bad end string->fill!" end))
     (when (> start end) (error "Bad end string->fill!" end))
     (do ((i start (+ i 1)))
	 ((= i end))
       (string-set! string i fill)))))
(define (string . chars) (list->string chars))
(define (list->string chars)
  (let* ((len (length chars))
	 (c (make-string len)))
    (let loop ((i 0) (chars chars))
      (if (< i len)
	  (begin
	    (string-set! c i (car chars))
	    (loop (+ i 1) (cdr chars)))))
    c))
(define string-copy
  (case-lambda
   ((string) (substring string 0 (string-length string)))
   ((string start) (substring string start (string-length string)))
   ((string start end) (substring string start end))))
(define (substring s start end)
  (let ((new (make-string (- end start))))
    (sys:FOREIGN_CALL "SCM_STRING_CPY" new 0 s start end)
    ;(string-copy! new 0 s start end)
    new))
;;;;;; Records
(define (record-set! record index value)
  ;(unless (record? record) (error "record-set!: not a record" record))
  (sys:FOREIGN_CALL "SCM_RECORD_SET" record index value))
(define (record-ref record index)
  ;(unless (record? record) (error "record-ref: not a record" record))
  (sys:FOREIGN_CALL "SCM_RECORD_REF" record index))
(define (make-record sz)
  (sys:FOREIGN_CALL "SCM_MAKE_RECORD" sz))
(define (record? p)
  (sys:GUARD p record-tag))

(define :record-type (make-record 3))
(record-set! :record-type 0 :record-type)	; Its type is itself.
(record-set! :record-type 1 ':record-type)
(record-set! :record-type 2 '(name field-tags))

(define (make-record-type name field-tags)
  (let ((rec (make-record 3)))
    (record-set! rec 0 :record-type)
    (record-set! rec 1 name)
    (record-set! rec 2 field-tags)
    rec))

(define (record-type-name record-type)
  (record-ref record-type 1))

(define (record-type-field-tags record-type)
  (record-ref record-type 2))

(define (field-index type tag)
  (let loop ((i 1) (tags (record-type-field-tags type)))
    (cond ((null? tags)
           (error "record type has no such field" type tag))
          ((eq? tag (car tags))
           i)
          (else
           (loop (+ i 1) (cdr tags))))))
(define (record-constructor type tags)
  (let ((size (length (record-type-field-tags type)))
        (arg-count (length tags))
        (indexes (map (lambda (tag)
                        (field-index type tag))
		      tags)))
    (lambda args
      (if (= (length args)
	     arg-count)
          (let ((new (make-record (+ size 1))))
	    (record-set! new 0 type)
	    (for-each (lambda (arg i)
			(record-set! new i arg))
		      args
		      indexes)
	    new)
          (error "wrong number of arguments to constructor" type args)))))

;;;;;;;;;; delay/promise
(define-record-type promise (%make-promise done? value) promise?
                    (done? promise-done? promise-done-set!)
                    (value promise-value promise-value-set!))

(define (make-promise obj)
  (if (promise? obj) obj
      (%make-promise #t obj)))

(define (force promise)
  (unless (promise? promise) (error "force: not a promise" promise))
  (if (promise-done? promise)
      (promise-value promise)
      (let ((promise* ((promise-value promise))))
        (unless (promise-done? promise)
          (promise-done-set! promise (promise-done? promise*))
          (promise-value-set! promise (promise-value promise*)))
        (force promise))))

(define (call-with-values producer consumer)
  (unless (and (procedure? producer)
	       (procedure? consumer))
    (error "Bad call-with-values" producer consumer))
  (let ((res (producer)))
    (if (and (pair? res) (eq? *values-tag* (car res)))
        (apply consumer (cdr res))
        (consumer res))))

(define *values-tag* (list 'values))

(define (%values ls)
  (if (and (pair? ls) (null? (cdr ls)))
      (car ls)
      (cons *values-tag* ls)))

(define values
  (case-lambda
   (() (list *values-tag*))
   ((a) a)
   ((a b) `(,*values-tag* ,a ,b))
   (ls (%values ls))))

(define *here* (list #f))

(define (dynamic-wind before during after)
  (unless (and (procedure? before)
	       (procedure? during)
	       (procedure? after))
    (error "bad dynamic wind proc:" before during after))
  (let ((here *here*))
    (reroot! (cons (cons before after) here))
    (call-with-values during
      (lambda results (reroot! here) (apply values results)))))

(define (reroot! there)
  (unless (eq? *here* there)
      (reroot! (cdr there))
      (let ((before (caar there))
	    (after (cdar there)))
	(set-car! *here* (cons after before))
	(set-cdr! *here* there)
	(set-car! there #f)
	(set-cdr! there '())
	(set! *here* there) (before))))

(define (round x)
  (cond
   ((flonum? x) (sys:FOREIGN_CALL "SCM_ROUND" x))
   ((ratnum? x)
    (let-values (((q r) (floor/ (numerator x) (denominator x))))
      (let ((half (/ (denominator x) 2)))
	(cond
	 ((> r half) (+ q 1))
	 ((= r half) (if (odd? q) (+ q 1) q))
	 (else q)))))
   (else x)))

;;;;;;;;;;;;;;;;;;; number->string
(define number->string
  (case-lambda
   ((num) (number->string num 10))
   ((num base)
    (unless (and (number? num) (fixnum? base) (<= 1 base 16)) (error "bad number->string" num))
    (let* ((buflen 100)
	   (buffer (make-string buflen)))
      (cond ((flonum? num)
	     (cond
	      ((nan? num) "+nan.0")
	      ((and (infinite? num) (positive? num)) "+inf.0")
	      ((infinite? num) "-inf.0")
	      (else
	       (sys:FOREIGN_CALL "SCM_FLONUM_STR" num))))
	    ((bignum? num) (sys:FOREIGN_CALL "SCM_BIGNUM_STR" num))
	    ((ratnum? num) (sys:FOREIGN_CALL "SCM_RATNUM_STR" num))
	    ((compnum? num) (string-append
			     (number->string (real-part num))
			     (if (not (or (negative? (imag-part num))
					  (nan? (imag-part num))
					  (infinite? (imag-part num))))
				 "+" "")
			     (number->string (imag-part num)) "i"))
	    ((eq? num 0) "0")
	    (else
	     (let ((neg (negative? num)))
	       (let loop ((p buflen) (n (if neg (- 0 num) num)))
		 (cond ((eq? n 0)
			(if neg
 			    (begin
			      (set! p (- p 1))
			      (string-set! buffer p #\-)))
			(substring buffer p buflen))
		       (else
			(let ((q (quotient n base))
			      (r (modulo n base))
			      (p (- p 1)))
			  (string-set! buffer p (integer->char (+ r (if (>= r 10) 55 48))))
			  (loop p q))))))))))))

(define make-list
  (case-lambda
    ((k) (make-list k '()))
    ((k fill) (if (= k 0)
		  '()
		  (cons fill (make-list (- k 1) fill))))))

(include "lib/str2num.scm")

;;;;;;;;;;;;; IO
(define-record-type port (make-port input textual fold-case fd pos len buf fillflush) port?
		    (input input-port?)
		    (textual textual-port?)
		    (fold-case port-fold-case port-fold-case-set!)
		    (fd port-fd port-fd-set!)
		    (pos port-pos port-pos-set!)
		    (len port-len port-len-set!)
		    (buf port-buf port-buf-set!)
		    (fillflush port-fillflush))

(define (output-port? p)
  (not (input-port? p)))

(define (port-fd-fill port)
  (unless (port-fd port) (error "ERROR port fill"))
  (when (< (port-pos port) (port-len port)) (error "Invalid port fill:" (port-pos port) " " (port-len port)))
  (port-pos-set! port 0)
  (port-len-set! port
		 (sys:FOREIGN_CALL "SCM_READ_FD" (port-fd port)
				   (port-buf port))))
(define (port-fd-flush port)
  (unless (port-fd port) (error "ERROR port flush"))
  (sys:FOREIGN_CALL "SCM_WRITE_FD" (port-fd port) (port-pos port) (port-buf port))
  (port-pos-set! port 0))

(define (port-string-flush port)
  (when (= (port-pos port) (string-length (port-buf port)))
    (let* ((buf (port-buf port))
	   (new-buf (make-string (* 2 (string-length buf)))))
      (string-copy! new-buf 0 buf 0 (port-pos port))
      (port-buf-set! port new-buf))))

(define (port-bytevector-flush port)
  (when (= (port-pos port) (bytevector-length (port-buf port)))
    (let* ((buf (port-buf port))
	   (new-buf (make-bytevector (* 2 (bytevector-length buf)))))
      (bytevector-copy! new-buf 0 buf 0 (port-pos port))
      (port-buf-set! port new-buf))))

(define flush-output-port
  (case-lambda
   (() (flush-output-port (current-output-port)))
   ((port) ((port-fillflush port) port))))

(define port-buffer-size 512)

(define *current-input-port* (make-port #t #t #f 0 0 0 (make-string port-buffer-size) port-fd-fill))
(define (current-input-port) *current-input-port*)
(define *current-output-port* (make-port #f #t #f 1 0 port-buffer-size (make-string port-buffer-size) port-fd-flush))
(define (current-output-port) *current-output-port*)
(define *current-error-port* (make-port #f #t #f 2 0 0 (make-string port-buffer-size) port-fd-flush))
(define (current-error-port) *current-error-port*)

(define (binary-port? port) (not (textual-port? port)))
(define (open-input-file file)
  (let ((fd (sys:FOREIGN_CALL "SCM_OPEN_FD" file #t)))
    (when (< fd 0) (error "open-input-file error:" file))
    (make-port #t #t #f fd 0 0 (make-string port-buffer-size) port-fd-fill)))
(define (open-output-file file)
  (let ((fd (sys:FOREIGN_CALL "SCM_OPEN_FD" file #f)))
    (when (< fd 0) (error "open-output-file error:" file))
    (make-port #f #t #f fd 0 port-buffer-size (make-string port-buffer-size) port-fd-flush)))
(define (open-input-string str)
  (make-port #t #t #f #f 0 (string-length str) str (lambda (port) #f)))
(define (open-output-string)
  (make-port #f #t #f #f 0 port-buffer-size (make-string port-buffer-size) port-string-flush))
(define (get-output-string port)
  (substring (port-buf port) 0 (port-pos port)))
(define (get-output-bytevector port)
  (bytevector-copy (port-buf port) 0 (port-pos port)))
(define (open-input-bytevector bv)
  (make-port #t #f #f #f 0 (bytevector-length bv) bv (lambda (port) #f)))
(define (open-output-bytevector)
  (make-port #f #f #f #f 0 port-buffer-size (make-bytevector port-buffer-size) port-bytevector-flush))
(define (with-output-to-file name thunk)
  (let ((file (open-output-file name))
	(old-port *current-output-port*))
    ;; TODO parameterize
    (set! *current-output-port* file)
    (thunk)
    (set! *current-output-port* old-port)
    (close-output-port file)))

(define (close-input-port p)
  (close-port p))
(define (close-output-port p)
  (flush-output-port p)
  (close-port p))
(define (close-port p)
  (when (port-fd p)
    (unless (= 0 (sys:FOREIGN_CALL "SCM_CLOSE_FD" (port-fd p)))
      (error "Close port: error closing fd " (port-fd p))))
  (port-fd-set! p #f))
(define (input-port-open? port)
  (cond
   ((port-fd port) #t)
   ((< (port-pos port) (port-len port)))
   (else #f)))
(define (output-port-open? port)
  (if (or (port-fd port)
	  (eq? (port-fillflush port) port-string-flush)
	  (eq? (port-fillflush port) port-bytevector-flush)) #t #f))

(define (call-with-input-file file l)
  (let* ((p (open-input-file file))
	 (res (l p)))
    (close-input-port p)
    res))

(define (call-with-output-file file l)
  (let* ((p (open-output-file file))
	 (res (l p)))
    (close-output-port p)
    res))

(define-record-type base-eof-object (eof-object) eof-object?)

(define peek-char
  (case-lambda
   (() (peek-char (current-input-port)))
   ((port)
    (if (< (port-pos port) (port-len port))
	(string-ref (port-buf port) (port-pos port))
	(begin
	  ((port-fillflush port) port)
	  (if (< (port-pos port) (port-len port))
	      (peek-char port)
	      (eof-object)))))))

(define read-char
  (case-lambda
   (() (read-char (current-input-port)))
   ((port)
    (if (< (port-pos port) (port-len port))
	(let ((res (string-ref (port-buf port) (port-pos port))))
	  (port-pos-set! port (+ 1 (port-pos port)))
	  res)
	(begin
	  ((port-fillflush port) port)
	  (if (< (port-pos port) (port-len port))
	      (read-char port)
	      (eof-object)))))))

(define read-u8
  (case-lambda
   (() (read-u8 (current-input-port)))
   ((port)
    (if (< (port-pos port) (port-len port))
	(let ((res (bytevector-u8-ref (port-buf port) (port-pos port))))
	  (port-pos-set! port (+ 1 (port-pos port)))
	  res)
	(begin
	  ((port-fillflush port) port)
	  (if (< (port-pos port) (port-len port))
	      (read-u8 port)
	      (eof-object)))))))

(define char-ready?
  (case-lambda
   (() (char-ready? (current-input-port)))
   ((port) (< (port-pos port) (port-len port)))))

(define u8-ready?
  (case-lambda
   (() (u8-ready? (current-input-port)))
   ((port) (< (port-pos port) (port-len port)))))

(define read-string
  (case-lambda
   ((k) (read-string k (current-input-port)))
   ((k port)
    (let ((p (open-output-string)))
      (unless (fixnum? k) (error "read-string not a fixnum" k))
      (let loop ((k k))
	(if (= k 0)
	    (get-output-string p)
	    (let ((c (read-char port)))
	      (if (eof-object? c)
		  (let ((res (get-output-string p)))
		    (if (= 0 (string-length res))
			c
			res))
		  (begin
		    (write-char c p)
		    (loop (- k 1)))))))))))

(define read-bytevector
  (case-lambda
    ((k) (read-bytevector k (current-input-port)))
    ((k port)
     (let ((p (open-output-bytevector)))
       (unless (fixnum? k) (error "read-bytevector not a fixnum" k))
       (let loop ((k k))
	 (if (= k 0)
	     (get-output-bytevector p)
	     (let ((c (read-u8 port)))
	       (if (eof-object? c)
		   (let ((res (get-output-bytevector p)))
		     (if (= 0 (bytevector-length res))
			 c
			 res))
		   (begin
		     (write-u8 c p)
		     (loop (- k 1)))))))))))

(include "lib/read.scm")

(define write-char
  (case-lambda
   ((ch) (write-char ch (current-output-port)))
   ((ch port)
    (when (>= (port-pos port) (port-len port))
      ((port-fillflush port) port))
    (sys:FOREIGN_CALL "SCM_STRING_SET_FAST" (port-buf port) (port-pos port) ch)
    (port-pos-set! port (+ 1 (port-pos port)))
    (when (eq? #\newline ch)
      ((port-fillflush port) port)))))

(define write-u8
  (case-lambda
   ((ch) (write-u8 ch (current-output-port)))
   ((ch port)
    (when (>= (port-pos port) (port-len port))
      ((port-fillflush port) port))
    (bytevector-u8-set! (port-buf port) (port-pos port) ch)
    ;;(sys:FOREIGN_CALL "SCM_STRING_SET_FAST" (port-buf port) (port-pos port) ch)
    (port-pos-set! port (+ 1 (port-pos port))))))

(define read-bytevector!
  (case-lambda
    ((bv) (read-bytevector! bv (current-input-port) 0 (bytevector-length bv)))
    ((bv port) (read-bytevector! bv port 0 (bytevector-length bv)))
    ((bv port start) (read-bytevector! bv port start (bytevector-length bv)))
    ((bv port start end)
     (unless (and (fixnum? start)
		  (fixnum? end)) (error "bad start read-bytevector!" start))
     (unless (or (< -1 start (bytevector-length bv))
		 (= start end)) (error "bad start len read-bytevector!" start))
     (unless (<= 0 end (bytevector-length bv)) (error "bad end read-bytevector!" end))
     (when (> start end) (error "bad end start read-bytevector!" end))
     (let loop ((pos start) (read 0))
       (if (= pos end)
	   read
	   (let ((c (read-u8 port)))
	     (if (eof-object? c)
		 (if (= 0 read)
		     c
		     read)
		 (begin
		   (bytevector-u8-set! bv pos c)
		   (loop (+ pos 1) (+ read 1))))))))))

(define write-bytevector
  (case-lambda
    ((bv) (write-bytevector bv (current-output-port) 0 (bytevector-length bv)))
    ((bv port) (write-bytevector bv port 0 (bytevector-length bv)))
    ((bv port start) (write-bytevector bv port start (bytevector-length bv)))
    ((bv port start end)
     (unless (and (fixnum? start)
		  (fixnum? end)) (error "bad start write-bytevector" start))
     (unless (or (< -1 start (bytevector-length bv))
		 (= start end)) (error "bad start len write-bytevector" start))
     (unless (<= 0 end (bytevector-length bv)) (error "bad end write-bytevector" end))
     (when (> start end) (error "bad end start write-bytevector" end))
     (do ((i start (+ i 1)))
	 ((= end i))
       (write-u8 (bytevector-u8-ref bv i) port)))))

(define (file-exists? name)
  (sys:FOREIGN_CALL "SCM_FILE_EXISTS" name))

(define (delete-file name)
  (sys:FOREIGN_CALL "SCM_DELETE_FILE" name))

(define read-line
  (case-lambda
    (() (read-line (current-input-port)))
    ((port)
     (let ((p (open-output-string)))
       (let loop ((c (read-char port)))
	 (if (or (eof-object? c) (eq? #\newline c))
	     (let ((res (get-output-string p)))
	       (if (and (eof-object? c) (eqv? (string-length res) 0))
		   c
		   res))
	     (begin
	       (write-char c p)
	       (loop (read-char port)))))))))

(define write-string
  (case-lambda
    ((str) (write-string str (current-output-port)))
    ((str port start) (write-string (substring str start (string-length str)) port))
    ((str port start end) (write-string (substring str start end) port))
    ((str port)
     (do ((i 0 (+ i 1)))
	 ((= i  (string-length str)))
       (write-char (string-ref str i) port)))))

(define (with-input-from-file file thunk)
  (let ((p (open-input-file file))
	(old-port *current-input-port*))
    (set! *current-input-port* p)
    (let ((res (thunk)))
      (close-input-port p)
      (set! *current-input-port* old-port)
      res)))

;;;;;;; equals?, hash tables.

(include "lib/hashtable.scm")
(include "lib/equal.scm")

;;;;;;; Symbols
(define (symbol->string a) (sys:FOREIGN_CALL "SCM_SYMBOL_STRING" a))
(define scm-symbol-table '())
(define (string->symbol str)
  (when (null? scm-symbol-table)
    (set! scm-symbol-table (make-hash-table equal? string-hash))
    (let ((table (sys:FOREIGN_CALL "SCM_GET_SYM_TABLE")))
      (for-each (lambda (x) (hash-table-set! scm-symbol-table (symbol->string x) x)) (vector->list table))))
  (cond
   ((hash-table-ref/default scm-symbol-table str #f))
   (else (let* ((strcopy (string-copy str))
		(new-sym (sys:FOREIGN_CALL "SCM_MAKE_SYMBOL" strcopy)))
	   (hash-table-set! scm-symbol-table strcopy new-sym)
	   new-sym))))

;;;;;;;; parameters
(define (make-parameter init . val)
  (let ((cell init))
    (lambda arg
      (if (pair? arg)
	  (set! cell (car arg))
	  cell))))

;; process context
(define (command-line) (sys:FOREIGN_CALL "SCM_COMMAND_LINE"))
;; TODO exit codes, dynamic unwinding
(define (exit) (sys:FOREIGN_CALL "SCM_EXIT" 0))
(define emergency-exit exit)
(define (get-environment-variables) (sys:FOREIGN_CALL "SCM_GET_ENV_VARS"))
(define (get-environment-variable var)
  (cond
   ((assoc var (get-environment-variables)) => cdr)
   (else #f)))


;;; time
(define (current-jiffy) (sys:FOREIGN_CALL "SCM_CURRENT_JIFFY"))
(define (jiffies-per-second) 1000000)
(define (current-second) (sys:FOREIGN_CALL "SCM_CURRENT_SECOND"))
(define feature-flags '(r7rs exact-closed exact-complex ieee-float
	 full-unicode ratios callcc))
(define (features) feature-flags)

(define (system . args)
  (sys:FOREIGN_CALL "SCM_SYSTEM" (apply string-append args)))

;; utf8
;; bytevectors
(define utf8->string
  (case-lambda
    ((bytevector) (utf8->string bytevector 0 (bytevector-length bytevector)))
    ((bytevector start) (utf8->string bytevector start (bytevector-length bytevector)))
    ((bytevector start end)
     (unless (and (fixnum? start) (fixnum? end)) (error "bad start utf8-string" start))
     (unless (or (< -1 start (bytevector-length bytevector))
		 (= start end)) (error "bad start len utf8-string" start))
     (unless (<= 0 end (bytevector-length bytevector)) (error "bad end utf8-string" end))
     (when (> start end) (error "bad end start utf8-string" end))
     (sys:FOREIGN_CALL "SCM_UTF8_STRING" (bytevector-copy bytevector start end)))))
(define string->utf8
  (case-lambda
    ((string) (string->utf8 string 0 (string-length string)))
    ((string start) (string->utf8 string start (string-length string)))
    ((string start end)
     ;; Count codepoints, ending at NULL terminator
     (unless (and (fixnum? start) (fixnum? end)) (error "string->utf8" start))
     (unless (or (< -1 start (string-length string))
		 (= start end)) (error "string->utf8" start))
     (unless (<= 0 end (string-length string)) (error "string->utf8" end))
     (when (> start end) (error "string->utf8" end))
     ;; TODO: remove copy.
     (sys:FOREIGN_CALL "SCM_STRING_UTF8" (substring string start end)))))
(define (bytevector-u8-ref bv i) (sys:FOREIGN_CALL "SCM_BYTEVECTOR_REF" bv i))
(define (bytevector-length bv) (sys:FOREIGN_CALL "SCM_BYTEVECTOR_LENGTH" bv))
(define (bytevector-u8-set! bv i val) (sys:FOREIGN_CALL "SCM_BYTEVECTOR_SET" bv i val))
(define make-bytevector
  (case-lambda
   ((len) (make-bytevector len 0))
   ((len init)
    (sys:FOREIGN_CALL "SCM_MAKE_BYTEVECTOR" len init))))
(define (bytevector . rest)
  (let ((res (make-bytevector (length rest))))
    (do ((i 0 (+ i 1)) (v rest (cdr v)))
	((= i (length rest)) res)
      (unless (fixnum? (car v)) (error "Bad bytevector set:"))
      (bytevector-u8-set! res i (car v)))
    res))
(define %bytevector-copy 
  (case-lambda
    ((bytevector) (%bytevector-copy bytevector 0 (bytevector-length bytevector)))
    ((bytevector start) (%bytevector-copy bytevector start (bytevector-length bytevector)))
    ((bytevector start end)
     (let ((bv (make-bytevector (- end start))))
	  (do ((in start (+ in 1))
	       (out 0 (+ out 1)))
	      ((= in end) bv)
	    (bytevector-u8-set! bv out (bytevector-u8-ref bytevector in)))))))
(define bytevector-copy
  (case-lambda
    ((bytevector) (bytevector-copy bytevector 0 (bytevector-length bytevector)))
    ((bytevector start) (bytevector-copy bytevector start (bytevector-length bytevector)))
    ((bytevector start end)
     (unless (and (fixnum? start)
		  (fixnum? end)) (error "bad start bytevector-copy" start))
     (unless (or (< -1 start (bytevector-length bytevector))
		 (= start end)) (error "bad start len bytevector-copy" start))
     (unless (<= 0 end (bytevector-length bytevector)) (error "bad end bytevector-copy" end))
     (when (> start end) (error "bad end start bytevector-copy" end))
     (%bytevector-copy bytevector start end))))
(define %bytevector-copy!
  (case-lambda
    ((to at from) (%bytevector-copy! to at from 0 (bytevector-length from)))
    ((to at from start) (%bytevector-copy! to at from start (bytevector-length from)))
    ((to at from start end)
     (if (>= start at)
	 (do ((in start (+ in 1))
	      (out at (+ out 1)))
	     ((= in end) to)
	   (bytevector-u8-set! to out (bytevector-u8-ref from in)))
	 (do ((in end (- in 1))
	      (out (+ at (- end start)) (- out 1)))
	     ((= in start) to)
	   (bytevector-u8-set! to (- out 1) (bytevector-u8-ref from (- in 1))))))))
(define bytevector-copy!
  (case-lambda
    ((to at from) (bytevector-copy! to at from 0 (bytevector-length from)))
    ((to at from start) (bytevector-copy! to at from start (bytevector-length from)))
    ((to at from start end)
     (unless (and (fixnum? start)
		  (fixnum? end)
		  (fixnum? at)) (error "bytevector-copy" start))
     (unless (or (< -1 start (bytevector-length from))
		 (= start end)) (error "Bad start bytevector-copy" start))
     (unless (<= 0 end (bytevector-length from)) (error "Bad end bytevector-copy" end (bytevector-length from)))
     (when (> start end) (error "Bad end start bytevector-copy" start end))
     (unless (and (or (< -1 at (bytevector-length to))
		      (= start end))
		  (<= (+ at (- end start))
		      (bytevector-length to)))
       (error "Bad bytevector-copy! destination"))
     (%bytevector-copy! to at from start end))))

(define (bytevector-append . bvs)
  (let* ((len (apply + (map bytevector-length bvs)))
	 (bv (make-bytevector len)))
    (let loop ((pos 0) (bvs bvs))
      (when (pair? bvs)
	(bytevector-copy! bv pos (car bvs))
	(loop (+ pos (bytevector-length (car bvs))) (cdr bvs)))
      bv)))
(define (bytevector=? a b)
  (and (= (bytevector-length a) (bytevector-length b))
       (let loop ((pos 0))
	 (or (= pos (bytevector-length a))
	     (and (eq? (bytevector-u8-ref a pos) (bytevector-u8-ref b pos))
		  (loop (+ pos 1)))))))
