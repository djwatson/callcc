(import (scheme r5rs) (prefix (flow sys) sys:) (scheme case-lambda) (scheme base))

(include "memory_layout.scm")

;;;;;;;;math 
(define (negative? p)
  (< p 0))

(define (positive? p)
  (> p 0))
(define (abs p)
  (if (negative? p)
      (- p)
      p))

(define (numerator x) x)
(define (denominator x) 1)

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
(define (atan f)
  (sys:FOREIGN_CALL "SCM_ATAN" (inexact f)))
(define (sqrt f)
  (sys:FOREIGN_CALL "SCM_SQRT" (inexact f)))
(define (floor f)
  (sys:FOREIGN_CALL "SCM_FLOOR" (inexact f)))
(define (ceiling f)
  (sys:FOREIGN_CALL "SCM_CEILING" (inexact f)))

(define (truncate x)
  (if (negative? x)
      (ceiling x)
      (floor x)))

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
   ((a) (base/ (inexact 1) a))
   ((a b) (base/ (inexact a) b))
   ((a b c) (base/ (base/ (inexact a) b) c))
   ((a . rest) (reducer base/ (inexact a) rest))))

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
  (display "Error:")
  (display msg)
  (display " ")
  (display args)
  (newline)
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

  (define (expt num exp)
    (let loop ((n 1) (cnt exp))
      (if (= cnt 0) n
	  (loop (* num n) (- cnt 1)))))

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
(define (quotient a b) (sys:FOREIGN_CALL "SCM_DIV" a b))


(define (not a) (if a #f #t))
(define (call-with-current-continuation x)
  (let* ((winds *here*)
	 (res (sys:FOREIGN_CALL "SCM_CALLCC" x)))
    (reroot! winds)
    res))

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
     ((vector? n) (display (vector->list n) port))
     ((null? n) (display "()" port))
     ((symbol? n) (display (symbol->string n) port))
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

(define write
  (case-lambda
   ((arg) (write arg (current-output-port)))
   ((arg port)
    (cond
     ((null? arg) (display "()" port))
     ((pair? arg)
      (display "(" port)
      (let loop ((arg arg))
	(if (not (pair? arg)) (begin (display ". " port) (write arg port))
	    (begin (write (car arg) port) 
		   (if (not (null? (cdr arg)))
		       (begin
			 (display " " port)
			 (loop (cdr arg)))))))
      (display ")" port))
     ((vector? arg)
      (display "#" port)
      (write (vector->list arg) port))
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
(define (string-copy! tostr tostart fromstr fromstart fromend)
  (let loop ((frompos fromstart) (topos tostart))
    (if (< frompos fromend)
	(begin
	  (string-set! tostr topos (string-ref fromstr frompos))
	  (loop (+ frompos 1) (+ topos 1))))))
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
   ((a b c)
    (string-append2 a (string-append2 b c)))
   ((a b c d e)
    (string-append2 a (string-append2 b (string-append2 c (string-append2 d e)))))
   (strs
    (let* ((totallen (apply + (map string-length strs)))
	   (newstr (make-string totallen)))
      (let loop ((strs strs) (place 0))
	(if (not (null? strs))
	    (let* ((cur_str (car strs))
 		   (cur_len (string-length cur_str)))
	      (string-copy! newstr place (car strs) 0 cur_len)
	      (loop (cdr strs) (+ place cur_len)))))
      newstr))))

;; ;;; IO


;; ;;; types
(define (boolean? x) (boolean? x))
(define (char? x) (char? x))
(define (null? x) (null? x))
(define (number? x) (or (fixnum? x) (flonum? x) (bignum? x)))
(define (pair? x) (pair? x))
(define (procedure? x) (procedure? x))
(define (string? x) (string? x))
(define (symbol? x) (symbol? x))
(define (vector? x) (vector? x))
(define (flonum? x) (sys:FOREIGN_CALL "SCM_IS_FLONUM" x))
(define (complex? x) (number? x))
(define (real? x) (number? x))
(define (rational? x) (fixnum? x))
(define write display)

(define (fixnum? x) (fixnum? x))
(define (bignum? x) (sys:FOREIGN_CALL "SCM_IS_BIGNUM" x))
(define (integer? x) (fixnum? x))
(define (exact-integer? x) (fixnum? x))
(define (exact? x) (or (fixnum? x) (bignum? x)))
(define (inexact? x) (flonum? x))

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
;; (define (append2 a b)
;;   (if (null? a)
;;       b
;;       (let ((cell (cons (car a) '())))
;; 	(let loop ((a (cdr a)) (b b) (head cell) (tail cell))
;; 	  (if (null? a)
;; 	      (begin
;; 		(set-cdr! tail b)
;; 		head)
;; 	      (let ((cell (cons (car a) '())))
;; 		(set-cdr! tail cell)
;; 		(loop (cdr a) b head cell)))))))

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
  (or (eq? a b) (and (flonum? a) (flonum? b) (= a b))))

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

(define (vector->list vec)
  (let loop ((i (vector-length vec)) (l '()))
    (if (= i 0)
	l
	(loop (- i 1) (cons (vector-ref vec (- i 1)) l)))))


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
(define (char-ci=? x y)
  (char=? (char-downcase x) (char-downcase y)))
(define (char-ci>? x y)
  (char>? (char-downcase x) (char-downcase y)))
(define (char-ci<? x y)
  (char<? (char-downcase x) (char-downcase y)))
(define (char-ci>=? x y)
  (char>=? (char-downcase x) (char-downcase y)))
(define (char-ci<=? x y)
  (char<=? (char-downcase x) (char-downcase y)))
(define (char-alphabetic? c)
  (let ((n (char->integer c)))
    (cond ((< n #x41) #f)		; A
	  ((> n #x7a) #f)		; z
	  ((> n #x60))		; a-1
	  ((< n #x5b))		; Z+1
	  (else #f))))
(define (char-numeric? c)
  (let ((n (char->integer c)))
    (cond ((< n #x30) #f)		; 0
	  ((> n #x39) #f)		; 9
	  (else #t))))
(define (char-upper-case? c)
  (let ((n (char->integer c)))
    (cond ((< n #x41) #f)		; A
	  ((> n #x5a) #f)		; Z
	  (else #t))))

(define (char-lower-case? c)
  (let ((n (char->integer c)))
    (cond ((< n #x61) #f)		; a
	  ((> n #x7a) #f)		; z
	  (else #t))))

(define (char-downcase c) 
  (let ((n (char->integer c)))
    (if (or (< n #x41)		; A
	    (> n #x5a))		; Z
	(integer->char n)
	(integer->char (+ n 32)))))

(define (char-upcase c)
  (let ((n (char->integer c)))
    (if (or (< n #x61)		; a
	    (> n #x7a))		; z
	(integer->char n)
	(integer->char (- n 32)))))

(define (char-whitespace? c)
  (let ((n (char->integer c)))
    (or (eq? n 32) (eq? n 9) (eq? n 12) (eq? n 10) (eq? n 13))))
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

(define (string<? a b) (strcmp char=? char<? a b #f #t #f))
(define (string>? a b) (strcmp char=? char>? a b #f #f #f))
(define (string<=? a b) (strcmp char=? char<=? a b #t #t #f))
(define (string>=? a b) (strcmp char=? char>=? a b #t #f #f))
(define (string-ci<? a b) (strcmp char-ci=? char-ci<? a b #f #t #f))
(define (string-ci>? a b) (strcmp char-ci=? char-ci>? a b #f #f #f))
(define (string-ci<=? a b) (strcmp char-ci=? char-ci<=? a b #t #t #f))
(define (string-ci>=? a b) (strcmp char-ci=? char-ci>=? a b #t #f #f))
(define (string-ci=? a b) (strcmp char-ci=? char-ci=? a b #t #f #f))
(define (string=? a b) (strcmp char=? char=? a b #t #f #f))
(define (string-ref str idx) (sys:FOREIGN_CALL "SCM_STRING_REF" str idx))
(define (string-set! str idx val) (sys:FOREIGN_CALL "SCM_STRING_SET" str idx val))
(define (string->list str)
  (let ((n (string-length str)))
    (let loop ((i (- n 1)) (lst '()))
      (if (< i 0)
	  lst
	  (loop (- i 1) (cons (string-ref str i) lst))))))
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

(define (round d)
  (sys:FOREIGN_CALL "SCM_ROUND" (inexact d)))

;;;;;;;;;;;;;;;;;;; number->string
(define number->string
  (case-lambda
   ((num) (number->string num 10))
   ((num base)
    (unless (and (number? num) (fixnum? base) (<= 1 base 16)) (error "bad number->string" num))
    (let* ((buflen 100)
	   (buffer (make-string buflen)))
      (cond ((flonum? num) (sys:FOREIGN_CALL "SCM_FLONUM_STR" num))
	    ((bignum? num) (sys:FOREIGN_CALL "SCM_BIGNUM_STR" num))
	    ;; ((ratnum? num) (error "numbratnum->str" num))
	    ;; ((compnum? num) (string-append
	    ;; 		     (number->string (real-part num))
	    ;; 		     (if (not (or (negative? (imag-part num))
	    ;; 				  (nan? (imag-part num))
	    ;; 				  (infinite? (imag-part num))))
	    ;; 			 "+" "")
	    ;; 		     (number->string (imag-part num)) "i"))
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

(include "str2num.scm")

;;;;;;;;;;;;; IO
(define-record-type port (make-port input fold-case fd pos len buf fillflush) port?
		    (input input-port?)
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

(define (flush-output-port port)
  ((port-fillflush port) port))

(define port-buffer-size 512)

(define *current-input-port* (make-port #t #f 0 0 0 (make-string port-buffer-size) port-fd-fill))
(define (current-input-port) *current-input-port*)
(define *current-output-port* (make-port #f #f 1 0 port-buffer-size (make-string port-buffer-size) port-fd-flush))
(define (current-output-port) *current-output-port*)
(define *current-error-port* (make-port #t #f 2 0 0 (make-string port-buffer-size) port-fd-flush))
(define (current-error-port) *current-error-port*)

(define (open-input-file file)
  (let ((fd (sys:FOREIGN_CALL "SCM_OPEN_FD" file #t)))
    (when (< fd 0) (error "open-input-file error:" file))
    (make-port #t #f fd 0 0 (make-string port-buffer-size) port-fd-fill)))
(define (open-output-file file)
  (let ((fd (sys:FOREIGN_CALL "SCM_OPEN_FD" file #f)))
    (when (< fd 0) (error "open-output-file error:" file))
    (make-port #f #f fd 0 port-buffer-size (make-string port-buffer-size) port-fd-flush)))
(define (open-output-string)
  (make-port #f #f #f 0 port-buffer-size (make-string port-buffer-size) port-string-flush))
(define (get-output-string port)
  (substring (port-buf port) 0 (port-pos port)))

(define (close-input-port p)
  (close-port p))
(define (close-output-port p)
  (close-port p))
(define (close-port p)
  (when (port-fd p)
    (unless (= 0 (sys:FOREIGN_CALL "SCM_CLOSE_FD" (port-fd p)))
      (error "Close port: error closing fd " (port-fd p))))
  (port-fd-set! p #f))

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

(define-record-type eof-object (make-eof-object) eof-object?)

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
	      (make-eof-object)))))))

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
	      (make-eof-object)))))))

(include "read.scm")

(define write-char
  (case-lambda
   ((ch) (write-char ch (current-output-port)))
   ((ch port)
    (when (>= (port-pos port) (port-len port))
      ((port-fillflush port) port))
    (string-set! (port-buf port) (port-pos port) ch)
    (port-pos-set! port (+ 1 (port-pos port)))
    (when (eq? #\newline ch)
      ((port-fillflush port) port)))))

(define (file-exists? name)
  (sys:FOREIGN_CALL "SCM_FILE_EXISTS" name))

(define (delete-file name)
  (sys:FOREIGN_CALL "SCM_FILE_EXISTS" name))

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

(include "hashtable.scm")
(include "equal.scm")

;;;;;;; Symbols
(define (symbol->string a) (sys:FOREIGN_CALL "SCM_SYMBOL_STRING" a))
(define scm-symbol-table '())
(define (string->symbol str)
  (when (null? scm-symbol-table)
    (set! scm-symbol-table (make-hash-table string-hash equal?))
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

(define (command-line) '("test" "test.scm"))




