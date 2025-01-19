(import (scheme r5rs) (prefix (flow sys) sys:) (scheme case-lambda) (scheme base))

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

(define (base* a b)
  (sys:MUL a b))
(define *
  (case-lambda
   (() 1)
   ((a) a)
   ((a b) (base* a b))
   ((a b c) (base* (base* a b) c))
   (rest (reducer base* 0 rest))))

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
	 (else (error "bad apply len:" len)))))
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

(define (quotient a b) (sys:FOREIGN_CALL "SCM_DIV" a b))
(define (min a b) (if (< a b) a b))
(define (not a) (if a #f #t))
(define (call-with-current-continuation x)
  (sys:FOREIGN_CALL "SCM_CALLCC" x))

(define (car a)
  (unless (pair? a) (error "Trying to car not a pair" a))
  (sys:FOREIGN_CALL "car" a))
(define (cdr a)
  (unless (pair? a) (error "Trying to cdr not a pair" a))
  (sys:FOREIGN_CALL "cdr" a))
(define (cddr a)
  (cdr (cdr a)))
(define (cdar a)
  (cdr (car a)))
(define (caar a)
  (car (car a)))
(define (cadr a)
  (car (cdr a)))
(define (set-car! n v) (sys:FOREIGN_CALL "setcar" n v))
(define (set-cdr! n v) (sys:FOREIGN_CALL "setcdr" n v))
(define (append  n a) (sys:FOREIGN_CALL "append" n a))
(define (cons  n a) (sys:FOREIGN_CALL "cons" n a))
(define (vector-length n) (sys:FOREIGN_CALL "vector_length" n))
(define (make-vector n) (sys:FOREIGN_CALL "make_vector" n))
(define (vector-ref v i) (sys:FOREIGN_CALL "vector_ref" v i))
(define (vector-set! v i val) (sys:FOREIGN_CALL "vector_set" v i val))
(define (display n)
  (sys:FOREIGN_CALL "display" n))
(define (zero? x)
  (= x 0))
(define (newline)
  (display "\n"))
(define (length x)
  (let loop ((n 0) (x x))
    (if (null? x)
	n
	(loop (+ n 1) (cdr x)))))


;; strings
(define (string-length n) (sys:FOREIGN_CALL "SCM_STRING_LENGTH" n))
(define make-string
  (case-lambda
   ((n) (sys:FOREIGN_CALL "SCM_MAKE_STRING" n #f))
   ((n fill) (sys:FOREIGN_CALL "SCM_MAKE_STRING" n fill))))
(define (str-copy tostr tostart fromstr fromstart fromend)
  (let loop ((frompos fromstart) (topos tostart))
    (if (< frompos fromend)
	(begin
	  (string-set! tostr topos (string-ref fromstr frompos))
	  (loop (+ frompos 1) (+ topos 1))))))
(define (string-append2 a b)
  (let* ((lena (string-length a))
	 (lenb (string-length b))
	 (newstr (make-string (+ lena lenb))))
    (str-copy newstr 0 a 0 lena)
    (str-copy newstr lena b 0 lenb)
    newstr))

(define string-append
  (case-lambda
   ((a b)
    (string-append2 a b))
   ((a b c d e)
    (string-append2 a (string-append2 b (string-append2 c (string-append2 d e)))))
   (strs
    (let* ((totallen (apply + (map string-length strs)))
	   (newstr (make-string totallen)))
      (let loop ((strs strs) (place 0))
	(if (not (null? strs))
	    (let* ((cur_str (car strs))
 		   (cur_len (string-length cur_str)))
	      (str-copy newstr place (car strs) 0 cur_len)
	      (loop (cdr strs) (+ place cur_len)))))
      newstr))))

;;; IO
(define write display)

;;; types
(define (boolean? x) (boolean? x))
(define (char? x) (char? x))
(define (null? x) (null? x))
(define (number? x) (or (fixnum? x) (flonum? x)))
(define (pair? x) (pair? x))
(define (procedure? x) (procedure? x))
(define (string? x) (string? x))
(define (symbol? x) (symbol? x))
(define (vector? x) (vector? x))
(define (flonum? x) (sys:FOREIGN_CALL "SCM_IS_FLONUM" x))

;; List
(define (list . x) x)
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
	(if (eq? obj (car list)) 
	    list
	    (loop (cdr list))))))
(define (memq obj list)
  (let loop ((list list))
    (if (null? list) #f
	(if (eqv? obj (car list)) 
	    list
	    (loop (cdr list))))))
(define (member obj list)
  (let loop ((list list))
    (if (null? list) #f
	(if (equal? obj (car list)) 
	    list
	    (loop (cdr list))))))

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
    (unless (any list? lsts) (error "circular for-each"))
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
