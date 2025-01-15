(import (scheme base)
	  (scheme r5rs)
	  (scheme case-lambda)
	  (prefix (flow sys) sys:))

(include "memory_layout.scm")

(define (not a)
  (if a #f #t))
(define display
  (case-lambda
   ((x) (display x (current-output-port)))
   ((x port)
    (cond
     ((flonum? x) (sys:WRITE x (port-fd port)))
     ((number? x) (display (number->string x) port))
     ((char? x) (write-char x port))
     ((vector? x)
      (display "#(" port)
      (do ((i 0 (+ i 1)))
	  ((= i (vector-length x)))
	(unless (= i 0) (write-char #\space port))
	(display (vector-ref x i) port))
      (write-char #\) port))
     ((pair? x)
      (write-char #\( port)
      (let loop ((cur x))
	(if (pair? (cdr cur))
	    (begin
	      (display (car cur) port)
	      (write-char #\space port)
	      (loop (cdr cur)))
	    (begin
	      (display (car cur) port)
	      (unless (eq? '() (cdr cur))
		(display " . " port)
		(display (cdr cur) port)))))
      (write-char #\) port))
     ((symbol? x) (display (symbol->string x) port))
     ;; TODO lookup name
     ((procedure? x) (display "#<procedure>" port))
     ((eq? x #t) (display "#t" port))
     ((eq? x #f) (display "#f" port))
     ((eq? x '()) (display "()" port))
     ((eof-object? x) (display "#<eof>" port))
     ((port? x) (display "#<port>" port))
     ((record? x) (display "#<record>" port))
     ((string? x) (do ((i 0 (+ i 1)))
		      ((= i (string-length x)))
		    (write-char (string-ref x i) port)))
     (else (error "Bad type in display: " x))))))

(define newline
  (case-lambda
   (() (newline (current-output-port)))
   ((port)
    (display #\newline port))))

 (define (error str . rest)
   (sys:WRITE "ERROR:" (port-fd (current-error-port)))
   (sys:WRITE str (port-fd (current-error-port)))
   (sys:WRITE rest (port-fd (current-error-port)))
   (sys:WRITE #\newline (port-fd (current-error-port)))
  (0))
(define (inexact->exact a)
  (if (flonum? a) 
      (sys:EXACT a)
      a))
(define exact inexact->exact)
(define (exact->inexact a)
  (if (fixnum? a)
      (sys:INEXACT a)
      a))
(define inexact exact->inexact)

(define (flonum? a)
  (sys:GUARD a flonum-tag))
(define (fixnum? a)
  (sys:GUARD a fixnum-tag))



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
(define (base+ a b)
  (sys:ADD a b))
(define (base- a b)
  (sys:SUB a b))
(define (base* a b)
  (sys:MUL a b))
(define (base/ a b)
  (sys:DIV a b))
(define (quotient a b)
  (sys:DIV a b))
(define (remainder a b)
  (sys:MOD a b))
(define (modulo x y)
  (let ((z (remainder x y)))
    (if (negative? y)
	(if (positive? z) (+ z y) z)
	(if (negative? z) (+ z y) z))))

(define (reducer f init args)
  (let loop ((init init) (args args))
    (if (pair? args)
	(loop (f init (car args)) (cdr args))
	init)))

(define +
  (case-lambda
   (() 0)
   ((a) a)
   ((a b) (base+ a b))
   ((a b c) (base+ (base+ a b) c))
   (rest (reducer base+ 0 rest))))

(define -
  (case-lambda
   ((a) (base* -1 a))
   ((a b) (base- a b))
   ((a . rest) (reducer base- a rest))))

(define *
  (case-lambda
   (() 1)
   ((a) a)
   ((a b) (base* a b))
   ((a b c) (* (* a b) c))
   (rest (reducer base* 1  rest))))

(define /
  (case-lambda
   ((a) (base/ 1.0 a))
   ((a b) (base/ a b))))

(define (pp a)
  (sys:WRITE a (current-error-port))
  (sys:WRITE #\newline (current-error-port)))

(define (car a)
  (unless (pair? a) (error "Trying to car not a pair" a))
  (sys:LOAD a 0))
(define (cdr a)
  (unless (pair? a) (error "Trying to cdr not a pair" a))
  (sys:LOAD a 1))
(define (cddr a)
  (cdr (cdr a)))
(define (cdar a)
  (cdr (car a)))
(define (caar a)
  (car (car a)))
(define (cadr a)
  (car (cdr a)))

(define (list-ref lst n)
  (let loop ((lst lst) (n n))
    (if (zero? n)
	(car lst)
	(loop (cdr lst) (- n 1)))))

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

(define (null? a)
  (sys:GUARD a nil-tag))
(define (pair? a)
  (sys:GUARD a cons-tag))
(define (boolean? a)
  (sys:GUARD a bool-tag))
(define (char? a)
  (sys:GUARD a char-tag))
(define (number? a)
  (or (fixnum? a) (flonum? a)))
(define (symbol? a)
  (sys:GUARD a symbol-tag))
(define (procedure? a)
  (sys:GUARD a closure-tag))
(define (vector? a)
  (sys:GUARD a vector-tag))
(define (string? a)
  (sys:GUARD a string-tag))
(define bytevector? string?)
(define (length a)
  (let loop ((len 0) (a a))
    (if (pair? a)
	(loop (+ len 1) (cdr a))
	len)))
(define (cons a b)
  (let ((cell (sys:ALLOC 24 cons-tag)))
    (sys:STORE cell a 0)
    (sys:STORE cell b 1)
    cell))
(define (set-car! cell a)
  (unless (pair? cell) (error "Trying to set-car not a pair" a))
  (sys:STORE cell a 0))
(define (set-cdr! cell a)
  (unless (pair? cell) (error "Trying to set-cdr not a pair" a))
  (sys:STORE cell a 1))

(define (string-length a)
  (unless (string? a) (error "string-length: not a string" a))
  (sys:LOAD a 0))

(define (string-ref a p)
  (unless (string? a) (error "string-ref: not a string" a))
  (sys:LOAD_CHAR a p))

(define (string-set! s p v)
  (sys:STORE_CHAR s v p)
  ;; TODO: return undefined OK?
  0)

(define (str-copy tostr tostart fromstr fromstart fromend)
  (let loop ((frompos fromstart) (topos tostart))
    (if (< frompos fromend)
	(begin
	  (string-set! tostr topos (string-ref fromstr frompos))
	  (loop (+ frompos 1) (+ topos 1))))))

(define make-string
  (case-lambda
   ((len) (make-string len #f))
   ((len c)
    (let ((str (sys:ALLOC (+ len 17) string-tag)))
    (sys:STORE str len 0)
    (string-set! str len #\x00)
    (when c
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(string-set! str i c)))
    str))))

(define string-copy
  (case-lambda
    ((string) (substring string 0 (string-length string)))
    ((string start) (substring string start (string-length string)))
    ((string start end) (substring string start end))))
(define (substring s start end)
  (let ((new (make-string (- end start))))
    (str-copy new 0 s start end)
    new))

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

(define (integer->char i)
  (sys:INTEGER_CHAR i))

(define (char->integer i)
  (sys:CHAR_INTEGER i))

(define (vector-length a)
    (unless (vector? a) (error "vector-length: not a vector" a))
  (sys:LOAD a 0))

(define (vector-set! vec pos val)
  (unless (vector? vec) (error "Vector-set!: not a vector"))
  (unless (< pos (vector-length vec) ) (error "Bad idx"))
  (unless (>= pos 0) (error "Bad idx"))
  (sys:STORE vec val (+ pos 1)))

(define (vector-ref vec pos)
  (unless (vector? vec) (error "Trying to load not a vector vec:" vec " pos:" pos))
  (unless (< pos (vector-length vec) ) (error "Bad idx"))
  (unless (>= pos 0) (error "Bad idx"))
  (sys:LOAD vec (+ pos 1)))

(define make-vector
  (case-lambda
   ((len) (make-vector len #f))
   ((len obj)
    (let ((vec (sys:ALLOC (+ (* len 8) 16) vector-tag)))
      (sys:STORE vec len 0)
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(vector-set! vec i obj))
      vec))))

(define (eq? a b)
  (sys:EQ a b))

(define (reverse lst)
  (let loop ((lst lst) (res '()))
    (if (pair? lst)
	(loop (cdr lst) (cons (car lst) res))
	res)))

(define apply
  (case-lambda
    ((fun args)
     (let* ((len (length args)))
       (unless (procedure? fun)
	 (error "Applying to not a procedure:" fun))
       (unless (list? args)
	 (error "Apply to non-list" args))
       ;; sys:APPLY must always be in tail position.
       (sys:APPLY fun args)))
    ((fun . lst)
     (let* ((rlst (reverse lst))
	    (unused (unless (list? (car rlst))
		      (error "Apply to non-list" (car rlst))))
	    (firstargs (reverse (cdr rlst)))
	    (args (append2 firstargs (car rlst))))
       (apply fun args)))))

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

(define (eqv? a b)
  (or (eq? a b) (and (flonum? a) (flonum? b) (= a b))))

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
(define list
  (case-lambda
   (() '())
   ((a) (cons a '()))
   ((a b) (cons a (cons b '())))
   (rest rest)))

(define (zero? a)
  (= a 0))

(define (list->vector lst)
  (let* ((len (length lst))
	 (v (make-vector len)))
    (do ((i 0 (+ i 1))
	 (p lst (cdr p)))
	((= i len) v)
      (vector-set! v i (car p)))))

(define vector
  (case-lambda
   ((a) (let ((v (make-vector 1)))
	  (vector-set! v 0 a)
	  v))
   ((a b)
    (let ((v (make-vector 2)))
      (vector-set! v 0 a)
      (vector-set! v 1 b)
      v))
   ((a b c)
    (let ((v (make-vector 3)))
      (vector-set! v 0 a)
      (vector-set! v 1 b)
      (vector-set! v 2 c)
      v))
   ((a b c d)
    (let ((v (make-vector 4)))
      (vector-set! v 0 a)
      (vector-set! v 1 b)
      (vector-set! v 2 c)
      (vector-set! v 3 d)
      v))
   (vals
    (list->vector vals))))

(define (vector->list vec)
  (let loop ((i (vector-length vec)) (l '()))
    (if (= i 0)
	l
	(loop (- i 1) (cons (vector-ref vec (- i 1)) l)))))

(define (odd? x)
  (= 1 (modulo x 2)))

(define (even? x)
  (= 0 (modulo x 2)))

;;;;;; Records
(define (record-set! record index value)
  (unless (record? record) (error "record-set!: not a record" record))
  (sys:STORE record value (+ index 1)))
(define (record-ref record index)
  (unless (record? record) (error "record-ref: not a record" record))
  (sys:LOAD record (+ index 1)))
(define (make-record sz)
  (let ((rec (sys:ALLOC (+ (* 8 (+ 1 sz)) 16) record-tag)))
    (sys:STORE rec (+ sz 1) 0)
    (do ((i 0 (+ i 1)))
	((= i (+ 1 sz)) rec)
      (record-set! rec i #f))))
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

;;;;;; Port ops
(define-record-type port (make-port fd peek buf) port?
		    (fd port-fd)
		    (peek port-peek port-peek-set!)
		    (buf port-buf port-buf-set!))

(define input-port? port?)
(define output-port? port?)

(define *current-input-port* (make-port 0 #f #f))
(define (current-input-port) *current-input-port*)
(define *current-output-port* (make-port 1 #f #f))
(define (current-output-port) *current-output-port*)
(define *current-error-port* (make-port 2 #f #f))
(define (current-error-port) *current-error-port*)


(define (c-open pathname read)
  (sys:FOREIGN_CALL "scm_open" '(int32 (string uint8)) pathname	read))

(define (c-close fd)
  (sys:FOREIGN_CALL "close" '(int32 (int32)) fd))

(define (c-write fd data len)
  (sys:FOREIGN_CALL "write" '(int64 (int32 string uint64)) fd data len))

(define (c-read fd buf cnt)
  (sys:FOREIGN_CALL "read" '(int64 (int32 string uint64)) fd buf cnt))

(define (open-input-file file)
  (let ((fd (c-open file 1)))
    (when (< fd 0) (error "open-input-file error:" file))
    (make-port fd #f #f)))
(define (open-output-file file)
  (let ((fd (c-open file 0)))
    (when (< fd 0) (error "open-output-file error:" file))
    (make-port fd #f #f)))
(define (close-port port)
  (c-close (port-fd port )))
(define close-output-port close-port)
(define close-input-port close-port)
(define-record-type eof-object (make-eof-object) eof-object?)
(define peek-char
  (case-lambda
   (() (peek-char (current-input-port)))
   ((port)
    (cond
     ((port-peek port))
     (else
      (let* ((buf (make-string 1))
	     (cnt (c-read (port-fd port) buf 1)))
	(if (= cnt 1)
	    (let ((peek (string-ref buf 0)))
	      (port-peek-set! port peek)
	      peek)
	    (make-eof-object))))))))

(define read-char
  (case-lambda
   (() (read-char (current-input-port)))
   ((port)
    (cond
     ((port-peek port) => (lambda (x) (port-peek-set! port #f) x))
     (else
      (let* ((buf (make-string 1))
	     (cnt (c-read (port-fd port) buf 1)))
	(if (= cnt 1)
	    (string-ref buf 0)
	    (make-eof-object))))))))
(define (write-char char port)
  (if (port-buf port)
      (port-buf-set! port (string-append (port-buf port) (make-string 1 char)))
      (let* ((buf (make-string 1 char))
	     (cnt (c-write (port-fd port) buf 1)))
	(if (= cnt 1)
	    #t
	    (error "write-char error")))))

(define (string->list str)
  (let ((n (string-length str)))
    (let loop ((i (- n 1)) (lst '()))
      (if (< i 0)
	  lst
	  (loop (- i 1) (cons (string-ref str i) lst))))))
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

(define (open-output-string)
  (make-port 0 #f ""))
(define (get-output-string port)
  (port-buf port))

;;;;;;;;;
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
;;;;;;;;;
(define (sin d)
  (sys:FOREIGN_CALL "sin" '(double (double)) (exact->inexact d)))
(define (cos d)
  (sys:FOREIGN_CALL "cos" '(double (double)) (exact->inexact d)))
(define (sqrt d)
  (sys:FOREIGN_CALL "sqrt" '(double (double)) (exact->inexact d)))
(define (atan d)
  (sys:FOREIGN_CALL "atan" '(double (double)) (exact->inexact d)))
(define (round d)
  (let* ((d (exact->inexact d))
	 (rounded (sys:FOREIGN_CALL "round" '(double (double)) d)))
    ;; Round to even, towards zero.
    (if (and  (= .5 (sys:FOREIGN_CALL "fabs" '(double (double)) (- d rounded)))
	     (not (= 0.0 (sys:FOREIGN_CALL "fmod" '(double (double double)) rounded 2.0))))
	(+ rounded (if (> d 0) -1 1))
	rounded)))
(define (ceiling x)
  (sys:FOREIGN_CALL "ceil" '(double (double)) (exact->inexact x)))
(define (log x)
  (sys:FOREIGN_CALL "log" '(double (double)) (exact->inexact x)))
(define (inexact? a)
  (flonum? a))
(define (exact? a)
  (fixnum? a))
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
;; (define (string=? a b)
;;   (define len (string-length a))
;;   (and
;;    (= (string-length a) (string-length b))
;;    (= 0 (sys:FOREIGN_CALL "strncmp" '(int32 (string string uint64)) a b len))))
(define (negative? p)
  (< p 0))
(define (positive? p)
  (> p 0))
(define (abs p)
  (if (negative? p)
      (- p)
      p))
(define (cons* first . rest)
      (let recur ((x first) (rest rest))
        (if (pair? rest)
          (cons x (recur (car rest) (cdr rest)))
          x)))


;;;;;;;;;;;;;;;;;;; Symbols

(define (symbol->string sym)
  (unless (symbol? sym) (error "symbol->string: not a sym" sym))
  (sys:LOAD sym 0))

;; Symbol table is defined in the BC emitter.
;;(define symbol-table '())
(define (string->symbol string)
  (cond
   ((assoc string symbol-table) => cdr)
   (else
    (let* ((new-name (string-copy string))
	   (cell (sys:ALLOC (* 8 5) symbol-tag)))
      (sys:STORE cell new-name 0)
      (sys:STORE cell undefined-tag 1)
      (sys:STORE cell #f 2)
      (sys:STORE cell #f 3)
      (set! symbol-table (cons (cons new-name cell) symbol-table))
      cell))))

;;;;;;;;;;;;;;;;;;; number->string
(define number->string
  (case-lambda
   ((num) (number->string num 10))
   ((num base)
    ;;(unless (and (number? num) (fixnum? base) (<= 1 base 16)) (error "bad number->string" num))
    (let* ((buflen 100)
	   (buffer (make-string buflen)))
      (cond ((flonum? num) (flonum->string num))
	    ;; ((bignum? num) (error "big-str" num))
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

(define (expt num exp)
  (if (> exp 0)
      (let loop ((n 1) (cnt exp))
	(if (= cnt 0) n
	    (loop (* num n) (- cnt 1))))
      (let loop ((n 1) (cnt exp))
	(if (= cnt 0) n
	    (loop (/ n num) (+ cnt 1))))))

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

(define (call-with-current-continuation thunk)
  (let ((cc (sys:CALLCC)))
    (thunk (lambda (res) (sys:CALLCC_RESUME cc res)))))

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

(define (flush-output-port port)
  #f)

(define (delete-file filename)
  (when (sys:FOREIGN_CALL "unlink" '(int32 (string)) filename)
    (error "Bad unlink:" filename)))

(define (file-exists? filename)
  (sys:FOREIGN_CALL "access" '(bool (string int32)) filename 0))

(define (call-with-values producer consumer)
  (apply consumer (producer)))

(define values
  (case-lambda
   ((a) a)
   ((a b) (cons a (cons b '())))
   ((a b c) (cons a (cons b (cons c '()))))
   (rest rest)))

(include "str2num.scm")

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

(define complex? number?)
(define real? number?)
(define rational? number?)
(define integer? fixnum?)
(define exact? fixnum?)
(define inexact? flonum?)
(define exact-integer? fixnum?)

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

;; TIME

(define (jiffies-per-second)
  1000000000 ;; returns 1 on my Bones, which is wrong. this number should work for ?many? linuxen
  )

(define (current-jiffy) (call-with-input-file "/proc/uptime" (lambda (port) (read port))))
(define (current-second) (call-with-input-file "/proc/uptime" (lambda (port) (read port))))

(define read-buf (make-string 1000))
(define read
  (case-lambda
   (() (read (current-input-port)))
   ((port)
    (define line 1)
    (define (read2 port)
      (define (read-to-delimited)
	(let loop ((res 0) (c (peek-char port)))
	  (if (eof-object? c)
	      (if (not (= 0 res)) (substring read-buf 0 res) c)
	      (case c
		((#\( #\) #\" #\| #\newline #\return #\space #\tab #\;)
		 (substring read-buf 0 res))
		(else
		 (string-set! read-buf res (read-char port))
		 (loop (+ res 1) (peek-char port)))))))
      (define (skip-whitespace)
	(let loop ()
	  (let ((c (peek-char port)))
	    (cond
	     ((eof-object? c) c)
	     ((char=? #\newline c) (set! line (+ 1 line)) (read-char port) (loop))
	     ((char-whitespace? c) (read-char port) (loop))))))
      (define (skip-whitespace-and-comments)
	(let loop ()
	  (let ((c (peek-char port)))
	    (cond
	     ((eof-object? c) c)
	     ((char=? #\newline c) (set! line (+ 1 line)) (read-char port) (loop))
	     ((char-whitespace? c) (read-char port) (loop))
	     ((char=? #\; c) (skip-line) (loop))))))
      (define (skip-line)
	(let loop ()
	  (let ((c (read-char port)))
	    (if (eof-object? c)
		c
		(if (char=? c #\newline)
		    (set! line (+ 1 line))
		    (loop))))))
      (define (read-escape)
	(let ((c (read-char port)))
	  (if (eof-object? c) (error "Incomplete escape sequence"))
	  (case c
	    ((#\a) #\alarm)
	    ((#\n) #\newline)
	    ((#\r) #\return)
	    ((#\t) #\tab)
	    ((#\b) #\backspace)
	    ((#\tab #\space) (skip-line) (skip-whitespace) #f)
	    ((#\newline) (skip-whitespace) #f)
	    ((#\x #\X)
	     (let* ((delim (read-to-delimited))
		    (ch (string->number delim 16))
		    (next (read-char port)))
	       (if (not (eq? #\; next))
		   (error "Invalid hex string escape")
		   (integer->char ch))))
	    (else  c)))
	)
      (define (read-delimited term)
	(let loop ((res 0) (c (read-char port)))
	  (cond
	   ((eof-object? c) (error "incomplete object:" (substring read-buf 0 res) "line: " line))
	   ((char=? #\\ c)
	    (let ((es (read-escape)))
	      (if es
		  (begin
		    (string-set! read-buf res es)
		    (loop (+ 1 res) (read-char port)))
		  (loop res (read-char port)))))
	   ((char=? term c) (substring read-buf 0 res))
	   (else
	    (string-set! read-buf res c)
	    (loop (+ 1 res) (read-char port))))
	  )
	)
      (define (lower-case string)
	(do ((i 0 (+ i 1)))
	    ((= i (string-length string)) string)
	  (string-set! string i (char-downcase (string-ref string i)))))
      (define (read-list)
	(define line-start line)
	(let loop ((res '()))
	  (skip-whitespace-and-comments)
	  (let ((c (peek-char port)))
	    (cond
	     ((eof-object? c) (error "EOF found while parsing list starting on line " line-start " and ending " line))
	     ((char=? c #\)) (read-char port) (reverse res))
	     ((char=? c #\.) (let ((token (read-to-delimited)))
			       (if (= 1 (string-length token))
				   (let ((fin (read-one)))
				     (skip-whitespace-and-comments)
				     (if (not (eq? #\) (read-char port)))
					 (error "Invalid dotted list")
					 (append (reverse res) fin)))
				   (loop (cons (cond
						((string->number token) => (lambda (num) num))
						(else (string->symbol (lower-case token)))) res)))))
	     (else (loop (cons (read-one) res)))))))
      (define named-chars '(("tab" . #\tab)
			    ("space" . #\space)
			    ("return" . #\return)
			    ("newline" . #\newline)
			    ("alarm" . #\alarm)
			    ("backspace" . #\backspace)
			    ("delete" . #\delete)
					;("escape" . #\escape)
					;		      ("null" . #\null)
			    ))
      (define delims '(#\( #\) #\; #\| #\" #\space))
      (define (do-read-char)
	(let ((ch (peek-char port)))
	  (if (memv ch delims)
	      (read-char port)
	      (let ((token (read-to-delimited)))
		(cond
		 ((= 1 (string-length token)) (string-ref token 0))
		 ((assoc token named-chars) => cdr)
		 (else (error "Error invalid char: " token)))))))
      (define (skip-comment)
	(let loop ((depth 0))
	  (case (read-char port)
	    ((#\#) (loop (if (char=? #\| (peek-char port)) (+ 1 depth) depth)))
	    ((#\|) (if (char=? #\# (peek-char port))
		       (if (= 0 depth)
			   (read-char port)
			   (loop (- depth 1)))
		       (loop depth)))
	    ((#\newline) (set! line (+ 1 line))
	     (loop depth))
	    (else (if (eof-object? (peek-char port))
		      (error "unterminated comment")
		      (loop depth))))))
      (define (read-hash)
	(let ((c (peek-char port)))
	  (case c
	    ((#\|) (skip-comment))
	    ((#\;) (read-char port) (read-one) (read-one))
	    ((#\() (read-char port) (list->vector (read-list)))
	    ((#\\) (read-char port) (do-read-char))
	    ((#\t #\T #\f #\F)
	     (let ((v (lower-case (read-to-delimited))))
	       (cond
		((equal? "f" v) #f)
		((equal? "t" v) #t)
		((equal? "true" v) #t)
		((equal? "false" v) #f)
		(else (error "Can't parse hash token:" v)))))
	    ((#\b #\B #\o #\O #\d #\D #\x #\X #\i #\I #\e #\E) (string->number (string-append "#" (read-to-delimited))))
	    ((#\u) (read-char port)
	     (if (not (char=? #\8 (peek-char port))) (error "Not a bytevector:" (peek-char port))
		 (read-char port))
	     (let ((ls (read-one)))
	       (if (not (list? ls))
		   (error "Not a bytevector list:" ls)
		   ls)))
	    (else (error "Unknown hash: " c)))))
      (define (read-one)
	(skip-whitespace)
	(let ((c (peek-char port)))
	  (case (peek-char port)
	    ((#\#) (read-char port) (read-hash))
	    ((#\() (read-char port)	 (read-list))
	    ((#\)) (read-char port)	 (error "Extra list terminator found"))
	    ((#\") (read-char port) (read-delimited #\"))
	    ((#\;) (read-char port) (skip-line) (read-one))
	    ((#\|) (read-char port) (string->symbol (read-delimited #\|)))
	    ((#\') (read-char port) (list 'quote (read-one)))
	    ((#\`) (read-char port) (list 'quasiquote (read-one)))
	    ((#\,) (read-char port)
	     (case (peek-char port)
	       ((#\@) (read-char port) (list 'unquote-splicing (read-one)))
	       (else
		(list 'unquote (read-one)))))
	    (else
	     (let  ((token (read-to-delimited)))
	       (cond 
		((eof-object? token) token)
		((string->number token) => (lambda (num) num))
		(else (string->symbol (lower-case token)))))))))
      (read-one))
    (read2 port))))
;;;;;;;;;;;;

;; TODO currently this doesn't work because we don't support bignums.

;; Flonum->str implementation
;; Printing Floating-Point Numbers Quickly and Accurately
;; Robert G Burger, R Kent Dybvig
;; v - float, used for approximation
;; f - mantissa
;; e - exponent
;; min exponent, -1022 for doubles.
;; p - precision, 53 for doubles
;; b - import base, 2
;; B - export base, 10.
(define (flonum->digits v f e min-e p b B)
  (let ((round? (even? f)))
    (if (>= e 0)
	(if (not (= f (expt b (- p 1))))
	    (let ((be (expt b e)))
	      (scale (* f be 2) 2 be be 0 B round? round? v))
	    (let* ((be (expt b e)) (be1 (* be b)))
	      (scale (* f be1 2) (* b 2) be1 be 0 B round? round? v)))
	(if (or (= e min-e) (not (= f (expt b (- p 1)))))
	    (scale (* f 2) (* (expt b (- e)) 2) 1 1 0 B round? round? v)
	    (scale (* f b 2) (* (expt b (- 1 e)) 2) b 1 0 B round? round? v)))))

(define scale
  (lambda (r s m+ m- k B low-ok? high-ok? v )
    (let (( est (exact (ceiling (- (logB B v) 1e-10)))))
      (if (>= est 0)
	  (fixup r (* s (expt B est)) m+ m- est B low-ok? high-ok? )
	  (let ((scale (expt B (- est))))
	    (fixup (* r scale) s (* m+ scale) (* m- scale) est B low-ok? high-ok? ))))))

(define (logB B v)
  (/ (log v) (log B)))

(define fixup
  (lambda (r s m+ m- k B low-ok? high-ok? )
    (if ((if high-ok? >= >) (+ r m+) s) ; too low?
	(cons (+ k 1) (generate r (* s B) m+ m- B low-ok? high-ok? ))
	(cons k (generate r s m+ m- B low-ok? high-ok? )))))
(define generate
  (lambda (r s m+ m- B low-ok? high-ok? )
    (let ((q (quotient (* r B) s))
	  (r (remainder (* r B) s))
	  (m+ (* m+ B))
	  (m- (* m- B)))
      (let ((d q ))
	(let ((tc1 ((if low-ok? <= <) r m-))
	      (tc2 ((if high-ok? >= >) (+ r m+) s)))
	  (if (not tc1 )
	      (if (not tc2 )
		  (cons d (generate r s m+ m- B low-ok? high-ok? ))
		  (list (+ d 1)))
	      (if (not tc2 )
		  (list d)
		  (if (< (* r 2) s)
		      (list d)
		      (list (+ d 1))))))))))

(define (format-free e d p)
  (when (< e 0)
    (display #\0 p)
    (display #\. p)
    (do ((e e (+ e 1)))
 	((>= e 0))
      (display #\0 p)))
  (let loop ((u (car d)) (d (cdr d)) (e e))
    (when (= e 0) (display #\. p))
    (display u p)
    (when (or (pair? d) (> e 0))
      (if (pair? d)
	  (loop (car d) (cdr d) (- e 1))
	  (loop 0 '() (- e 1))))))

(define (format-exponential e d p)
  (display (car d) p)
  (display #\. p)
  (for-each (lambda (x) (display x p)) (cdr d))
  (display #\e p)
  (display (- e 1) p))

(define (format-choose e d p)
  ((if (< -4 e 10) format-free format-exponential) e d p))

(define (flonum-abs->string f)
  (define digits (flonum->digits f
				 (sys:FOREIGN_CALL "flonum_mantissa" '(int64 (double)) f)
				 (sys:FOREIGN_CALL "flonum_exponent" '(int64 (double)) f)
				  -1022 53 2 10))
  (define port (open-output-string))
  (format-choose (car digits) (cdr digits) port)
  (get-output-string port))

(define (flonum->string num)
  (cond
   ;; ((nan? num) "+nan.0")
   ;; ((and (infinite? num) (positive? num)) "+inf.0")
   ;; ((infinite? num) "-inf.0")
   ((= 0.0 num)
    (if (sys:FOREIGN_CALL "flonum_negative" '(bool (double)) num)
	"-0.0"
	"0.0"))
   (else
    (let ((res (flonum-abs->string (abs num))))
      (if (negative? num)
	  (string-append "-" res)
	  res)))))


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

