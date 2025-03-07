(define-syntax push!
  (syntax-rules ()
    ((_ var val) (set! var (cons val var)))))

;; Versions of map, for-each, filter, where we don't need to indent for the lambda.
(define-syntax omap
  (syntax-rules ()
    ((_ (x ...) (xs ...) body ...) (map (lambda (x ...) body ...) xs ...))
    ((_ x xs body ...) (map (lambda (x) body ...) xs))))
(define-syntax for
  (syntax-rules ()
    ((_ (x ...) (xs ...) body ...) (for-each (lambda (x ...) body ...) xs ...))
    ((_ x xs body ...) (for-each (lambda (x) body ...) xs))))
(define-syntax ofilter
  (syntax-rules () ((_ x xs body ...) (filter (lambda (x) body ...) xs))))


;; Can probably remove some
(define (fold-left f init seq)
  (if (null? seq)
    init
    (fold-left f (f init (car seq)) (cdr seq))))

(define (reduce-left f init seq)
  (if (null? seq)
    init
    (if (null? (cdr seq))
      (car seq)
      (fold-left f (car seq) (cdr seq)))))

;; Join, preallocating the total string.
(define (join sep strs)
  (if (null? strs)
      ""
      (let* ((sep-len (string-length sep))
	     (totallen (+ (* (- (length strs) 1) sep-len) (apply + (map string-length strs))))
	     (newstr (make-string totallen)))
	(string-copy! newstr 0 (car strs) 0 (string-length (car strs)))
	(let loop ((strs (cdr strs)) (place (string-length (car strs))))
	  (if (not (null? strs))
	      (let* ((cur_str (car strs))
 		     (cur_len (string-length cur_str)))
		(string-copy! newstr place sep 0 sep-len)
		(string-copy! newstr (+ place sep-len)  (car strs) 0 cur_len)
		(loop (cdr strs) (+ place cur_len sep-len)))))
	newstr)))

;; Used for handling rest arguments.
(define (ilength lst)
  (do ((lst lst (cdr lst)) (cnt 0 (+ 1 cnt)))
      ((not (pair? lst)) cnt)))

(define (to-proper l)
  (if (null? l)
    '()
    (if (pair? l)
      (cons (car l) (to-proper (cdr l)))
      (list l))))

(define (ilist? a) (or (pair? a) (null? a)))
(define (imap f l)
  (if (null? l)
    '()
    (if (pair? l)
      (let ((val (f (car l)))) (cons val (imap f (cdr l))))
      (f l))))

(define (read-file)
  (let read-file-rec ((sexps '()))
    (define next (read))
    (if (eof-object? next)
      (reverse sexps)
      (read-file-rec (cons next sexps)))))

(define gen-sym
  ;; TODO fuckin' hack - gensyms need to be unique because of serialization. FUCK
  (let ((n (cond-expand (callcc 100000) (else 0))))
    (lambda (a)
      (set! n (+ n 1))
      (string->symbol
       (string-append "GENSYM-" (symbol->string a) "-" (number->string n))))))

(define (bytevector->list bv)
  (unless (bytevector? bv) (error "Not a bytevector:" bv))
  (do ((i 0 (+ i 1)) (lst '() (cons (bytevector-u8-ref bv i) lst)))
      ((= i (bytevector-length bv)) (reverse lst))))
