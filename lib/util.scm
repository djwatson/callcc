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

(define (join sep lst)
  (reduce-left (lambda (x y) (string-append x sep y)) "" lst))

;; Seems to break chez?  Is there a bug?
;; (define (join2 sep strs)
;;   (if (null? strs)
;;       ""
;;       (let* ((sep-len (string-length sep))
;; 	     (totallen (+ (* (- (length strs) 1) sep-len) (apply + (map string-length strs))))
;; 	     (newstr (make-string totallen)))
;; 	(let loop ((strs strs) (place 0))
;; 	  (if (not (null? strs))
;; 	      (let* ((cur_str (car strs))
;;  		     (cur_len (string-length cur_str)))
;; 		(unless (= place 0)
;; 		  (string-copy! newstr place sep 0 sep-len))
;; 		(let ((new-place (if (= place 0) (+ place sep-len) place)))
;; 		  (string-copy! newstr new-place  (car strs) 0 cur_len)
;; 		  (loop (cdr strs) (+ new-place cur_len))))))
;; 	newstr)))

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
  (let ((n 0))
    (lambda (a)
      (set! n (+ n 1))
      (string->symbol
       (string-append "GENSYM-" (symbol->string a) "-" (number->string n))))))
