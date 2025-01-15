;; A matcher, similar to dybvig's.  It uses catamorphism + guard.

(define-syntax identifier/literal
  (syntax-rules ()
    ((identifier/literal atom identifier literal)
     (let-syntax ((check-identifier (syntax-rules ()
                                      ((_ atom symbol _) symbol)
                                      ((_ datum _ value) value))))
       (check-identifier raw-symbol identifier literal)))))

(define-syntax match/extract
  (syntax-rules (___ unquote)
    ((_ ,(? cat sym) (sk ...) . ids)
     (sk ... (sym sym-tmp) . ids ))
    ((_ ,(cat sym) (sk ...) . ids)
     (sk ... (sym sym-tmp) . ids ))
    ((_ ,sym (sk ...) . ids)
     (sk ... (sym sym-tmp) . ids ))
    ((_ #(a ...) sk . ids)
     (match/extract (a ...) sk . ids))
    ((_ () (sk ...) .  ids)
     (sk ... . ids))
    ((_ (a . b) sk . ids)
     (match/extract a (match/extract b sk) . ids))
    ((_ var (sk ...) . ids)
     (sk ... . ids))))

(define (ilength lst)
  (define (ilength lst cnt)
    (if (pair? lst)
	(ilength (cdr lst) (+ cnt 1))
	cnt))
  (ilength lst 0))
(define-syntax match/ellipsis
  ;; Slightly complicated here because ,var looks like a list,
  ;; even though we want it as the atomic last pattern.
  (syntax-rules (___)
		((_ pat ,tail exp sk fk (ids id-tmp) ...)
		 (match/ellipsis "run" pat () ,tail exp sk fk (ids id-tmp) ...))
		((_ pat (tail ... . ,fin) exp sk fk (ids id-tmp) ...)
		 (match/ellipsis "run" pat (tail ...) (tail ... . ,fin) exp sk fk (ids id-tmp) ...))
		((_ pat tail exp sk fk (ids id-tmp) ...)
		 (match/ellipsis "run" pat tail tail exp sk fk (ids id-tmp) ...))
		((_ "run" pat tail tail-pattern exp sk fk (ids id-tmp) ...)
		 (let* ((tail-len (ilength 'tail))
			(dot-len (- (ilength exp) tail-len)))
		   (if (negative? dot-len) fk
		       (let loop ((len dot-len) (exp exp) (id-tmp '()) ...)
			 (if (= 0 len)
			     (let ((ids (lambda () (map (lambda (x) (x)) (reverse id-tmp)))) ...)
			       (match/clause tail-pattern exp sk fk))
			     (match/clause pat (car exp) (loop (- len 1) (cdr exp) (cons ids id-tmp) ...) fk))))))))

(define-syntax match/clause
  (syntax-rules (quote quasiquote unquote and ___)
    ;; Match sym with guard
    ((_ ,(? guard sym) exp sk fk)
     (if (not (guard exp))
	 fk
	 (let ((sym (lambda () exp))) sk)))
    ;; Match a quoted sym with catamorphism
    ((_ ,(cat sym) exp sk fk)
     (let ((sym (lambda () (cat exp)))) sk))
    ;; Match a quoted sym
    ((_ ,sym exp sk fk)
     (let ((sym (lambda () exp))) sk))
    ;; Vector
    ((_ #(vec ...) exp sk fk)
     (if (vector? exp)
	 (let ((exp2 (vector->list exp)))
	   (match/clause (vec ...) exp2 sk fk))
	 fk))
    ;; empty list
    ((_ () exp  sk fk)
     (if (null? exp) sk fk))
    ((_ (a ___ . tail) exp sk fk)
     (if (or (null? exp) (pair? exp))
	 (match/extract a (match/ellipsis a tail exp sk fk))
	 fk))
    ;; Split pairs.
    ((_ (a . b) exp sk fk)
     (if (pair? exp)
	 (let ((left (car exp)) (rest (cdr exp)))
	   (match/clause a left (match/clause b rest sk fk) fk))
	 fk))
    ;; Match a var or literal
    ((_ var exp sk fk)
     (identifier/literal var
			 (if (equal? 'var exp) sk fk)
			 (if (equal? var exp) sk fk)))))

(define-syntax match/build-vars
  (syntax-rules ()
    ((_ sk (ids id-tmp) ...)
     (let ((ids (ids)) ...)
	sk))))

(define-syntax match/evaluated
  (syntax-rules (guard)
    ((_ exp) (error "No match for:" exp)) ;; No match
    ((_ exp (pattern (guard guard-actions ...) actions ...) . rest)
     (let ((failure (lambda () (match/evaluated exp . rest))))
       (match/clause
	pattern
	exp
	(match/extract
	 pattern
	 (match/build-vars
	  (if (and guard-actions ...)
	      (begin actions ...)
	      (failure))))
	(failure))))
    ((_ exp (pattern actions ...) . rest)
     (let ((failure (lambda () (match/evaluated exp . rest))))
       ;;(display "CHecking patterm:") (display 'pattern) (newline)
       (match/clause
	pattern
	exp
	(match/extract pattern (match/build-vars (begin actions ...)))
	(failure))))))

(define-syntax match
  (syntax-rules ()
    ((_ exp (pattern actions ... value) ...)
     (let ((evaluated exp))
       (match/evaluated evaluated (pattern actions ... value) ...)))))

;;;;;;;;;;;;; test

;; (when #f
;;   (let ()
;;     (define (runner x)
;;       (pass x
;; 	    ((let ((,var ,(runner bind) ) ___) ,form . ,forms)
;; 	     (let ((bind (map (lambda (e) (if (number? e) (+ e 1) e)) bind)))
;; 	       `(letter ((,var ,bind ) ___) ,form . ,forms))
;; 	     )))


;;     (define (if-pass x)
;;       (pass x
;; 	    ((if ,(if-pass a) ,(if-pass b))
;; 	     `(if ,a ,b #f))
;; 	    ((if ,(if-pass a) ,(if-pass b) ,(if-pass c))
;; 	     `(if ,a ,b ,c))
;; 	    ((if ,a ___) (error "No match:" x))))

;;     (define (lpass x)
;;       (pass x
;; 	    ((lambda (,args ___  . ,rest) . ,body)
;; 	     `(1 "args" ,args  , "rest" ,rest))
;; 	    ((lambda ,arg . ,body)
;; 	     `(2 ,arg . ,body ))
;; 	    ))
;;     (pretty-print (if-pass '(if (if 1 2 3) (if 1 2) 3)))
;;     (pretty-print (runner '(let ((x 10) (y (let ((z 20)) 'ok))) body stuff)))
;;     (pretty-print (lpass '(lambda (a b c . d) 10)))
;; ))
