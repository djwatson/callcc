(test-begin "Syntax-rules patterns")

;;    ;; Some simple patern and template pitfalls:

(let-syntax ((foo (syntax-rules () 
		    ((_ (x ...) ...)
		     '(x ... ...)))))
  (test-equal '(1 2 3 4) (foo (1 2) (3 4))))     ;==> (1 2 3 4)
 ;; rnrs pattern extensions:

(let-syntax ((foo (syntax-rules  () 
		    ((_ x ... y z)
		     '((x ...) y z)))))
  (test-equal '((1 2) 3 4) (foo 1 2 3 4)))  ;==> ((1 2) 3 4)


(let-syntax ((foo (syntax-rules  () 
		    ((_ x ... y . z)
		     '((x ...) y z)))))
  (test-equal '((1 2) 3 4) (foo 1 2 3 . 4)))  ;==> ((1 2) 3 4)

(let-syntax ((foo (syntax-rules  () 
		    ((_ #(x ... y z))
		     '(#(x ...) y z)))))
  (test-equal '(#(1 2) 3 4) (foo #(1 2 3 4))))  ;==> (#(1 2) 3 4)

(let-syntax ((foo (syntax-rules  () 
		    ((_ (a b) ...)
		     '((a ...) (b ...))))))
  (test-equal '((1 3) (2 4)) (foo (1 2) (3 4))))  ;==> ((1 3) (2 4))

(let-syntax ((foo (lambda (stx) (syntax-case stx () 
			       ((_ (a b) ...)
				'((a ...) (b ...)))
			       (_ #f)))))
  (test-eq #f (foo (1 2) 3)))  ;==> #f

(let-syntax ((foo (syntax-rules  () 
		    ((_ (a b) ... . c)
		     '((a ...) (b ...))))))
  (test-equal '((1 3) (2 4)) (foo (1 2) (3 4) . 3)))  ;==> ((1 3) (2 4))

(let-syntax ((foo (syntax-rules  () 
		    ((_ (var ...) (var2 ...) ...)
		     '((var var2 ...) ...)))))
  (test-equal '((1 4 5) (2 7 8) (3 10 11)) (foo (1 2 3) (4 5) (7 8) (10 11))))  ;==> ((1 4 5) (2 7 8) (3 10 11))

(let-syntax ((foo (syntax-rules  () 
		    ((_ (var ...) (var2 ...) ...)
		     '((var ... var2 ...) ...)))))
  (test-equal '((1 2 3 4 5 6) (1 2 3 7 8 9))
      (foo (1 2 3) (4 5 6) (7 8 9))))   ;==> ((1 2 3 4 5 6) (1 2 3 7 8 9))

(let-syntax ((foo (syntax-rules  () 
		    ((_ ((var ...) ...) ...)
		     '(var ... ... ...)))))
  (test-equal '(4 5 6 7 8 9 4 5 6 7 8 9) (foo ((4 5 6) (7 8 9)) ((4 5 6) (7 8 9)))))  ;==> (4 5 6 7 8 9 4 5 6 7 8 9)

(let-syntax ((foo (syntax-rules  () 
		    ((_ ((Var ...)
			 ((Var2 ...) ...) ...))
		     '(((Var ... Var2 ...) ... ...))))))
  (test-equal '(((1 2 3 4 5 6) (1 2 3 7 8 9) (1 2 3 4 5 6) (1 2 3 7 8 9)))
      (foo ((1 2 3) ((4 5 6) (7 8 9)) ((4 5 6) (7 8 9)))))) ;==> (((1 2 3 4 5 6) (1 2 3 7 8 9) (1 2 3 4 5 6) (1 2 3 7 8 9)))

(let-syntax ((foo (syntax-rules  () 
		    ((_ ((Var ...)
			 ((Var2 ...) ...) ...))
		     '(((Var ... Var2 ...) ...) ...)))))
  (test-equal '(((1 2 3 4 5 6) (1 2 3 7 8 9)) ((1 2 3 4 5 6) (1 2 3 7 8 9)))
      (foo ((1 2 3) ((4 5 6) (7 8 9)) ((4 5 6) (7 8 9)))))) ;==> (((1 2 3 4 5 6) (1 2 3 7 8 9)) ((1 2 3 4 5 6) (1 2 3 7 8 9)))

;; TODO needs export of identifier?
;; (define-syntax foo
;;   (lambda (e)
;;     (or (identifier? e)
;;         (syntax-violation 'foo "Invalid expression" e))
;;     40))

;; TODO setter-syntax
;   foo             ;==> 40
   ;; (set! foo 1) ;==> Syntax violation: Syntax being set! is not a variable transformer
   ;; (foo)        ;==> syntax violation: foo - Invalid expression


;; TODO make-variable-transformer? identifier-syntax?
   ;; (define p (cons 4 5))
   ;; (define-syntax p.car
   ;;   (make-variable-transformer
   ;;    (lambda (x)
   ;;      (syntax-case x (set!)
   ;;        ((set! _ e) (syntax (set-car! p e)))
   ;;        ((_ . rest) (syntax ((car p) . rest)))
   ;;        (_          (syntax (car p)))))))
   ;; (set! p.car 15)
   ;; p.car           ;==> 15
   ;; p               ;==> (15 . 5)

   ;; (define p (cons 4 5))
   ;; (define-syntax p.car (identifier-syntax (car p)))
   ;; p.car              ;==> 4
   ;; ;;(set! p.car 15)  ;==> Syntax violation: Keyword being set! is not a variable transformer

   ;; (define p (cons 4 5))
   ;; (define-syntax p.car
   ;;   (identifier-syntax
   ;;    (_          (car p))
   ;;    ((set! _ e) (set-car! p e))))
   ;; (set! p.car 15)
   ;; p.car           ;==> 15
   ;; p               ;==> (15 . 5)

   ;; Testing toplevel forward references:

(define (f) (g))
(define (g) 15)
(test-eqv 15 (f))             ;==> 15

(define-syntax foo (lambda (_) (syntax (bar))))
(define-syntax bar (lambda (_) 1))
(test-eqv 1 (foo))           ;==> 1

   ;; The following must give an error, since g-0 is bound at level 0 but used at level 1:

   ;; (define-syntax foo (lambda (_) (g-0)))  ;==> Syntax violation: invalid reference
   ;;                                         ;    No binding available for g-0 at level 1
   ;; (define (g-0) 1)
   ;; (foo)

   ;; Correct forward reference (*):

   (let ((x 'outer))
     (define-syntax foo
       (syntax-rules ()
         ((_ lhs) (define lhs x))))
     (foo (f))
     (define x 'inner)
     (test-eq 'inner (f)))                  ;==> inner

   ;; This must give an error:
;;
;; TODO this should be a compile error
     ;; (let ()
     ;;   (let-syntax ((foo (lambda (_) (let ((x 2)) (syntax x)))))
     ;;     (define (f) (foo))
     ;; 	 (define x 1)
     ;;   (display (f))))
                                        ;==>  Attempt to use binding of x at invalid level 0.  Binding is only valid at levels: 1

   ;; Forward references for internal define-syntax works correctly.


   (let ()
     (define-syntax odd
       (syntax-rules ()
         ((odd) #t)
         ((odd x . y) (not (even . y)))))
     (define-syntax even
       (syntax-rules ()
         ((even) #f)
         ((even x . y) (not (odd . y)))))
     (test-eq #t (odd x x x)))                          ;==> #t


   ;; Forward reference to procedure from transformer.

   (let ()
     (define-syntax foo
       (syntax-rules ()
         ((_) bar)))
     (define bar 1)
     (test-eqv 1 (foo)))            ;==> 1

   ;; Secrecy of generated toplevel defines:

;; TODO I have no idea what's going on here
;; (define x 1)
;;    (let-syntax ((foo (lambda (e)
;;                        (syntax (define x 2)
;;                                x))))
;;      (display (foo)))  ;==> 2
;; (display x)         ;==> 1



   ;; Stress testing expander with internal letrec-generated body,
   ;; begins, etc.

(test-eqv 1 (let ()
    (letrec-syntax ((foo (syntax-rules ()
                           ((_) (begin (define (x) 1)
                                       (begin
                                         (define-syntax y
                                           (syntax-rules ()
                                             ((_) (x))))
                                         (bar y))))))
                    (bar (syntax-rules ()
                           ((_ y) (begin (define (z) (baz (y)))
                                         (z)))))
                    (baz (syntax-rules ()
                           ((baz z) z))))
      (foo))))                               ;==> 1

;; TODO r7rs doesn't allow empty bodies.
;; (let ((foo /))
;;      (letrec-syntax ((foo (syntax-rules ()
;;                             ((_ z) (begin (define (x) 4)
;;                                           (define-syntax y
;;                                             (syntax-rules ()
;;                                               ((_) (x))))
;;                                           (bar z y)))))
;;                      (bar (syntax-rules ()
;;                             ((_ z y) (define (z) (baz (y))))))
;;                      (baz (syntax-rules ()
;;                             ((baz z) z))))
;;        (let-syntax ((foobar (syntax-rules ()   ;; test nested let-syntax
;;                               ((_ u z)
;;                                (define-syntax u
;;                                  (syntax-rules ()
;;                                    ((_ x y) (z x y))))))))
;;          (foo a)
;;          (foobar gaga goo)))   ;; foobar creates forward reference to goo
;;      ;; from expanded transformer.
;;      (define-syntax goo (syntax-rules ()
;;                           ((_ x y) (define-syntax x
;;                                      (syntax-rules ()
;;                                        ((_) y))))))
;;      (gaga b (a))
;;      (foo (b)))

   (let ((a 1)
         (b 2))
     (test-eqv 3 (+ a b)))     ;==> 3

   (define-syntax swap!
     (lambda (exp)
       (syntax-case exp ()
         ((_ a b)
          (syntax
           (let ((temp a))
             (set! a b)
             (set! b temp)))))))

(let-values (((a b) (let ((temp 1)
		       (set! 2))
		   (swap! set! temp)
		   (values temp set!))))
  (test-eqv 2 a)
  (test-eqv 1 b))   ;==> 2 1

   (let ((x 'outer))
     (let-syntax ((foo (lambda (exp) (syntax x))))
       (let ((x 'inner))
         (test-eq 'outer (foo)))))          ;==> outer

   ;; SRFI-93 example of expansion of internal definitions

   (let ()
     (define-syntax foo
       (syntax-rules ()
         ((foo x) (define x 37))))
     (foo a)
     (test-eqv 37 a))                 ;==> 37

(test-eq 'yes (case 'a
    ((b c) 'no)
    ((d a) 'yes)))      ;==> yes

(test-eqv 1 (let ((x 1))
    (let-syntax ((foo (lambda (exp) (syntax x))))
      (let ((x 2))
        (foo)))))       ;==> 1

(test-eqv 1 (let ((x 1))
    (let-syntax ((foo (lambda (exp) (datum->syntax (syntax y) 'x))))
      (let ((x 2))
        (foo)))))       ;==> 1


   (let-syntax ((foo (lambda (exp)
                       (let ((id (cadr exp)))
                         (bound-identifier=? (syntax x)
                                             (syntax id))))))
     (test-eq #f (foo x)))    ;==> #f

(test-eqv 2 (cond (#f 1) (else 2)))                 ;==> 2
;; TODO not sure how to test
(let ((else #f)) (cond (else 2)))      ;==> unspecified

   (let-syntax ((m (lambda (form)
                     (syntax-case form ()
                       ((_ x) (syntax
                               (let-syntax ((n (lambda (_)
                                                 (syntax (let ((x 4)) x)))))
                                 (n))))))))
     (test-eqv 4 (m z)))   ;==> 4

   ;; ;; Expression let-syntax and sequences:

(test-eqv 3 (+ (let-syntax ((foo (lambda (e) 1)))
       (display 'foo)
       (foo))
     2))          ;==> foo 3

(test-eqv 3 (+ (begin (display 'foo)
            1)
     2))          ;==> foo 3

    ;;;=========================================================================
   ;;
   ;; Composing macros with intentional variable capture using DATUM->SYNTAX
   ;;
    ;;;=========================================================================

(define-syntax if-it
     (lambda (x)
       (syntax-case x ()
         ((k e1 e2 e3)
          (with-syntax ((it (datum->syntax (syntax k) 'it)))
            (syntax (let ((it e1))
                      (if it e2 e3))))))))

   (define-syntax when-it
     (lambda (x)
       (syntax-case x ()
         ((k e1 e2)
          (with-syntax ((it* (datum->syntax (syntax k) 'it)))
            (syntax (if-it e1
                           (let ((it* it)) e2)
                           (if #f #f))))))))

   (define-syntax my-or
     (lambda (x)
       (syntax-case x ()
         ((k e1 e2)
          (syntax (if-it e1 it e2))))))

(test-eqv 2 (if-it 2 it 3))    ;==> 2
(test-eqv 42 (when-it 42 it))   ;==> 42
(test-eqv 2 (my-or 2 3))       ;==> 2

    ;;;=========================================================================
   ;;
   ;; Escaping ellipses:
   ;;
    ;;;=========================================================================

   (let-syntax ((m (lambda (form)
                     (syntax-case form ()
                       ((_ x ...)
                        (with-syntax ((___ (datum->syntax (syntax here) '...)))
                          (syntax
                           (let-syntax ((n (lambda (form)
                                             (syntax-case form ()
                                               ((_ x ... ___)
                                                (syntax `(x ... ___)))))))
                             (n a b c d)))))))))
     (test-equal '(a b c d) (m u v)))

   ;;==> (a b c d)


   (let-syntax ((m (lambda (form)
                     (syntax-case form ()
                       ((_ x ...)
                        (syntax
                         (let-syntax ((n (lambda (form)
                                           (syntax-case form ()
                                             ((_ x ... (... ...))
                                              (syntax `(x ... (... ...))))))))
                           (n a b c d))))))))
     (test-equal '(a b c d) (m u v)))

   ;;==> (a b c d)

   ;;;=========================================================================
   ;;
   ;; From R5RS:
   ;;
   ;;;=========================================================================

   (define-syntax or
     (syntax-rules ()
       ((or)          #f)
       ((or e)        e)
       ((or e1 e ...) (let ((temp e1))
                        (if temp temp (or e ...))))))

(test-eqv 1 (or #f #f 1))  ;==> 1

   (define-syntax or
     (lambda (form)
       (syntax-case form ()
         ((or)          (syntax #f))
         ((or e)        (syntax e))
         ((or e1 e ...) (syntax (let ((temp e1))
                                  (if temp temp (or e ...))))))))

(test-eqv 1 (or #f #f 1))  ;==> 1

   (let-syntax ((when (syntax-rules ()
                        ((when test stmt1 stmt2 ...)
                         (if test
                             (begin stmt1
                                    stmt2 ...))))))
     (test-eq 'now (let ((if #t))
	 (when if (set! if 'now))
	 if)))                                  ;===>  now

(test-eq 'outer (let ((x 'outer))
    (let-syntax ((m (syntax-rules () ((m) x))))
      (let ((x 'inner))
        (m)))))                              ;===>  outer

(test-eqv 7 (letrec-syntax
      ((my-or (syntax-rules ()
                ((my-or) #f)
                ((my-or e) e)
                ((my-or e1 e2 ...)
                 (let ((temp e1))
                   (if temp
                       temp
                       (my-or e2 ...)))))))
    (let ((x #f)
	  (y 7)
	  (temp 8)
	  (let odd?)
	  (if even?))
      (my-or x
	     (let temp)
	     (if y)
	     y))))                ;===>  7

   (define-syntax cond
     (syntax-rules (else =>)
       ((cond (else result1 result2 ...))
        (begin result1 result2 ...))
       ((cond (test => result))
        (let ((temp test))
          (if temp (result temp))))
       ((cond (test => result) clause1 clause2 ...)
        (let ((temp test))
          (if temp
              (result temp)
              (cond clause1 clause2 ...))))
       ((cond (test)) test)
       ((cond (test) clause1 clause2 ...)
        (let ((temp test))
          (if temp
              temp
              (cond clause1 clause2 ...))))
       ((cond (test result1 result2 ...))
        (if test (begin result1 result2 ...)))
       ((cond (test result1 result2 ...)
              clause1 clause2 ...)
        (if test
            (begin result1 result2 ...)
            (cond clause1 clause2 ...)))))

(test-eq 'ok (let ((=> #f))
    (cond (#t => 'ok))))                   ;===> ok

(test-equal '(2) (cond ('(1 2) => cdr)))                  ;===> (2)

(test-equal 'greater (cond ((> 3 2) 'greater)
        ((< 3 2) 'less)))                 ;===>  greater
(test-equal 'equal (cond ((> 3 3) 'greater)
        ((< 3 3) 'less)
        (else 'equal)))                   ;===>  equal


   ;; Eli Barzilay
   ;; In thread:
   ;; R5RS macros...
   ;; http://groups.google.com/groups?selm=skitsdqjq3.fsf%40tulare.cs.cornell.edu

(test-eqv 4 (let-syntax ((foo
                (syntax-rules ()
                  ((_ expr) (+ expr 1)))))
    (let ((+ *))
      (foo 3))))               ;==> 4

   ;; Al Petrofsky again
   ;; In thread:
   ;; Buggy use of begin in core:primitives cond and case macros.
   ;; http://groups.google.com/groups?selm=87bse3bznr.fsf%40radish.petrofsky.org

(test-eqv 2 (let-syntax ((foo (syntax-rules ()
                      ((_ var) (define var 1)))))
    (let ((x 2))
      (begin (define foo +))
      (cond (else (foo x)))
      x)))                    ;==> 2

   ;; Al Petrofsky
   ;; In thread:
   ;; An Advanced syntax-rules Primer for the Mildly Insane
   ;; http://groups.google.com/groups?selm=87it8db0um.fsf@radish.petrofsky.org

(test-eqv 1 (let ((x 1))
    (let-syntax
        ((foo (syntax-rules ()
                ((_ y) (let-syntax
                           ((bar (syntax-rules ()
                                   ((_) (let ((x 2)) y)))))
                         (bar))))))
      (foo x))))                        ;==> 1

   ;; another example:

(test-eqv 1 (let ((x 1))
    (let-syntax
        ((foo (syntax-rules ()
                ((_ y) (let-syntax
                           ((bar (syntax-rules ()
                                   ((_ x) y))))
                         (bar 2))))))
      (foo x))))                         ;==> 1

   ;; Al Petrofsky

(test-equal '(1 2 3 a) (let ((a 1))
		  (letrec-syntax
		      ((foo (syntax-rules ()
			      ((_ b)
			       (bar a b))))
		       (bar (syntax-rules ()
			      ((_ c d)
			       (cons c (let ((c 3))
					 (list d c 'c)))))))
		    (let ((a 2))
		      (foo a)))))                ;==> (1 2 3 a)

   (define-syntax loop     ;; no change
     (lambda (x)
       (syntax-case x ()
         ((k e ...)
          (with-syntax ((break (datum->syntax (syntax k) 'break)))
            (syntax (call-with-current-continuation
                     (lambda (break)
                       (let f () e ... (f))))))))))

(test-equal '(a a a) (let ((n 3) (ls '()))
		  (loop
		   (if (= n 0) (break ls))
		   (set! ls (cons 'a ls))
		   (set! n (- n 1)))))    ;==> (a a a)

(test-eqv 25 (let ((x '(1 3 5 7 9)))
    (do ((x x (cdr x))
         (sum 0 (+ sum (car x))))
        ((null? x) sum))))                ;==>  25

   (define-syntax define-structure
     (lambda (x)
       (define gen-id
         (lambda (template-id . args)
           (datum->syntax template-id
                          (string->symbol
                           (apply string-append
                                  (map (lambda (x)
                                         (if (string? x)
                                             x
                                             (symbol->string
                                              (syntax->datum x))))
                                       args))))))
       (syntax-case x ()
         ((_ name field ...)
          (with-syntax
              ((constructor (gen-id (syntax name) "make-" (syntax name)))
               (predicate (gen-id (syntax name) (syntax name) "?"))
               ((access ...)
                (map (lambda (x) (gen-id x (syntax name) "-" x))
                     (syntax (field ...))))
               ((assign ...)
                (map (lambda (x) (gen-id x "set-" (syntax name) "-" x "!"))
                     (syntax (field ...))))
               (structure-length (+ (length (syntax (field ...))) 1))
               ((index ...) (let f ((i 1) (ids (syntax (field ...))))
                              (if (null? ids)
                                  '()
                                  (cons i (f (+ i 1) (cdr ids)))))))
            (syntax (begin
                      (define constructor
                        (lambda (field ...)
                          (vector 'name field ...)))
                      (define predicate
                        (lambda (x)
                          (and (vector? x)
                               (= (vector-length x) structure-length)
                               (eq? (vector-ref x 0) 'name))))
                      (define access (lambda (x) (vector-ref x index))) ...
                      (define assign
                        (lambda (x update)
                          (vector-set! x index update)))
                      ...)))))))

   (define-structure tree left right)
   (define t
     (make-tree
      (make-tree 0 1)
      (make-tree 2 3)))

(test-equal '#(tree #(tree 0 1) #(tree 2 3)) t)                     ;==> #(tree #(tree 0 1) #(tree 2 3))
(test-eq #t (tree? t))             ;==> #t
(test-equal '#(tree 0 1) (tree-left t))         ;==> #(tree 0 1)
(test-equal '#(tree 2 3) (tree-right t))        ;==> #(tree 2 3)
(set-tree-left! t 0)
(test-equal '#(tree 0 #(tree 2 3)) t)                     ;==> #(tree 0 #(tree 2 3))




   ;; Quasisyntax tests:

   (define-syntax swap!
     (lambda (e)
       (syntax-case e ()
         ((_ a b)
          (let ((a (syntax a))
                (b (syntax b)))
            (quasisyntax
             (let ((temp (unsyntax a)))
               (set! (unsyntax a) (unsyntax b))
               (set! (unsyntax b) temp))))))))

   (let ((temp 1)
         (set! 2))
     (swap! set! temp)
     (test-eqv temp 2)
     (test-eqv set! 1)
     (values temp set!))   ;==> 2 1

   (define-syntax case
     (lambda (x)
       (syntax-case x ()
         ((_ e c1 c2 ...)
          (quasisyntax
           (let ((t e))
             (unsyntax
              (let f ((c1    (syntax c1))
                      (cmore (syntax (c2 ...))))
                (if (null? cmore)
                    (syntax-case c1 (else)
                      ((else e1 e2 ...)    (syntax (begin e1 e2 ...)))
                      (((k ...) e1 e2 ...) (syntax (if (memv t '(k ...))
                                                       (begin e1 e2 ...)))))
                    (syntax-case c1 ()
                      (((k ...) e1 e2 ...)
                       (quasisyntax
                        (if (memv t '(k ...))
                            (begin e1 e2 ...)
                            (unsyntax
                             (f (car cmore) (cdr cmore))))))))))))))))

(test-eq 'yes (case 'a
    ((b c) 'no)
    ((d a) 'yes)))  ;==> yes

   (define-syntax let-in-order
     (lambda (form)
       (syntax-case form ()
         ((_ ((i e) ...) e0 e1 ...)
          (let f ((ies (syntax ((i e) ...)))
                  (its (syntax ())))
            (syntax-case ies ()
              (()            (quasisyntax (let (unsyntax its) e0 e1 ...)))
              (((i e) . ies) (with-syntax (((t) (generate-temporaries '(t))))
                               (quasisyntax
                                (let ((t e))
                                  (unsyntax
                                   (f (syntax ies)
                                      (quasisyntax
                                       ((i t) (unsyntax-splicing its)))))))))))))))

(test-eqv 3 (let-in-order ((x 1)
                 (y 2))
		(+ x y)))                ;==> 3

(test-equal '((1 a) (2 a) (3 a)) (let-syntax ((test-ellipses-over-unsyntax
			      (lambda (e)
				(let ((a (syntax a)))
				  (with-syntax (((b ...) '(1 2 3)))
				    (quasisyntax
				     (quote ((b (unsyntax a)) ...))))))))
		  (test-ellipses-over-unsyntax)))

                                        ;==> ((1 a) (2 a) (3 a))

   ;; Some tests found online (Guile?)

(test-equal '(list 3 4) (let-syntax ((test
			      (lambda (_)
				(quasisyntax
				 '(list (unsyntax (+ 1 2)) 4)))))
		  (test)))
                                        ;==> (list 3 4)

(test-equal '(list a 'a) (let-syntax ((test
			      (lambda (_)
				(let ((name (syntax a)))
				  (quasisyntax '(list (unsyntax name) '(unsyntax name)))))))
		  (test)))
                                        ;==> (list a 'a)

(test-equal '(a 3 4 5 6 b) (let-syntax ((test
			      (lambda (_)
				(quasisyntax '(a (unsyntax (+ 1 2)) (unsyntax-splicing (map abs '(4 -5 6))) b)))))
		  (test)))
                                        ;==> (a 3 4 5 6 b)

(test-equal '((foo 7) . 7) (let-syntax ((test
			      (lambda (_)
				(quasisyntax '((foo (unsyntax (- 10 3))) (unsyntax-splicing (cdr '(5))) . (unsyntax (car '(7))))))))
		  (test)))
                                        ;==> ((foo 7) . 7)

(test-eqv 5 (let-syntax ((test
                (lambda (_)
                  (quasisyntax (unsyntax (+ 2 3))))))
    (test)))
                                        ;==> 5

(test-equal '(a (quasisyntax (b (unsyntax (+ 1 2)) (unsyntax (foo 4 d)) e)) f)
  (let-syntax ((test
                (lambda (_)
                  (quasisyntax
                   '(a (quasisyntax (b (unsyntax (+ 1 2)) (unsyntax (foo (unsyntax (+ 1 3)) d)) e)) f)))))
    (test)))
                                        ;==> (a (quasisyntax (b #,(+ 1 2) #,(foo 4 d) e)) f)

(test-equal '(a (quasisyntax (b (unsyntax x) (unsyntax (syntax y)) d)) e)
    (let-syntax ((test
                (lambda (_)
                  (let ((name1 (syntax x)) (name2 (syntax y)))
                    (quasisyntax
                     '(a (quasisyntax (b (unsyntax (unsyntax name1)) (unsyntax (syntax (unsyntax name2))) d)) e))))))
    (test)))
                                        ;==> (a (quasisyntax (b #,x #,(syntax y) d)) e)

   ;; Bawden's extensions:

(test-equal '(a 1 2 b) (let-syntax ((test
			      (lambda (_)
				(quasisyntax '(a (unsyntax 1 2) b)))))
		  (test)))
                                        ;==> (a 1 2 b)

(test-equal '(a 1 2 3 4 b) (let-syntax ((test
			      (lambda (_)
				(quasisyntax '(a (unsyntax-splicing '(1 2) '(3 4)) b)))))
		  (test)))
                                        ;==> (a 1 2 3 4 b)

(test-equal '#`(#,(a b c) #,@(a b c) (unsyntax a b c) (unsyntax-splicing a b c))
  (let-syntax ((test
                (lambda (_)
                  (let ((x (syntax (a b c))))
                    (quasisyntax '(quasisyntax ((unsyntax (unsyntax x))
                                                (unsyntax-splicing (unsyntax x))
                                                (unsyntax (unsyntax-splicing x))
                                                (unsyntax-splicing (unsyntax-splicing x)))))))))
    (test)))

   ;;==> (quasisyntax (#,(a b c) #,@(a b c) (unsyntax a b c) (unsyntax-splicing a b c)))
   ;;     which is equivalent to
   ;;    (quasisyntax (#,(a b c) #,@(a b c) #,a #,b #,c #,@a #,@b #,@c)
   ;;     in the Bawden prescripion

   ;; QUASIQUOTE tests:

(test-equal '(list 3 4) `(list ,(+ 1 2) 4))                                ;==> (list 3 4)

(test-equal '(list a 'a) (let ((name 'a)) `(list ,name ',name)))            ;==> (list a (quote a))

(test-equal '(a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))             ;==> (a 3 4 5 6 b)

(test-equal '((foo 7) . cons ) `(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))) ;==> ((foo 7) . cons)

(test-equal '#(10 5 2 4 3 8) `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))         ;==> #(10 5 2 4 3 8)

(test-equal '(foo foo foo) (let ((name 'foo))
		  `((unquote name name name))))                    ;==> (foo foo foo)

(test-equal '(foo foo foo)
  (let ((name '(foo)))
    `((unquote-splicing name name name))))           ;==> (foo foo foo)

(test-equal '`(foo (unquote (append x y) (sqrt 9)))
  (let ((q '((append x y) (sqrt 9))))
    ``(foo ,,@q)))              ;==> `(foo (unquote (append x y) (sqrt 9)))

(test-equal '(foo (2 3 4 5) 3)
  (let ((x '(2 3))
        (y '(4 5)))
    `(foo (unquote (append x y) (sqrt 9))))) ;==> (foo (2 3 4 5) 3)

(test-equal '(a `(b ,(+ 1 2) ,(foo 4 d) e) f) `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))  ;==>  (a `(b ,(+ 1 2) ,(foo 4 d) e) f)

(test-equal '(a `(b ,x ,'y d) e)  (let ((name1 'x)
		   (name2 'y))
	       `(a `(b ,,name1 ,',name2 d) e)))         ;==>  (a `(b ,x ,'y d) e)





(test-end)
