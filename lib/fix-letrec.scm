(define-record-type binding (make-binding free complex init var) binding?
		    (free binding-free)
		    (complex binding-complex)
		    (init binding-init)
		    (var binding-var))

;; To handle: if begin letrec letrec*  case set! define quote primcall
;; consts: bytevector, char, boolean, number, string const-label vector, null, symbol, pair
(define (%fix-letrec expr)
  (match expr
    ((begin ,(%fix-letrec free complex expr assigned) ___)
     (values (set-union* free)
	     (or* complex)
	     `(begin ,@expr)
	     (set-union* assigned)))
    ((if ,(%fix-letrec test-free test-complex test-expr test-assigned)
	 ,(%fix-letrec true-free true-complex true-expr true-assigned)
	 ,(%fix-letrec false-free false-complex false-expr false-assigned))
     (values (lset-union eq? test-free true-free false-free)
	     (any (lambda (x) x) (list test-complex true-complex false-complex))
	     `(if ,test-expr ,true-expr ,false-expr)
	     (lset-union eq? test-assigned true-assigned false-assigned)))
    ((set! ,var ,(%fix-letrec free complex expr assigned))
     (values (lset-union eq? (list var) free)
	     #t
	     `(set! ,var ,expr)
	     (lset-union eq? (list var) assigned)))
    ((define ,var ,(%fix-letrec free complex expr assigned))
     (values (lset-union eq? (list var) free)
	     #t
	     `(define ,var ,expr)
	     assigned))
    ((letrec* ((,vars  ,(%fix-letrec init-free init-complex init-expr init-assigned)) ___)
       ,(%fix-letrec body-free body-complex body-expr body-assigned))
     (let* ((assigned (set-union* `(,body-assigned ,@init-assigned)))
	    (bindings (omap (free complex expr var) (init-free init-complex init-expr vars)
			    (make-binding free complex expr var)))
	    (deps (let loop ((bindings bindings) (deps '()) (last-complex #f))
		    (if (pair? bindings)
			(let* ((binding (car bindings))
			       (depend (filter (lambda (x) (memq x vars)) (binding-free binding)))
			       (depend* (if (and (or (binding-complex binding)
						     ;; If the binding is complex, *or*
						     ;; it is a variable reference to an assigned var,
						     ;; and a letrec*, then we need to add an ordering constraint.
						     (and (symbol? (binding-init binding))
							  (memq (binding-init binding) assigned)))
						 last-complex)
					    (lset-union eq? (list last-complex) depend)
					    depend)))
			  (loop (cdr bindings) (alist-cons (binding-var binding) depend* deps)
				(if (binding-complex binding) (binding-var binding) last-complex)))
			deps)))
	    (bind-map (map cons vars bindings))
	    (scc (tarjan-scc deps))
	    (not-assigned-lambda? (lambda (bind)
				    (let ((init (binding-init bind)))
				      (and (pair? init)
					   (eq? 'lambda (car init))
					   (not (memq (binding-var bind) assigned))))))
	    (new-body (fold-right
		       (lambda (scc expr)
			 (if (= 1 (length scc))
			     (let ((bind (cdr (assq (car scc) bind-map))))
			       (cond 
				((not-assigned-lambda? bind)
				 (build-fix (list (binding-var bind)) (list (binding-init bind))
					    expr))
				((not (memq (binding-var bind) (binding-free bind)))
				 (build-let (list (binding-var bind)) (list (binding-init bind))
					    expr))
				(else (build-let (list (binding-var bind)) (list #f)
						 (build-set! (list (binding-var bind)) (list (binding-init bind))
							     expr)))))
			     ;; Multi-scc case
			     (let-values (((unset set)
					   (partition not-assigned-lambda?
						      (omap var scc
							    (cdr (assq var bind-map))))))
			       ;; Orderer the complex bindings in their occurance in the original form
			       
			       (let ((seto (filter
					    (lambda (bind)
					      (memq bind set)) bindings)))
				 ;; (display "SET:") (display (length seto)) (display ":")
				 ;; (display (map binding-var seto)) (newline)
				 ;; (display "UNSET:") (display (length unset)) (display ":")
				 ;; (display (map binding-var unset)) (newline)
				 ;; (display "DEPS:") (pretty-print (filter-map (lambda (x)
				 ;; 					       (if (memq (car x) scc)
				 ;; 						   (filter (lambda (x) (memq x scc)) x)
				 ;; 						   #f)) deps))
				 ;; (newline)
				 (build-let (map binding-var seto) (make-list (length seto) #f)
					    (build-fix (map binding-var unset) (map binding-init unset)
						       (build-set! (map binding-var seto) (map binding-init seto) expr)))))))
		       body-expr (reverse scc))))
       ;; (when (> (length deps) 1)
       ;; 	  (display deps) (newline) (newline))
       ;; (display "Tarjan:") (display scc) (newline)
       (values (lset-difference eq? (set-union* `(,body-free ,@init-free)) vars)
	       (or (or* init-complex) body-complex)
	       new-body
	       assigned)))
    ((letrec ((,args ,inits) ___) ,body)
     (error "bad letrec"))
    ((lambda (case ,args ,(%fix-letrec free complex expr2 assigned)) ___)
     (values (set-union* (omap (args free) (args free)
			       (lset-difference eq? free (to-proper args))))
	     #f
	     `(lambda (case ,args ,expr2) ___)
	     (set-union* assigned)))
    ((quote ,x)
     (values '() #f `(quote ,x) '()))
    ;; Calls
    ((primcall ,name ,(%fix-letrec free complex expr assigned) ___)
     (values (set-union* free)
	     #f
	     `(primcall ,name ,@expr)
	     (set-union* assigned)))
    ((call ,(%fix-letrec free complex expr assigned) ___)
     (values (set-union* free)
	     #t
	     `(call ,@expr)
	     (set-union* assigned)))
    ;; TODO: make primcall?
    ((global-set! ,var ,vard)
     (values (list vard)
	     #f
	     `(global-set! ,var ,vard)
	     '()))
    ((const-init ,label ,(%fix-letrec free complex expr assigned))
     (values free
	     complex
	     `(const-init ,label ,expr)
	     assigned))
    ((unlikely ,(%fix-letrec free complex expr assigned))
     (values free complex `(unlikely ,expr) assigned))
    (,v
     (guard (symbol? v))
     (values `(,v) #f v '()))
    ;; Constants
    (,else
     (guard (not (pair? else)))
     (values '() #f else '()))))

(define (set-union* set*)
  (apply lset-union eq? set*))

(define (or* obj*)
  (and (not (null? obj*))
       (or (car obj*)
	   (or* (cdr obj*)))))

(define (fix-letrec expr)
  (let-values (((free complex expr assigned) (%fix-letrec expr)))
    expr))

