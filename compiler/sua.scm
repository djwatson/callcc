;; "Storage use analysis and its applications" Manuel Serrano, Marc Feeley
;; and the follow-up paper
;; "Abstract compilation: a new implementation paradigm for static analysis" Dominique Boucher, Marc Feeley

(define db (make-hash-table eq?))
(define changed #f)
(define (update-db sexp types)
  (let* ((prev (hash-table-ref/default db sexp '()))
	 (new (lset-union eq? types prev)))
    (unless (lset= eq? new prev)
      (set! changed #t)
      (hash-table-set! db sexp new))))

;; (define math-ops '(ADD SUB MUL DIV))
;; (define (analyze s env)
;;   (let ((types
;; 	 (match s
;; 	   ((let ((,args ,inits) ___) ,body)
;; 	    (for (arg init) (args inits)
;; 		 (update-db arg (analyze init env)))
;; 	    (analyze body env))
;; 	   ((primcall ,op ,arg1 ,arg2)
;; 	    (guard (memq op math-ops))
;; 	    (let ((t1 (analyze arg1 env))
;; 		  (t2 (analyze arg2 env)))
;; 	      (if (or (equal? t1 '(flonum))
;; 		      (equal? t2 '(flonum)))
;; 		  '(flonum)
;; 		  '(number))))
;; 	   ((if ,test ,true ,false)
;; 	    (analyze test env)
;; 	    (let ((true-type (analyze true env))
;; 		  (false-type (analyze false env)))
;; 	      (lset-union eq? true-type false-type)))
;; 	   ((begin ,args ___ ,tail)
;; 	    (for arg args
;; 		 (analyze arg env))
;; 	    (analyze tail env))
;; 	   ((loop ,vars ,name ,body ,inits ___)
;; 	    (for (var init) (vars inits)
;; 		 (update-db var (analyze init env)))
;; 	    (analyze body (append (list (cons name s)) env)))
;; 	   ((quote ,x) '(unkown))
;; 	   ((call ,loopvar ,args ___)
;; 	    (guard (assq loopvar env))
;; 	    (let ((v (cdr (assq loopvar env)))
;; 		  (arg-types (omap arg args (analyze arg env))))
;; 	      (for (arg-type arg-name) (arg-types (second v))
;; 		   (update-db arg-name arg-type))
;; 	      (hash-table-ref/default db v '())))
;; 	   ;; ((call (lookup inexact) ,var)
;; 	   ;;  (analyze var env)
;; 	   ;;  '(flonum))
;; 	   ;; ((primcall INEXACT ,var)
;; 	   ;;  (analyze var env)
;; 	   ;;  '(flonum))
;; 	   ((call ,args ___)
;; 	    (for arg args (analyze arg env))
;; 	    '(unknown))
;; 	   (,var
;; 	    (guard (symbol? var))
;; 	    (hash-table-ref/default db var '(unknown)))
;; 	   ((lookup ,var)
;; 	    (hash-table-ref/default db var '(unknown)))
;; 	   ((primcall ,op ,args ___)
;; 	    ;; Unknown call or something.
;; 	    (for arg args (analyze arg env))
;; 	    '(unknown))
;; 	   (,flonum
;; 	    (guard (inexact? flonum))
;; 	    '(flonum))
;; 	   (,num
;; 	    (guard (number? num))
;; 	    '(number))
;; 	   (,else '(unknown)))))
;;     (update-db s types)
;;     types))
;; (define (op-to-flonum-op op)
;;   (string->symbol (string-append "f" (string-map char-downcase (symbol->string op)))))
;; (define (sua s type env)
;;   (match s
;;     ((primcall ,op ,a ,b)
;;      (guard (and (memq op math-ops)
;; 		 (equal? '(flonum) (hash-table-ref/default db a '()))
;; 		 (equal? '(flonum) (hash-table-ref/default db b '()))))
;;      (let ((op `(flonum-op ,(op-to-flonum-op op) ,(sua a 'flonum env) ,(sua b 'flonum env))))
;;        (if (eq? type 'flonum)
;; 	   op
;; 	   `(primcall FLONUM_BOX ,op))))
;;     ((let ((,names ,inits) ___) ,body)
;;      (let* ((types (omap name names (if (equal? (hash-table-ref/default db name '()) '(flonum))
;; 					   'flonum
;; 					   'unknown)))
;; 	    (inits (omap (type init) (types inits)
;; 			 (sua init type env))))
;;        `(let ((,names ,inits) ___)
;; 	  ,(sua body type env))))
;;     ((loop ,vars ,name ,body ,inits ___)
;;      (let* ((types (omap var vars (if (equal? (hash-table-ref/default db var '()) '(flonum))
;; 					   'flonum
;; 					   'unknown)))
;; 	    (inits (omap (type init) (types inits)
;; 			 (sua init type env))))
;;        `(loop (,vars ___) ,name ,(sua body 'unknown (append (list (cons name types)) env)) ,inits ___)))
;;     (,flonum
;;      (guard (inexact? flonum))
;;      (if (eq? 'flonum type)
;; 	 `(primcall FLONUM_UNBOX ,flonum)
;; 	 flonum))
;;     ((call ,loopvar ,args ___)
;;      (guard (assq loopvar env))
;;      (let* ((arg-types (cdr (assq loopvar env)))
;; 	    (args (map (lambda (arg type) (sua arg type env)) args arg-types)))
;;        `(call ,loopvar ,args ___)))
;;     ((,args ___)
;;      (omap arg args (sua arg 'unknown env)))
;;     (,var
;;      (guard (hash-table-exists? db var))
;;      (let ((var-type (if (equal? (hash-table-ref/default db var '()) '(flonum))
;; 			 'flonum
;; 			 'unknown)))
;;        (if (eq? var-type 'flonum)
;; 	   (if (eq? type 'flonum)
;; 	       var
;; 	       `(primcall FLONUM_BOX ,var))
;; 	   (if (eq? type 'flonum)
;; 	       `(primcall FLONUM_UNBOX ,var)
;; 	       var))))
;;     (,else
;;      else)))

(define escaping-var (make-hash-table eq?))
(define (analyze-escapes sexp)
  (define (escapes-set! var val)
    (let ((prev (hash-table-ref/default escaping-var var #f)))
      (unless (or (eq? prev #t) (equal? prev val))
	(set! changed #t)
	(hash-table-set! escaping-var var val))))
  (let update ((sexp sexp) (bindings '()))
    (define-pass check x
      ((labels ((,labels (nlambda ,names (case ,args ,bodies) ___)) ___) ,body)
       (update body (append labels bindings))
       ;; for each label
       (for (label args bodies) (labels args bodies)
	    ;; for each case
	    (for (args body) (args bodies)
		 (update body (append (to-proper args) labels bindings)))
	    ;; Now update the label with escapes info for all vars.
	    (escapes-set! label
			  (omap arg (to-proper (car args))
				   (hash-table-ref/default escaping-var arg #f)))
	    ;; TODO: update for all cases, currently only support a single case
	    (unless (= 1 (length args)) (escapes-set! label #t)))
       x)
      ;; Add let vars to escape table.
      ((let ((,binds ,(check inits)) ___) ,body)
       (update body (append binds bindings))
       x)
      ;; set vars escape
      ((set! ,sym ,val)
       (update val bindings)
       (escapes-set! sym #t)
       x)
      ;; Label-call only escapes if it escapes in the labeled function
      ;; (it's a intra-procedural analysis)
      ((label-call ,label ,args ___)
       (let ((fescape (hash-table-ref/default escaping-var label #f)))
	 (when fescape
	   (if (and (list? fescape) (= (length args) (length fescape)))
	       (for (escapes arg) (fescape args)
		    (when (and (memq arg bindings) escapes)
		      (escapes-set! arg #t)))
	       ;; Otherwise, all escape
	       (for arg args
		    (when (memq arg bindings)
		      (escapes-set! arg #t)))))
	 (for arg args
	      (unless (memq arg bindings)
		(check arg))))
       x)
      ;; Calls with sym in call position, sym does not escape.
      ((call ,sym ,args ___)
       (guard (memq sym bindings))
       (for-each check args)
       x)
      ;; Otherwise, sym escapes.
      (,sym
       (guard (memq sym bindings))
       (escapes-set! sym #t)
       sym))
    (check sexp)))

(define-pass update-callcc-oneshot
  ((label-call ,callcc #f
	 (labels ((,label (nlambda ,name (case (,closure ,k ,args ___) ,body))))
		 ,label-body))
   (guard (and (not (hash-table-ref/default escaping-var k #f))
	       (eq? 'call-with-current-continuation (hash-table-ref/default global-defs-inv callcc #f))))
   (display (format "Update ~a to oneshot\n" label) (current-error-port))
   `(label-call ,(hash-table-ref global-defs 'call-with-current-continuation-oneshot) #f
	  (labels ((,label (nlambda ,name (case (,closure ,k ,args ___) ,(update-callcc-oneshot body)))))
		  ,label-body))))

(define (storage-use-analysis s)
  (let loop ((i 0))
    (analyze-escapes s)
    (when changed
      (set! changed #f)
      (loop (+ i 1))))
  ;; Now update
  (update-callcc-oneshot s))
