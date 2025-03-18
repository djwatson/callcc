
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some routines for parsing output of expander
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type const-label (%make-const-label label needs-label) const-label?
		    (label const-label-label)
		    (needs-label const-needs-label))

(define (make-const-label needs-label)
  (%make-const-label (gen-label "constant") needs-label))
;; TODO just put these in the expander directly.
(define (unquoted? x)
  (or
   (bytevector? x)
   (const-label? x)
   (char? x)
    (boolean? x)
    (number? x)
    (string? x)
    (vector? x)))
(define (datum x) (or (null? x) (pair? x) (symbol? x) (unquoted? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
AST layout:

It's the standard scheme layout for the most part.

New rules:
fix: (fix (var-names ...) var-inits ... body)

We prepend 'primcall' or 'call' to (op expr ...) for better error checking:
we can determine call vs. builtin/primitive easily, instead of matching 'op'
against anything.

'match' is used for most destructuring. 
Hints:
* Always guard matching uvars with symbol?, otherwise ,uvar matches anything
* We use destructive map! and set-car! calls for improved performance.
* Therefore you can't use catamorphism with guard! You have to guard
  *first*, then explicitly call the recursive path.

TODO: we could uniq-ify uvars if we wanted: make a record for them?
        then it could hold assigned?
TODO: boxes could be passed down through funcs
 |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map! proc lst)
  (when (pair? lst)
    (set-car! lst (proc (car lst)))
    (map! proc (cdr lst))))

;; AST node-walker.
;; Pull out the matcher so we're not re-generating it
;; for every rule.
(define (continue-pass proc rule)
  (match rule
    (,any
     ;; Rules.
     (guard (pair? any))
     (case (car any)
       ;; Specials for let, letrec, letrec*, where the first arg is
       ;; partially walked.
       ((let let* letrec letrec* labels)
	(for val (second rule)
	     (set-car! (cdr val) (proc (cadr val))))
	(set-car! (cddr rule) (proc (third rule)))
	rule)
       ;; Items we *do* walk the first arg.
       ((begin if call closure lambda) (map! proc (cdr any)))
       ;; Everything else, the first arg is ignored.
       (else
	(map! proc (cddr any))))
     any)
    ;; Things that just pass through, but we'll do some error checking:
    ;; Datum.
    (,datum
     (guard (unquoted? datum))
     (if (vector? datum) `',datum
	 datum)
     )
    ;; uvars.
    (,any
     (guard (symbol? any))
     any)))
(define-syntax define-pass
  (syntax-rules ()
    ((_ name (rules ...) ...)
     (define-pass name x (rules ...) ...))
    ((_ name arg (rules ...) ...)
     (define (name arg)
       ;;(display "Running " (current-error-port)) (write arg (current-error-port)) (newline (current-error-port))
       (match arg
	 (rules ...) ...
	 (,any
	  (continue-pass name any)))))))

(define-pass parse-expanded x
  ;; TODO can remove?  primcalls should only ever be calls.
   ;; TODO error
  ((primcall ,op)
   `(primref ,op))
  (((primcall ,op) ,(parse-expanded args) ___)
   `(primcall ,op ,args ___))
  ;; Normalize case-lambda & lambda
  ((lambda ,args ,(parse-expanded body))
   `(lambda (case ,args ,body)))
  ((case-lambda (,args ,(parse-expanded body)) ___)
   `(lambda (case ,args ,body) ___))
  ;; Anything else that's not a special operator is a call.
  ((,(parse-expanded op) ,args ___)
   (guard (not (memq op '(if begin letrec letrec* lambda case-lambda case set! define quote))))
   `(call ,op ,@(map parse-expanded args))))

(define (is-bignum? x)
  (and (number? x) (exact? x) (integer? x)
       (or (> x #xffffffffffff)
	   (< x #x-ffffffffffff))))

(define (is-ratnum? x)
  (and (number? x) (exact? x) (rational? x) (not (integer? x))))

(define (is-compnum? x)
   (and (number? x) (not (real? x)) (not (rational? x))))

(define gen-label
  (let ((num 0))
    (lambda (str)
      (set! num (+ num 1))
      (string-append  str "_" (number->string num)))))

(define (lift-bignums e)
  (define (contains-complex-const? x)
    (match x
      ((,x . ,y)
       (or (contains-complex-const? x) (contains-complex-const? y)))
      (#(,x ___)
       (any contains-complex-const? x))
      (,x
       (guard (or (is-compnum? x) (is-bignum? x) (is-ratnum? x) ))
       #t)
      (,else
       #f)))
  (define (build-complex x)
    (match x
      ((,(build-complex x) . ,(build-complex y))
       `(call cons ,x ,y))
      (#(,(build-complex x) ___)
       `(call vector ,x ___))
      (,x
       (guard (or (is-compnum? x) (is-bignum? x) (is-ratnum? x) ))
       `(call string->number ,(number->string x)))
      (,else
       `(quote ,else))))
  (define bignums '())
  (define-pass lift
    ((quote ,x)
     (guard (contains-complex-const? x))
     (let ((tmp (make-const-label #t)))
       (push! bignums (cons (build-complex x) tmp))
       tmp))
    (,x
     (guard (or (is-compnum? x)(is-bignum? x) (is-ratnum? x) ))
     (cond
      ((assoc x bignums) => cdr)
      (else
       (let ((tmp (make-const-label #t)))
	 (push! bignums (cons `(call string->number ,(number->string x)) tmp))
	 tmp)))))
  (define res (lift e))
  (define (is-string->number? x) (and (pair? x) (eq? 'define (first x)) (eq? 'string->number (second x))))
  (define-values (before after) (split-at (cdr res) (+ 1 (or (list-index is-string->number? (cdr res)) -1))))
  `(begin
     ,@before
     ,@(omap num bignums `(primcall const-init ,(cdr num) ,(car num)))
     ,@after))

(define ops '((< . LT)
	      (> . GT)
	      (<= . LTE)
	      (>= . GTE)
	      (= . NUM_EQ)
	      (+ . ADD)
	      (- . SUB)
	      (* . MUL)
	      (eq? . EQ)
	      (eqv? . EQV)
	      (char=? . EQ)))
(define uni-ops '((char->integer . CHAR_INTEGER)
		  (integer->char . INTEGER_CHAR)
		  (inexact . INEXACT)))
(include "memory_layout.scm")
(define guards `((null? . ,nil-tag)
		 (pair? . ,cons-tag)
		 (boolean? . ,bool-tag)
		 (char? . ,char-tag)
		 (symbol? . ,symbol-tag)
		 (procedure? . ,closure-tag)
		 (vector? . ,vector-tag)
		 (bytevector? . ,bytevector-tag)
		 (string? . ,string-tag)
		 (fixnum? . ,fixnum-tag)))
(define-pass integrate-r5rs
  ;; TODO do length checking
  ((call ,op ,a ,b)
   (guard (assq op ops))
   `(primcall ,(cdr (assq op ops)) ,(integrate-r5rs a) ,(integrate-r5rs b)))
  ((call ,op ,a)
   (guard (assq op uni-ops))
   `(primcall ,(cdr (assq op uni-ops)) ,(integrate-r5rs a)))
  ((call ,op ,a)
   (guard (assq op guards))
   `(primcall GUARD  ,(integrate-r5rs a) ,(cdr (assq op guards))))
  ((call zero? ,(integrate-r5rs a))
   `(primcall NUM_EQ ,a 0))
  ;; TODO: multiple math ops.
  ;; caar, cddr, cadr
  ;; car, cdr, set-car!, set-cdr!, cons, vector-ref, vector-length, string-length, string-ref, string-set!, vector-set!
  ;; symbol-string, string->symbol, round, vector, eqv?, string=, stringcmp,  record?,
  ;; memv, port?
  ((if (call not ,test) ,a ,b)
   (integrate-r5rs `(if ,test ,b ,a)))
  ((not ,(integrate-r5rs a)) ;; This one could be a builtin
   `(if ,a #f #t)))


;; TODO move to util? 
(define-syntax ->
  (syntax-rules ()
    ((_ arg (command args ...) rest ...)
     (->
      (command arg args ...) rest ...))
    ((_ arg command rest ...)
     (-> (command arg) rest ...))
    ((_ arg) arg)))

(define (make-a-program prog)
  (define sexp (cdr prog))
  (define (doit sexp defs)
    (if (null? (cdr sexp))
	`(letrec* ,(reverse defs)
	   ,(car sexp))
	(match (car sexp)
	  ((define ,var ,val)
	   (if (assq var defs)
	       (begin
		 (display (format "WARNING: multiple definition of ~a\n" var) (current-error-port))
		 (doit (cdr sexp) (cons (list (gen-sym 'unused) `(set! ,var ,val)) defs)))
	       (doit (cdr sexp) (cons
				 (list (gen-sym 'unused) `(global-set! ,var ,var))
				 (cons (list var val) defs)))))
	  (,else
	   (doit (cdr sexp) (cons (list (gen-sym 'unused) else) defs))))))
  `(begin ,(doit sexp '())))


;; Requires alpha-renamed
(define assigned (make-hash-table eq?))
(define-pass find-assigned arg
  ((set! ,var ,(find-assigned exp))
   (hash-table-set! assigned var #t)
   arg))

(define (lambda-expr? x)
  (match x
    ((lambda ,f ___) #t)
    (,x #f)))

(define (build-set! vars inits body)
  (if (null? vars)
      body
      `(begin (set! ,vars ,inits) ___
	      ,body)))

;; We don't actually have let yet, use a lambda call.
(define (build-let vars inits body)
  (if (null? vars)
    body
    `(call (lambda (case ,vars ,body)) ,@inits)))

(define (build-fix vars inits body)
  (if (null? vars)
    body
    `(fix ,vars ,@inits
	,body)))

(define (find-fixed vars inits)
  (filter-map
    (lambda (uvar init)
      (and
        (lambda-expr? init)
        (not (hash-table-exists? assigned uvar))
        (list uvar init)))
    vars
    inits))

(define (find-not-fixed vars inits fixed)
  (filter-map
    (lambda (uvar init)
      (and
        (not (assq uvar fixed))
        (list uvar init)))
    vars
    inits))

;; Add 'lookup' to global variables.
(define (find-globals x)
  (define (find x locals)
    (define-pass pass
      ((case ,args ,body)
       `(case ,args ,(find body (append (to-proper args) locals))))
      ((fix ,vars ,inits ___ ,body)
       (let* ((new-vars (append vars locals))
	      (new-inits (omap init inits
			       (find init new-vars))))
	 `(fix ,vars ,new-inits ___ ,(find body new-vars))))
      (,var
       (guard (symbol? var))
       (if (memq var locals)
	   var
	   `(lookup ,var))))
    (pass x))
  (find x '()))

(define (assignment-conversion x)
  ;; TODO cleanup boxes: make it a hash table?
  ;; cleanup uvar to be unique?
  (define boxes '())
  
  (define-pass conv arg
    (,var
     (guard (symbol? var))
     (cond
      ((assq var boxes) => (lambda (x) `(primcall FOREIGN_CALL "SCM_CAR" ,(cdr x))))
      (else arg)))
    ((set! ,var ,(conv body))
     (cond
      ((assq var boxes) => (lambda (x) `(primcall FOREIGN_CALL "SCM_SETCAR" ,(cdr x) ,body)))
      (else `(set! ,var ,body))))
    ((case ,args ,body)
     (let* ((new-boxes (filter-map
		       (lambda (x)
			 (if (hash-table-exists? assigned x)
			     (cons x (gen-sym x))
			     #f))
		       (to-proper args))))
      (set! boxes (append new-boxes boxes))
      `(case ,args
	 ,(build-let (map cdr new-boxes)
		     (map (lambda (x) `(primcall FOREIGN_CALL "SCM_CONS" ,(car x) #f)) new-boxes)
		     (conv body))))))
  (conv x))

;; TODO: we need (list ... ) here, but it's in the scheme-base, which may not be imported???
(define-pass recover-let
  ((call (lambda (case ,args ,body)) ,params ___)
   (guard (list? args) (= (length args) (length params)))
   `(let ,(map list args (map recover-let params)) ,(recover-let body)))
  ((call (lambda (case (,first ___ . ,rest) ,body)) ,params ___)
   (guard (not (null? rest))
     (<= (length first) (length params)))
   (let ((params (recover-let params))
	 (first-params (take params (length first)))
	  (rest-params (drop params (length first))))
     `(let (,@(map list first first-params) (,rest (call (lookup list) ,@rest-params ))) ,(recover-let body))))
  ((call (lambda (case ,arg ,body)) ,params ___)
   (guard (symbol? arg))
   `(let ((,arg (call (lookup list) ,@(map recover-let params)))) ,(recover-let body))))

;; Recover loops
(define (atom? x) (not (pair? x)))
(define (lower-loops f)
  (define (no-refs? var f)
    (if (atom? f)
	(not (eq? f var))
	(if (eq? 'quote (car f))
	    #t
	    (fold (lambda (a b) (and (no-refs? var a) b)) #t f))))
  (define (only-tailcalls? var f)
    (if (atom? f)
	(not (eq? f var))
	(case (car f)
	  ((if) (and (no-refs? var (second f)) (only-tailcalls? var (third f))
		     (or (= 3 (length f)) (only-tailcalls? var (fourth f)))))
	  ((let) (and (no-refs? var (second f)) (only-tailcalls? var (third f))))
	  ((begin) (case (length f)
		     ((1) #t)
		     ((2) (only-tailcalls? var (second f)))
		     (else (and (no-refs? var (second f)) (only-tailcalls? var `(begin ,@(cddr f)))))))
	  ((lambda) (no-refs? var f))
	  ((call)
	   (if (eq? var (cadr f))
	       (no-refs? var (cddr f))
	       (no-refs? var (cdr f))))
	  ((loop)
	   (and (no-refs? var (second f))
		(only-tailcalls? var (fourth f))))
	  (else (no-refs? var f)))))
  (define-pass lower
    ((call (fix (,name) (lambda (case ,vars ,(lower body))) ,name2) ,(lower args) ___)
     (guard (and (eq? name name2) (only-tailcalls? name body)))
     `(loop ,vars ,name ,body ,args ___)))
  (lower f))

;; Give all lambdas a name.

;; TODO: we should track the current name, and give anonymous lambdas
;; named after the function they are defined in.
(define (name-lambdas sexp)
  (define (do-name sexp ns)
    (define-pass namer
      ((define ,var (lambda ,cases ___))
       `(define ,var (nlambda ,(symbol->string var) ,@(omap case cases (do-name case (symbol->string var))))))
      ((set! ,var (lambda ,cases ___))
       `(set! ,var (nlambda ,(symbol->string var) ,@(omap case cases (do-name case (symbol->string var))))))
      ((fix ,vars (lambda ,cases ___) ___ ,(namer fix-body))
       (let ((names (map symbol->string vars)))
	 `(fix ,vars ,@(omap (var cases name) (vars cases names)
			     `(nlambda ,name
				      ,@(omap case cases
					      (do-name case (symbol->string var)))))
	       ,fix-body)))
      ((lambda ,(namer cases) ___)
       `(nlambda ,(symbol->string (string->symbol (string-append ns "-anon"))) ,cases ___)))
    (namer sexp))
  (do-name sexp "main"))

;; Add a fix-form to single lambdas.
(define-pass fix-all x
  ;; Don't need to run on already-fixed things.
  ((fix ,vars (nlambda ,name ,(fix-all cases) ___) ___ ,(fix-all body))
   `(fix ,vars (nlambda ,name ,cases ___) ___ ,body))
  ((nlambda ,name ,(fix-all cases) ___)
   (let ((tmp (gen-sym (string->symbol name))))
     `(fix (,tmp)
	   (nlambda ,name ,cases ___)
	   ,tmp))))

;; We've made new variables above for all-letrec,
;; so this has to come after, since it references variables
(define (uncover-free e)
  (let uncover-free ((e e) (bindings '()) (fv-info (make-hash-table eq?)))
    (define-pass pass x
      (,uv
       (guard (symbol? uv))
       (when (memq uv bindings) (hash-table-set! fv-info uv #t))
       x)
      ((let ((,vars ,(pass inits)) ___) ,body)
       (let ((new-body (uncover-free body (append vars bindings) fv-info)))
	 (for key vars (hash-table-delete! fv-info key))
	 `(let ((,vars ,inits) ___) ,new-body)))
      ;; TODO the same as let?
      ((loop ,vars ,name ,body ,(pass args) ___)
       (let ((new-body (uncover-free body (append vars bindings) fv-info)))
	 (for key vars (hash-table-delete! fv-info key))
	 `(loop ,vars ,name ,new-body ,args ___)))
      ((fix ,vars (nlambda ,name (case ,args ,lbody) ___) ___ ,body)
       (let* ((new-env (append vars bindings))
	      (infos (omap _ vars (make-hash-table eq?)))
	      (new-lbodies
	       (omap (args lbody info) (args lbody infos) ;; For each lambda
		     (omap (args x) (args lbody) ;; For each case
		      (uncover-free x (append (to-proper args) new-env) info))))
	      (new-body (uncover-free body new-env fv-info))
	      (free-vars (omap (args table) (args infos) ;; for each lambda
			       (omap args args ;; for each case
				     (for key (to-proper args) (hash-table-delete! table key)))
			       (hash-table-keys table))))    
	 
	 (for table infos (hash-table-merge! fv-info table))
	 (for key vars (hash-table-delete! fv-info key))
	 (for (var calc-free) (vars free-vars)
	      (define free (lset-difference eq? calc-free vars))
	      (define scc (lset-difference eq? (lset-intersection eq? calc-free vars) (list var)))
	      (hash-table-set! free-table var free)
	      (hash-table-set! scc-table var scc))
	 x)))
    (pass e)))

(define (update-direct-calls sexp)
  (let update ((sexp sexp) (bindings '()))
    (define-pass check
      ((call ,x ,vars ___)
       (guard (memq x bindings))
       `(label-call ,x ,@(map check vars)))
      ((fix ,args ,lambdas ___ ,body)
       (define new-bindings (append args bindings))
       `(fix ,args ,@(omap lambda lambdas (update lambda new-bindings))
	     ,(update body new-bindings)))
      (,sym
       (guard (memq sym bindings))
       (hash-table-set! escapes-table sym #t)
       sym))
    (check sexp)))

(define (tarjan-scc graph)
  (define stack '())
  (define index 0)
  (define indices '())
  (define lowlinks '())
  (define on-stack '())
  (define result '())

  (define (update-lowlink v link)
    (let ((cur (assq v lowlinks)))
      (if cur (set-cdr! cur link)
	  (set! lowlinks (cons (cons v link) lowlinks)))))

  (define (strongconnect v)
    (set! indices (cons (cons v index) indices))
    (set! lowlinks (cons (cons v index) lowlinks))
    (set! index (+ index 1))
    (set! on-stack (cons v on-stack))
  
    (for-each (lambda (w)
                (let ((w-index (assq w indices)))
                  (if (not w-index)
		      (begin
			(strongconnect w)
			(update-lowlink v (min (cdr (assq v lowlinks)) (cdr (assq w lowlinks)))))
		      (when (memq w on-stack)
			(update-lowlink v (min (cdr (assq v lowlinks)) (cdr (assq w indices))))))))
	      (assq v graph))

    (when (= (cdr (assq v lowlinks)) (cdr (assq v indices)))
      (let loop ((scc '()))
	(let ((w (car on-stack)))
          (set! on-stack (cdr on-stack))
          (if (eq? v w)
              (set! result (cons (cons v scc) result))
              (loop (cons w scc)))))))

  (for-each (lambda (v)
              (unless (assq v indices)
                (strongconnect v)))
            (map car graph))
  
  result)

(define scc-table (make-hash-table eq?))
(define free-table (make-hash-table eq?))
(define escapes-table (make-hash-table eq?))

(define-pass scletrec
  ((fix ,vars ,(scletrec lambdas) ___ ,(scletrec body))
   (define tarjan (tarjan-scc (map (lambda (var) (cons var (hash-table-ref scc-table var))) vars)))
   (define bindings (map list vars lambdas))
   (let loop ((sccp tarjan) (body body))
     (if (null? sccp)
	 body
	 (loop (cdr sccp) `(letrec ,(map (lambda (f) (assq f bindings)) (car sccp)) ,body))))))

;; Group all well known procedures with a single not-well-known
(define (scletrec2 sexp)
  (define (update f)
    (if (atom? f)
	f
	(case (car f)
	  ((quote) f)
	  ((letrec)
	   (let* ((bindings (map car (second f)))
		  (new-bindings (map (lambda (f) `(,(car f) ,(update (second f)))) (second f)))
		  (new-body (imap update (cddr f)))
		  (well-known (filter (lambda (b) (not (hash-table-exists? escapes-table b))) bindings))
		  (not-well-known (lset-difference eq? bindings well-known)))
	     (define (binding-set s)
	       (filter (lambda (b) (if (memq (car b) s) b #f)) new-bindings))
	     ;;(display (format "well known: ~a not: ~a\n" well-known not-well-known))
	     (if (null? not-well-known)
		 `(scletrec (,new-bindings) ,@new-body)
		 `(scletrec (,(append (binding-set (list (car not-well-known)))
				       (binding-set well-known))
			      ,@(map binding-set (map list (cdr not-well-known)))) ,@new-body))))
	  (else (imap update f)))))
  (imap update sexp))

;; Calculate final free: We need to add additional free variables if
;; one of its dependant letrec vars needs a non-constant closure.
;; Proced outermost to innermost doing the determination.
(define final-free-table (make-hash-table eq?))
(define (union a b) (lset-union eq? a b))
(define (difference a b) (lset-difference eq? a b))
(define (final-free sexp)
  (define (update f)
    (if (atom? f)
	f
	(case (car f)
	  ((quote) f)
	  ((scletrec)
	   (let ((again #f))
	     (define (analyze-group1 g)
	       (define (try-reduce f)
		 ;;(dformat "try treduce f ~a final-free-table ~a ~a\n" f final-free-table (assq f final-free-table))
		 (if (and (hash-table-exists? final-free-table f)
			  (not (hash-table-exists? escapes-table f))
			  (= 0 (length (hash-table-ref final-free-table f))))
		     #f
		     f))
	       (let* ((free (fold union '() (map (lambda (f) (hash-table-ref free-table (car f))) g)))
		      (reduced-free (filter-map try-reduce free)))
		 ;;(dformat "Analyze group ~a\n" (map car g))
		 (for-each (lambda (f) (hash-table-set! final-free-table (car f) reduced-free)) g)))
	     (define (analyze-group2 g)
	       (define (add-if-needs-closure link g)
		 (define free (hash-table-ref final-free-table link))
		 ;;(dformat "add if needs closure: link ~a free ~a\n" link free)
		 (when (not (null? free))
		   (for-each (lambda (f)
			       (let* ((cur-free (hash-table-ref final-free-table f))
				     (needs-update (not (memq link cur-free))))
				(when needs-update
				  (hash-table-set! final-free-table f (union (list link) cur-free))
				  (set! again #t)
				  )))
			     g)))
	       (define (add-links f g)
		 (define links (hash-table-ref scc-table f))
		 ;;(dformat "Adding links for ~a:~a\n" f links)
		 (for-each (lambda (link) (add-if-needs-closure link g)) (difference links g)))
	       (for-each (lambda (f) (add-links (car f) (map car g))) g))
	     (define (descend g)
	       (map (lambda (f) (update (second f))) g))
	     (for-each analyze-group1 (second f))
	     (let loop ()
	       (set! again #f)
	       (for-each analyze-group2 (second f))
	       (when again (loop)))
	     (for-each descend (second f))
	     (for-each update (cddr f))))
	  (else (imap update f)))))
  (imap update sexp)
  sexp)

;; Final free vars are decided.  Decide on closure representation, and
;; update the code.
(define (closure-conversion-scc sexp)
  (define (update f replace)
    ;;(dformat "update ~a\n" f)
    (if (atom? f)
	(let ((repl (assq f replace)))
	  (if repl (cdr repl) f))
	(case (car f)
	  ((label-call)
	   (if (hash-table-exists? escapes-table (second f))
	       `(label-call ,(second f) ,(cdr (assq (second f) replace))
			    ,@(map (lambda (f) (update f replace)) (cddr f)))
	       (let* ((free-cnt (length (hash-table-ref final-free-table (second f))))
		      (clo (cdr (assq (second f) replace))))
		 (if (= 0 free-cnt)
		     `(label-call ,(second f) ,@(map (lambda (f) (update f replace)) (cddr f)))
		     `(label-call ,(second f) ,clo ,@(map (lambda (f) (update f replace)) (cddr f)))))))
	  ((quote) f)
	  ((scletrec)
	   (let ()
	     (define (is-group-well-known g)
	       (define names (map car g))
	       (fold (lambda (a b) (and a b)) #t
		     (map (lambda (name) (not (hash-table-exists? escapes-table name))) names)))
	     (define (get-free-cnt g)
	       (define representative (caar g))
	       (length (hash-table-ref final-free-table representative)))
	     (let* ((group-well-known (map is-group-well-known (second f)))
		    (groups (second f))
		    (free-cnt (map get-free-cnt (second f)))
		    (closures (map (lambda (wk free) (if (and wk (= free 0)) #f
							   (gen-sym 'closure)))
				   group-well-known free-cnt))
		    (clo-bind (apply append (map (lambda (g clo free-cnt)
						   (map (lambda (f)
							  (cons (car f)
								(if (= 0 free-cnt)
								    `(const-closure (label ,(car f)))
								    clo)))
							g))
						 (second f) closures free-cnt))))
	       (define (update-group g clo)
		 (let* ((representative (caar g))
		       (free (hash-table-ref final-free-table (caar g)))
		       (free-bind (map (lambda (f n) (cons f `(primcall CLOSURE_GET ,clo ,n))) free (iota (length free))))
		       ;; Clo-bind must be done for all groups
		       (new-replace (append free-bind clo-bind replace))
		       ;;(foo (dformat "update grou ~a bindings: ~a\n" (map car g) free-bind))
		       )
		   (define (update-lambda f)
		     (define var (car f))
		     (define lam (second f))
		     (define new-cases
		       (omap case (cddr lam)
			    (define args (second case))
			    (define body (third case))
			    `(case ,(if clo (cons clo args) args)
			       ,(update body new-replace))))
		     `(,var (nlambda ,(second lam) ,@new-cases)))
		   (map update-lambda g)))
	       ;;(dformat "Gen closure \n")
	       ;; The unknown binding that needs a pointer is always first.
	       (define (generate-closure g clo)
		 (let* ((free (hash-table-ref final-free-table (caar g)))
			(all-names (apply append (map (lambda (g) (map car g)) (second f))))
			;; Map function pointers to closures
			(free-replaced (map (lambda (f)
					      (define r (assq f replace))
					      (if r
						  (cdr r)
						  f)) free)))
		   `(,clo (,(if (= (length free-replaced) 0) 'const-closure 'closure)
			   (label ,(caar g))
			   ,@(map (lambda (f) (if (memq f all-names) #f f)) free-replaced)))))
	       (define (generate-closure-set g clo)
		 (define (find-closure f)
		   (let loop ((groups groups) (closures closures))
		     (let loop2 ((lams (car groups)))
		       (if (null? lams)
			   (loop (cdr groups) (cdr closures))
			   (if (eq? f (caar lams))
			       (car closures)
			       (loop2 (cdr lams)))))))
		 (let* ((free (hash-table-ref final-free-table (caar g)))
			(all-names (apply append (map (lambda (g) (map car g)) (second f))))
			(set-names (map car g))
			(other-names (difference all-names set-names)))
		   (if clo
		       (map (lambda (f n) (if (memq f other-names)
					      `(primcall CLOSURE_SET ,clo ,(find-closure f) ,n)
					      #f))
			    free (iota (length free)))
		       '())))
	       (define (id a) a)
	       (define body-clo-bind (apply append (map (lambda (g clo) (map (lambda (f) (cons (car f) clo)) g)) (second f) closures)))
	       ;;(dformat "Well known groups:~a ~a ~a ~a\n" (second f) group-well-known free-cnt closures)
	       ;; The function labels
	       ;; TODO $labels
	       `(labels ,(apply append (map update-group (second f) closures))
			 ;; The closures
		  (let ,(filter-map (lambda (f) (if (car f) f #f)) (map generate-closure (second f) closures))
		    ;; Any letrec groups that need closure-set!
		    (begin
		      ,@(filter-map id (apply append (map generate-closure-set (second f) closures)))
		      ;; The body
		      ,@(imap (lambda (f) (update f (append body-clo-bind replace))) (cddr f))))))))
	  (else (imap (lambda (f) (update f replace)) f)))))
  (imap (lambda (f) (update f '())) sexp))

(define global-defs (make-hash-table eq?))
(define global-labels (make-hash-table eq?))

(define-pass find-global-labels sexp
  ((define ,var (labels ((,label ,lam)) (let ((,closure (const-closure (label ,label2))))
					  (begin ,closure2))))
   (guard (and (eq? label label2) (eq? closure closure2)))
   (if (hash-table-exists? assigned var)
       1;(display (format "Global assigned:~a\n" var) (current-error-port))
       (begin
	 ;;(display (format "Record global ~a\n" var) (current-error-port))
	 (hash-table-set! global-defs var label)
	 (hash-table-set! global-labels label lam)))
   sexp))

(define-pass programify sexp
  ((call (lookup ,global) ,(programify args) ___)
   (if (hash-table-exists? global-defs global)
       `(label-call ,(hash-table-ref global-defs global) #f ,args ___)
       (begin
	 ;(display (format "Global not found:~a\n" global) (current-error-port))
	 `(call (lookup ,global) ,args ___)))))

(define (debug-print x)
  (pretty-print x)
  x)

;; This pass mutates the input, let's make sure we have a fresh copy.
;; Some macro invocations may result in constant, immutable datums
;; being inserted in to the output.
(define (deep-copy x)
  (if (pair? x)
      (cons (deep-copy (car x)) (deep-copy (cdr x)))
      x))

(include "sua.scm")
(define (r7-pass x)
  (-> x
      deep-copy
      parse-expanded
      lift-bignums
      integrate-r5rs
      ;make-a-program
      fix-letrec
      find-assigned
      find-globals
      assignment-conversion
      recover-let
      lower-loops
      name-lambdas
      fix-all
      uncover-free
      update-direct-calls
      scletrec
      scletrec2
      final-free

      closure-conversion-scc

      find-global-labels
      programify
      ;;debug-print
      
;      storage-use-analysis
      ))


