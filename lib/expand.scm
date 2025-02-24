#| 

An expander heavily inspired by the van tonder expander for r6rs on
the srfi website, and used in larceny.

TODO: revert to only a compiled, non-interpreted syntax-rules
TODO: also maybe go back to pre-resolving idents? only internal
      mixed define-syntax and define's are ever an issue, and 
      they are just whacky.

|#

;; Eval used by macros.
(define (expand-eval e)
  (define env (cond-expand ((or chicken chez) (interaction-environment))
			   (else (environment '(scheme base) '(util) '(expand)))))
  (eval e env))

(define cur-namespace (make-parameter ""))
(define base-ellipsis (make-parameter '(...  .  #f)))
(define base-underscore '(_  .  #f))
;; TODO parameterize
(define (ellipsis? x)
  (and (identifier? x) (free-identifier=?2 x (base-ellipsis))))
(define (underscore? x)
  (and (identifier? x) (free-identifier=?2 x base-underscore)))
(define (expand-with-ellipsis x)
  (define el (first x))
  (parameterize ((base-ellipsis (cons (get-ident-name el) (binding el))))
    (expand-any (second x))))
(define (ellipsis-pair? rule)
  (and
    (pair? (cdr rule))
    (ellipsis? (second rule))))

;; Returns an alist of (var . depth), when var matches pred?.
;; Depth is the number of ellipsis depth.
(define (collect-pattern-vars rule pred?)
  (let collect-pattern-vars ((rule rule) (depth 0))
    (cond
     ((ellipsis? rule) '())
     ((underscore? rule) '())
     ((pair? rule)
        (if (ellipsis-pair? rule)
          (append
            (collect-pattern-vars (car rule) (+ depth 1))
            (collect-pattern-vars (cddr rule) depth))
          (append
            (collect-pattern-vars (car rule) depth)
            (collect-pattern-vars (cdr rule) depth))))
      ((vector? rule) (collect-pattern-vars (vector->list rule) depth))
      ((and (identifier? rule) (pred? rule)) (list (cons rule depth)))
      (else '()))))

;; The main identifier record.
;; Each expansion adds a new 'color'.

;; As an optimization, we keep around the definition environment to speed lookup.
(define-record-type ident (make-ident-raw name colors env orig-env) ident-raw?
  (name ident-name)
  (colors ident-colors)
  (env ident-env)
  (orig-env ident-orig-env))

;; Pattern vars as a type.
(define-record-type pattern-var (make-pattern-var var depth) pattern-var?
  (var pattern-var-var)
  (depth pattern-var-depth))

;; TODO: parameterize?
(define *color* 0)

;; Reflect the identifier given the current usage env.
(define (ident-reflect id)
  (define env-id (rib-id (*usage-env*)))
  (set-env-table! (*usage-env*))
  (unless (symbol? id)
    (for-each set-env-table! (ident-env id)))
  (if (symbol? id)
      `(make-ident ',id '() '(,env-id))
      `(make-ident ',(ident-name id) ',(ident-colors id) (cons ',env-id ',(map rib-id (ident-env id))))))
;; Make an ident from a previous ident, *or* a symbol.
;; Note the transformer-env optimization here.
(define (make-ident name colors env-ids)
  (define env (map get-env-table env-ids))
  (make-ident-raw name (cons *color* colors) env (*trans-env*)))

;; Ribs, that are only useful for internal-definitions.
;; They facilitate lookup for macros, because we don't resolve
;; the macro binding until later (because of internal definitions that
;; may not be complete....sigh)
(define-record-type rib (%make-rib hash num prev id) environment?
  (hash rib-hash)
  (num rib-num)
  (prev rib-prev)
  (id rib-id))

(define (get-env-table id)
  (unless (symbol? id) (error "get-env-table: Not a sym:" id))
  (unless (hash-table-exists? (libman-envs (*libman*)) id) (error "Bad env id:" id))
  (hash-table-ref (libman-envs (*libman*)) id))
(define (set-env-table! rib)
  (hash-table-set! (libman-envs (*libman*)) (rib-id rib) rib))
(define (make-rib hash num prev)
  (%make-rib hash num prev (gen-sym 'env)))
(define (identifier? a) (or (symbol? a) (ident-raw? a)))
;; TODO parameterize
(define (next-color) (set! *color* (+ *color* 1)))
(define *rib-num* 0)
(define *trans-env* (make-parameter #f))
(define *usage-env* (make-parameter #f))
(define (make-env prev)
  (set! *rib-num* (+ 1 *rib-num*))
  (make-rib (make-hash-table equal?) *rib-num* prev))

;; A hashable, but not serializable, key.
(define (make-key id)
  ;;(assert (identifier? id))
  (if (symbol? id)
    id
    (cons (ident-name id) (ident-colors id))))

;; A variable binding - which may be a pattern variable.
(define-record-type var-binding (make-binding type name dim mutable) binding?
		    (type binding-type?)
		    (name binding-name)
		    (dim binding-dim)
		    (mutable binding-mutable?))

(define (add-env id val)
  ;;(dlog "Adding to env:~a\n" key)
  (hash-table-set! (rib-hash (*usage-env*)) (make-key id) val))

;; Get the original name of the identifier.
(define (get-ident-name a)
  ;;(dlog "Get-ident-name ~a \n" a)
  (if (symbol? a) a (ident-name a)))

;; Check only key equality.
(define (bound-identifier=? x y)
  ;; (assert (identifier? x))
  ;; (assert (identifier? y))
  (equal? (make-key x) (make-key y)))

;; Either they are both bound and are equal, or both
;; unbound, and the original names match.
(define (free-identifier=? x y)
  ;; (assert (identifier? x))
  ;; (assert (identifier? y))
  ;;(dlog "Free-identifier=? \n~a\n\n~a\n" x y)
  (and
   (eq? (get-ident-name x) (get-ident-name y))
    (let ((bx (binding x)) (by (binding y)))
      ;;(dlog "bind ~a ~a\n" bx by)
      (or
        (and bx (eq? bx by))
        (and
          (not bx)
          (not by)
          (eq? (get-ident-name x) (get-ident-name y)))))))

;; Used for stripping identifiers out of quoted data.
(define (syntax->datum n)
  (cond
   ((identifier? n) (get-ident-name n))
   ((pair? n) (imap syntax->datum n))
   ((vector? n) (list->vector (imap syntax->datum (vector->list n))))
   (else n)))

;; Useful only for syntax-case, or toplevel.
(define (datum->syntax tid d)
  ;; (assert (identifier? tid))
  ;; (assert (not (ident-raw? d)))
  (cond
    ((symbol? d)
      (if (symbol? tid)
        d
        (make-ident-raw d (ident-colors tid) (ident-env tid) (*trans-env*))))
    ((pair? d) (cons (datum->syntax tid (car d)) (datum->syntax tid (cdr d))))
    ((vector? d) (list->vector (datum->syntax tid (vector->list d))))
    (else d)))

(define (search-rib key rib next)
  (and
    rib
    (let ((res (hash-table-ref/default (rib-hash rib) key #f)))
      (or
        res
        (and
         ;(not (eq? rib next))
          (search-rib key (rib-prev rib) next))))))

;; Main binding resolution search
(define (binding id)
  ;;(dlog "Resolve ident: ~a ~a\n" (get-ident-name id) (if (symbol? id) '() (ident-colors id)))
  ;; (assert (identifier? id))
  ;; Resolve an identifier.
  (let resolve ((env (*usage-env*))
               (colors (if (symbol? id) '() (ident-colors id)))
               (envs (if (symbol? id) '() (ident-env id))))
    ;;(dlog "Resolve ident ~a in rib ~a\n" (get-ident-name id) env)
    (cond
      ;; It's a symbol.
      ((null? colors) (search-rib (get-ident-name id) env #f))
      ;; It's an identifier.
      ((search-rib (cons (ident-name id) colors) env (ident-orig-env id)))
      ;; Not found in this env (macro expansion), try next color / env.
      (else (resolve (car envs) (cdr colors) (cdr envs))))))

;; Macros.
(define-record-type dot-pat (make-dot-pat var) dot-pat? (var dot-pat-var))
(define-record-type macro (make-macro-raw raw rules bindings lits name matcher) macro?
  (raw macro-raw)
  (rules macro-rules set-macro-rules!)
  (bindings macro-bindings set-macro-bindings!)
  (lits macro-lits set-macro-lits!)
  (name macro-name set-macro-name!)
  (matcher macro-matcher set-macro-matcher!))
(define (make-macro name rules) (make-macro-raw rules '() '() '() name #f))
;; Replace ellipsis in pattern and template, looking up
;; the ellipsis in the macro environment.
;;
;; We do this so we don't have to carry the macro environment
;; around.

;; Free-identifier=?, where one is already resolved.
(define (free-identifier=?2 x resolved)
  (let ((bx (binding x)) (by (cdr resolved)))
    (or
      (and bx (eq? bx by))
      (and
        (not bx)
        (not by)
        (eq? (get-ident-name x) (car resolved))))))

(define (memp proc ls)
  (cond
   ((null? ls) #f)
   ((not (pair? ls)) (error "Invalid argument to memp:" ls))
   ((proc (car ls)) (car ls))
   (else (memp proc (cdr ls)))))


;; SYNTAX-CASE MATCHER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (gen-matcher sexp macro rules)
  ;; We can only emit base forms, and let/named-let are not base forms.
  ;; Destructure to lambdas and calls.
  (define (build-let var val body)
    `((lambda (,var) ,body) ,val))
  (define (build-lets vars vals body)
    `((lambda ,vars ,body) ,@vals))
  (define (build-and a b)
    `(if ,a ,b #f))
  (define (build-named-let vars inits lam)
    (define loop-name (gen-sym 'loop))
    `((letrec*
	  ((,loop-name (lambda ,vars ,(lam loop-name))))
	,loop-name)
      ,@inits))
  
  (define (gen-match sexp rule sk fk)
    (cond
      ((not (symbol? sexp))
        (let ((sym (gen-sym 'sexp)))
          (build-let sym sexp (gen-match sym rule sk fk))))
      ((and (underscore? rule)
	    (not (memp (lambda (x) (bound-identifier=? rule x))
		       (macro-lits macro)))) sk)
      ;; Check if it is a literal using free-identifier=? or it's just a ident.
      ((identifier? rule)
        (let ((lit
                (memp
                  (lambda (x) (bound-identifier=? rule x))
                  (macro-lits macro))))
          (if lit
            `(if ,(build-and
                   `(identifier? ,sexp)
                   `(free-identifier=? ,sexp ,(ident-reflect lit)))
              ,sk
              ,fk)
            (let ((sym (pattern-var-var (binding rule))))
              (add-env rule sym)
              (build-let sym sexp sk)))))
      ((vector? rule)
        `(if (vector? ,sexp)
          ,(gen-match `(vector->list ,sexp) (vector->list rule) sk fk)
          ,fk))
      ((not (pair? rule)) `(if (equal? ',rule ,sexp) ,sk ,fk))
      ((and (ellipsis-pair? rule)
	    (not (memp (lambda (x) (bound-identifier=? (second rule) x))
		       (macro-lits macro))))
        (let* ((tail-len (- (ilength rule) 2))
               (loop (gen-sym 'loop))
               (len (gen-sym 'len))
               (vars
                 (map
                   car
                   (collect-pattern-vars
                     (first rule)
                     (lambda (x)
                       (not
                         (memp
                           (lambda (lit) (bound-identifier=? x lit))
                           (macro-lits macro)))))))
               (var-names
                 (map (lambda (x) (pattern-var-var (binding x))) vars))
               (tmp-vars (map gen-sym (map get-ident-name vars))))
          `(if ,(build-and `(not (pair? ,sexp)) `(not (null? ,sexp)))
               ,fk
	       ,(build-let
		 'dot-len `(- (ilength ,sexp) ,tail-len)
		 `(if (negative? dot-len)
		      ,fk
		      ,(build-named-let
			`(,len ,sexp ,@tmp-vars)
			`(dot-len ,sexp ,@(map (lambda (x) ''()) tmp-vars))
			(lambda (loop)
			  `(if (= ,len 0)
				       ,(build-lets
					 var-names
					 (omap tmp tmp-vars
					       `(reverse ,tmp))
					 (gen-match sexp (cddr rule) sk fk))
				       ,(gen-match
					 `(car ,sexp)
					 (car rule)
					 `(,loop
					   (- ,len 1)
					   (cdr ,sexp)
					   ,@(map
					      (lambda (v tmp) `(cons ,v ,tmp))
					      var-names
					      tmp-vars))
					 fk)))))))))
      (else
        `(if (not (pair? ,sexp))
          ,fk
          ,(gen-match
            `(car ,sexp)
            (car rule)
            (gen-match `(cdr ,sexp) (cdr rule) sk fk)
            fk)))))
  
  (if (null? rules)
    `(error "No match for " ,sexp)
    (let* ((rule (car rules))
	   (has-fender (= (length rule) 3))
           ;; Per  r7rs, the car of pattern is ignored.
           (pattern (first rule))
	   (fender (if has-fender (second rule) #t))
           (template (if has-fender (third rule) (second rule)))
           (vars
             (collect-pattern-vars
               pattern
               (lambda (x)
                 (not
                   (memp
                     (lambda (lit) (bound-identifier=? lit x))
                     (macro-lits macro))))))
           (failure-match (gen-matcher sexp macro (cdr rules))))
      (parameterize
        ((*usage-env* (make-env (*usage-env*))))
        (for var vars
          (add-env
            (car var)
            (make-pattern-var (gen-sym (get-ident-name (car var))) (cdr var))))
	(build-let 'failure `(lambda () ,failure-match)
		   (gen-match sexp pattern `(if ,(expand-expr fender) ,(expand-expr template) (failure)) '(failure)))))))

(define (compile-macro-bindings macro)
  ;; TODO ellipsis from scheme base
  (define m-sexp (macro-raw macro))
  
  ;; (assert (>= (length m-sexp) 2))
  ;;(dlog "COMPILE MACRO BINDINGS ~a ~a\n" (get-ident-name (macro-name macro)) m-sexp)
  (let* ((have-ellipsis (not (list? (second m-sexp))))
         (ellipsis
           (if have-ellipsis
             (cons (get-ident-name (second m-sexp)) (binding (second m-sexp)))
             (base-ellipsis)))
         (rest ((if have-ellipsis cddr cdr) m-sexp))
         (lits (first rest))
	 (ellipsis-in-lits (any (lambda (x) (free-identifier=?2 x ellipsis)) lits))
	 (new-ellipsis (if ellipsis-in-lits `(,(gen-sym 'ellipsis) . #f) ellipsis))
         (rules (cdr rest))
         (literals lits))
    (set-macro-lits! macro literals)
    ;;(dlog "Macro lits::::::::::::::: ~a\n" literals)
    (set-macro-rules! macro rules)
    (parameterize ((base-ellipsis new-ellipsis))
      (let* ((sexp (gen-sym 'sexp))
             (matcher `(lambda (,sexp) ,(gen-matcher sexp macro rules))))
	;; (dlog "Macro ~a\n" (macro-name macro))
	;; (display "RULES:\n")
	;; (pretty-print rules)
	;; (display "MACRO:\n")
	;; (pretty-print matcher)
	(set-macro-matcher! macro matcher)))))

;; Also TODO, symbols should keep namespace separate somehow?
(define (emit-global b)
  (string->symbol (string-append (cur-namespace) (symbol->string b))))

(define (expand-lookup sexp)
  ;;(dlog "Expand lookup ~a\n" sexp)
  ;; (assert (identifier? sexp))
  (or
    ;; It could be in the environment
    (binding sexp)
    ;; Or, an undefined global.  Make a name for it
    (emit-global (get-ident-name sexp))))

(define-record-type transformer (make-transformer proc code) transformer?
		    (proc transformer-proc)
		    (code transformer-code))
(define (run-macro proc sexp)
  (next-color)
  ;;(dlog "Run macro:~a\n" sexp)
    (let ((res (parameterize ((*trans-env* (*usage-env*))) (proc sexp))))
      ;;(dlog "Done run macro:~a ~a\n" res (syntax->datum res))
    res
    ))

(define (expand-head sexp)
  ;; TODO van tonder returns expand-lookup result here, and every
  ;; expander handles non-lists.
  (if (not (and (pair? sexp) (identifier? (car sexp))))
      (values sexp #f)
      (let ((lookup (expand-lookup (car sexp))))
        (if (transformer? lookup)
            (expand-head (run-macro (transformer-proc lookup) sexp))
            (values sexp lookup)))))

(define (expand-any sexp)
  ;;(dlog "Expand any: ~a\n" (syntax->datum sexp))
  (cond
   ((identifier? sexp) (expand-lookup sexp))
   ((not (pair? sexp)) sexp)
   ((identifier? (car sexp))
    (let ((rhead (expand-lookup (car sexp))))
      (cond
       ((procedure? rhead)
	(rhead (cdr sexp)))
       ((transformer? rhead)
	(expand-any (run-macro (transformer-proc rhead) sexp)))
       (else
	(imap expand-expr sexp)))))
   (else (imap expand-expr sexp))))

(define (expand-expr sexp)
  (define expanded (expand-any sexp))
  (when (or
	 (transformer? expanded)
	 (procedure? expanded)
	 (pattern-var? expanded))
    (error "Syntax in bad context:" sexp " expanded to: " expanded))
  ;; Vectors are self-quoting.
  (if (vector? expanded)
      (syntax->datum expanded)
      expanded))

(define *invoke* (make-parameter #f))
(define (expand-import decl)
  (define (exp-lib x)
    (define res (expand-library x))
    ;; Visit it.  Not useful except for eval?
    ;; We can always visit eagerly in r7rs, there are no phases.
    ;(for-each (lambda (x) (expand-eval x)) res)
    ;; Add it to invoke code.
    (for-each (lambda (x) (set-car! (*invoke*) (cons x (car (*invoke*))))) res)
    res)
  (define (parse-import-set spec)
    ;; (assert (pair? spec))
    (case (car spec)
      ((only)
        (ofilter i (parse-import-set (second spec)) (memv (car i) (cddr spec))))
      ((except)
        (ofilter
          i
          (parse-import-set (second spec))
          (not (memv (car i) (cddr spec)))))
      ((prefix)
        (omap
          i
          (parse-import-set (second spec))
          (cons
            (string->symbol
              (string-append
                (symbol->string (third spec))
                (symbol->string (car i))))
            (cdr i))))
      ((rename)
        (omap
          i
          (parse-import-set (second spec))
          (let ((re (assq (car i) (cddr spec))))
            (if re
              (cons (second re) (cdr i))
              i))))
      ;; It's a library name.
      (else
       (library-exports (do-load-library (*libman*) spec exp-lib)))))
  
  (define (install-spec spec) (for x spec (add-env (car x) (cdr x))))
  ;; (assert (pair? decl))
  ;;(dlog "Expanding import ~a\n" decl)
  (for-each (lambda (x) (install-spec (parse-import-set x))) (cdr decl)))

(define (expand-library sexp)
  (define name (second sexp))
  (define code '())
  (define imports '())
  (define exports '())
  (define (parse-library-decl decl)
    ;; (assert (pair? decl))
    (case (car decl)
      ((export) (set! exports (append exports (cdr decl))))
      ((import)
        (set! imports (append imports (cdr decl)))
        (expand-import decl))
      ((begin) (set! code (append code (cdr decl))))
      ((include include-ci)
        (for file (cdr decl)
          (with-input-from-file
              (add-current-path (*libman*) file)
            (lambda () (set! code (append code (read-file)))))))
      ((include-library-declarations)
        (for file (cdr decl)
          (with-input-from-file
              (add-current-path (*libman*) file)
            (lambda () (for-each parse-library-decl (read-file))))))
      ((cond-expand)
       (for-each parse-library-decl (parse-cond-expand (cdr decl))))
      (else (error "Invalid library decl:" decl))))
  
  (define (resolve-export decl)
    (if (pair? decl)
      (begin
        ;; (assert (eq? (first decl) 'rename))
        (cons (third decl) (binding (second decl))))
      (cons decl (binding decl))))
  
  (parameterize
    ((*usage-env* (make-env #f))
      (cur-namespace (string-append "LIBRARY-" (lib-name->namespace name) "-")))
    (for-each parse-library-decl (cddr sexp))
    (let* ((code (expand-toplevel code))
           (resolved-exports (map resolve-export exports))
           (lib (make-library name resolved-exports imports)))
      (install-library! lib (*libman*))
      code)))

(define *libman* (make-parameter runtime-man))
(define expand-program
  (case-lambda
    ((sexps namespace) (expand-program sexps namespace runtime-man))
    ((sexps namespace man) (expand-program sexps namespace man (make-env #f)))
    ((sexps namespace man env)
     (parameterize
	 ((cur-namespace namespace)
	  (*usage-env* env)
	  (*libman* man)
	  (*invoke* (cons '() #f)))
       (do ((sexps sexps (cdr sexps)))
	   ((not
             (and
              (pair? sexps)
              (pair? (first sexps))
              (eq? 'import (car (first sexps)))))
            (append (reverse (car (*invoke*))) (expand-toplevel sexps)))
	 (expand-import (car sexps)))))))

(define (expand-toplevel sexps)
  (let loop ((res '()) (sexps sexps))
    (if (not (pair? sexps))
      (reverse res)
      (let-values
        (((sexp type) (expand-head (car sexps))))
        ;;(dlog "Expanding ~a head: ~a type ~a eq ~a\n" (car sexps) sexp type (equal? type expand-define-syntax))
        (cond
          ((eq? type expand-include)
            (loop res (append (parse-include sexp) (cdr sexps))))
          ((eq? type expand-begin) (loop res (append (cdr sexp) (cdr sexps))))
          ((eq? type expand-define-syntax)
            (parse-define-syntax (cdr sexp))
            (loop res (cdr sexps)))
          ((eq? type expand-def)
            (unless
              (identifier? (second sexp))
              (error "Trying to define a non-ident:" (second sexp)))
            (let ((name (emit-global (get-ident-name (second sexp)))))
              (add-env (second sexp) name)
              (loop
                (cons `(define ,name ,(expand-expr (third sexp))) res)
                (cdr sexps))))
          (else (loop (cons (expand-expr sexp) res) (cdr sexps))))))))

(define (expand-body sexps)
  (define (maybe-emit-begin sexps)
    ;; (assert (pair? sexps))
    (if (null? (cdr sexps))
      (car sexps)
      `(begin ,@sexps)))
  
  (define (maybe-new-env t)
    (if t
      (*usage-env*)
      (make-env (*usage-env*))))
  
  ;;(dlog "Expand body ~a\n" sexps)
  (let loop ((have-defs #f) (defs '()) (sexps sexps))
    (if (not (pair? sexps))
      (error "Invalid body, contains no expressions")
      (let-values
        (((sexp type) (expand-head (car sexps))))
        (cond
          ((eq? type expand-include)
            (loop have-defs defs (append (parse-include sexp) (cdr sexps))))
          ((eq? type expand-begin)
            (loop have-defs defs (append (cdr sexp) (cdr sexps))))
          ((eq? type expand-define-syntax)
            (parameterize
              ((*usage-env* (maybe-new-env have-defs)))
              (parse-define-syntax (cdr sexp))
              (loop #t defs (cdr sexps))))
          ((eq? type expand-def)
            (unless
              (identifier? (second sexp))
              (error "Trying to define a non-ident:" (second sexp)))
            (parameterize
              ((*usage-env* (maybe-new-env have-defs)))
              (let* ((name (gen-sym (get-ident-name (second sexp))))
                     (expanded `(,name ,(third sexp))))
                (add-env (second sexp) name)
                (loop #t (cons expanded defs) (cdr sexps)))))
          (else
            (let ((rest
                    (maybe-emit-begin
                      (map expand-expr (cons sexp (cdr sexps))))))
              (if (not (pair? defs))
                rest
                `(letrec*
                  ,(map
                    (lambda (x) (list (first x) (expand-expr (second x))))
                    (reverse defs))
                  ,rest)))))))))

;;;;;;;;;;;;; The built-in expanders.
(define (expand-lambda-case sexp)
  ;; env-vars is alpha-renamed here.
  (define args
    (if (ilist? (first sexp))
      (first sexp)
      (list (first sexp))))
  (define proper-args (to-proper args))
  (unless (every identifier? proper-args) (error "Bad lambda args:" proper-args))
  (let ()
    (define new-vars (imap gen-sym (imap get-ident-name args)))
    (define env-vars (to-proper new-vars))
    (parameterize
	((*usage-env* (make-env (*usage-env*))))
      (for (x y) (proper-args env-vars)
	   (add-env x y))
      (let ((res
             `(,(if (ilist? (first sexp))
			 new-vars
			 (car env-vars))
		,(expand-body (cdr sexp)))))
	res))))

(define (expand-lambda sexp)
  `(lambda ,@(expand-lambda-case sexp)))

(define (expand-case-lambda sexp)
  (match sexp
    ((,cases ___)
     `(case-lambda ,@(omap case cases (expand-lambda-case case))))
    (,else (error "bad case lambda" sexp))))

(define (expand-begin sexp)
  (define res (map expand-expr sexp))
  (if (null? (cdr res))
    (first res)
    `(begin ,@res)))

(define (expand-set! sexp)
  (define expr (expand-expr (first sexp)))
  (define val (expand-expr (second sexp)))
  `(set! ,expr ,val))

(define (expand-if sexp)
  (case (length sexp)
    ((2) `(if ,@(map expand-expr sexp) #f))
    ((3) `(if ,@(map expand-expr sexp)))
    (else (error "Invalid if: " sexp))))

(define (expand-syntax-case sexp)
  (define macro (make-macro "NONE" (cons 'syntax-case (cdr sexp))))
  (compile-macro-bindings macro)
  `(,(macro-matcher macro) ,(expand-any (car sexp))))

(define (expand-define-syntax sexp)
  (error "define-syntax used in bad context"))

(define (parse-define-syntax sexp)
  (define code (expand-body (cdr sexp)))
  ;(define unused (begin (display "Genned:") (pretty-print code) (newline)))
  (define evaled (expand-eval code))
  (define name (first sexp))
  (define transformer (make-transformer evaled code))
  (add-env name transformer)
  (list name transformer))

(define (expand-let-syntax sexp)
  (define bind (first sexp))
  (define new-vals
    (omap
      v
      bind
      (define code (expand-any (second v)))
      ;(define unused (begin (display "Genned:") (pretty-print code) (newline)))
      (define evaled (expand-eval code))
      (make-transformer evaled code)))
  ;;(dlog "Let-syntax ~a\n" bind)
  (parameterize
    ((*usage-env*
        (if (null? new-vals)
          (*usage-env*)
          (make-env (*usage-env*)))))
    ;;(assert (= 2 (length sexp)))
    (for (x y) ((map first bind) new-vals)
      (add-env x y))
    (expand-body (cdr sexp))))

;; SYNTAX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the second half of syntax-case or syntax-rules: expanding a pattern.
(define (expand-syntax sexp)
  ;; Split the segment, resolving multiple dots in a smart way, i.e.:
  ;; x ... ... tail
  (define (split-segment head)
    (define (do-split head)
     (if (and (pair? head) 
	      (pair? (cdr head))
	      (ellipsis? (cadr head))
	      (pair? (cddr head))
	      (ellipsis? (caddr head)))
	 (let-values (((h tail depth) (do-split (cdr head))))
	   (values (cons (car head) h) tail (+ depth 1)))
	 (values (list (car head)) (cddr head) 0)))
    (let-values (((h t d) (do-split head)))
      (if (= d 0) (values (car h) t d) (values h t d))))

  (define (find-reference var var-level maps)
    (if (= 0 var-level)
	(values var maps)
	(if (null? maps)
	 (error "Missing ellipsis for pattern variable:" var)
	 (let-values (((outer-var outer-maps) (find-reference var (- var-level 1) (cdr maps))))
	   (cond
	    ((assq outer-var (car maps)) => (lambda (x) (values (cdr x) maps)))
	    (else
	     (let ((inner-var (gen-sym 'pat-var)))
	       (values inner-var (cons (alist-cons outer-var inner-var (car maps)) outer-maps)))))))))

  ;; We need to do a bottom-up search for pattern variables and destructure them using 'maps'.
  ;; Maps is returned everywhere, so we can re-use the pattern variables instead of
  ;; destructuring them multiple times.
  (define (expand-syntax rule can-ellipsis maps)
    (cond
      ((identifier? rule)
       (let ((lookup (binding rule)))
	 ;(dlog "Lookup ~a val ~a\n" rule lookup)
          (if (and
               lookup
               (pattern-var? lookup))
	      (find-reference (pattern-var-var lookup) (pattern-var-depth lookup) maps)
              (values (ident-reflect rule) maps))))
      ((vector? rule)
       (let-values (((res maps) (expand-syntax (vector->list rule) can-ellipsis maps)))
	 (values `(list->vector ,res) maps)))
      ((not (pair? rule)) (values `',rule maps))
      ((and can-ellipsis (ellipsis? (car rule))) (expand-syntax (second rule) #f maps))
      ((and can-ellipsis (ellipsis-pair? rule))
       (let-values (((head tail depth) (split-segment rule)))
	 (let-values (((head-match head-maps) (expand-syntax head can-ellipsis (cons '() maps))))
	   (when (null? (car head-maps))
	     (pretty-print head-match)
	     (pretty-print head-maps)
	     (error "Too many ellipsis for pattern vars:" head))
	   (let* ((new-map (car head-maps))
		  (var-names (map cdr new-map))
		  (var-values (map car new-map))
		  (gen `(map (lambda ,var-names ,head-match) ,@var-values)))
	     (let-values (((tail-match tail-maps) (expand-syntax tail can-ellipsis (cdr head-maps))))
               (values `(append ,(if (= depth 0)
				     gen
				     ;; Multiple ellipsis patterns are spliced in:
				     `(apply append ,gen))
				,tail-match)
		       tail-maps))))))
      (else
       (let-values (((car-res car-maps) (expand-syntax (car rule) can-ellipsis maps)))
	 (let-values (((cdr-res cdr-maps) (expand-syntax (cdr rule) can-ellipsis car-maps)))
	   (values `(cons ,car-res ,cdr-res) cdr-maps))))))
  (let-values (((res maps) (expand-syntax (first sexp) #t '())))
    ;; (assert (null? maps))
    res))

;; OTHER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expand-quote sexp)
  `',(syntax->datum (first sexp)))

(define (expand-def sexp) (error "Definition used in bad context" sexp))
;; TODO ideally this should be an internal macro instead - then
;; no need to treat specially in body expansion.
(define (expand-include sexp)
  (define parsed (parse-include (cons 'include sexp)))
  (define res `(begin ,@(map expand-expr parsed)))
  res)
;; Hack to save paths until we have wraps.
(define include-paths (make-hash-table eq?))
(define (parse-include sexp)
  (define (set-path x path)
    (match x
      ((include ,name)
       (hash-table-set! include-paths name path))
      ((,foo . ,bar)
       (set-path foo path)
       (set-path bar path))
      (,else 0)))
  (define lookup (hash-table-ref/default include-paths (second sexp) ""))
  (define file-path (add-current-path (*libman*) (string-append lookup (second sexp))))
  (define file-dir (list->string (reverse (or (memq #\/ (reverse (string->list file-path))) '()))))
  (define input (with-input-from-file file-path (lambda () (read-file))))
  (set-path input file-dir)
  input)

(define (features) '(callcc))

;; TODO should this match identifiers instead of symbols??
;; Would be easier to write in syntax-case?
(define (expand-cond-expand sexp)
  (define exprs (parse-cond-expand sexp))
  `(begin ,@(map expand-any exprs)))
(define (parse-cond-expand sexp)
  (define (parse-feature f)
    (match f
      ((library ,name) (error "no cond-expand lib feature"))
      ((and ,features ___) (every (map parse-feature features)))
      ((or ,features ___) (any (lambda (x) x) (map parse-feature features)))
      ((not ,feature) (not (parse-feature feature)))
      (else #t)
      (,feature (memq feature (features)))))
  (let loop ((clauses sexp))
    (when (null? clauses)
      (error "No matching cond-expand:" sexp))
    (let* ((clause (car clauses))
	   (feature (first clause))
	   (rest (cdr clauses)))
      (if (parse-feature feature)
	  (cdr clause)
	  (loop rest)))))

;;;;;;;;;;;;; Load the standard library

(define expand-builtins
    `((lambda  .  ,expand-lambda)
      (case-lambda  .  ,expand-case-lambda)
      (begin  .  ,expand-begin)
      (if . ,expand-if)
      (quote  .  ,expand-quote)
      (define-syntax  .  ,expand-define-syntax)
      (let-syntax  .  ,expand-let-syntax)
      (syntax  .  ,expand-syntax)
      (syntax-case  .  ,expand-syntax-case)
      (with-ellipsis . ,expand-with-ellipsis)
      (def  .  ,expand-def)
      (set! . ,expand-set!)
      ;; TODO include-ci
      (include  .  ,expand-include)
      (cond-expand . ,expand-cond-expand)))

(define (install-builtins man)
  (install-library! (make-library '(flow builtins) expand-builtins '()) man))

;; TODO: cleanup stdlib. Not used if no eval, except the handful of builtins.
(include "stdlib.scm")
(define (expander-init man)
  (install-builtins man)
  (install-primitives man)
  (install-sys man))



;;;;;;;;;;;;; Serializer
;; This is used to generate macro & library exports that can be used
;; in statically built libraries for e.g. eval.
;;
;; For example the base runtime serializes all the macros for
;; scheme base (cond, case, etc), but during REPL-eval activities
;; we don't need to serialize anything more
;;;;;;;;;;;;;

(define (serialize-libraries man)
  (define libs '())
  (define libs-done (make-hash-table eq?))
  (define envs-done (make-hash-table eq?))
  (define (serialize-env rib)
    (unless (hash-table-exists? envs-done (rib-id rib))
      (hash-table-set! envs-done (rib-id rib) #t)
      (when (rib-prev rib) (serialize-env (rib-prev rib)))
      (push! libs
	     `(install-rib! ',(rib-id rib)
			    ',(if (rib-prev rib) (rib-id (rib-prev rib)) #f)
			    ',(omap mapping (hash-table->alist (rib-hash rib))
				   (serialize-export mapping))))))
  (define macros (make-hash-table eq?))
  (define (serialize-transformer sym trans)
    (cond
     ((hash-table-ref/default macros trans #f))
     (else
      (let ((name (gen-sym 'transformer)))
	(hash-table-set! macros trans name)
	(push! libs
	       `(install-transformer! ',name ,(transformer-code trans)))
	name))))
  (define (serialize-export exp)
    (match exp
      ((,name . ,proc)
       (guard (procedure? proc))
       `(,name . builtin))
      ((,name . ,trans)
       (guard (transformer? trans))
       `(,name . (transformer ,(serialize-transformer name trans))))
      ((,name lookup ,var)
       `(,name . ,var))
      (,else
       else)))
  (define (serialize-lib lib)
    (cond
     ((hash-table-ref/default libs-done (car lib) #f))
     (else
      (hash-table-set! libs-done (car lib) #t)
      (unless (member (car lib) '((r7 builtins) (r7 primitives) (r7 sys)))
	(for lib (library-imports (cdr lib))
	     (let ((imp (assoc lib (get-installed-libraries man))))
	       (when imp
		 (serialize-lib imp))))
	(let* ((lib (cdr lib))
	       (exports (map serialize-export (library-exports lib))))
	  (push! libs
		 `(install-library!
		   (make-library ',(library-name lib)
				 ',exports
				 '()))))))))
  (for-each serialize-env (hash-table-values (libman-envs man)))
  (for-each serialize-lib (get-installed-libraries man))
  (reverse libs))

;; Deserialization
(define serialized-transformers (make-hash-table eq?))
(define (deserialize-export exp)
  (match exp
    ((,name . builtin) (cons name (cdr (assq name expand-builtins))))
    ((,name . (transformer ,var))
     (cons name (hash-table-ref serialized-transformers var)))
    ((,name . ,var) exp)
    (,else (error "Invalid deserialize-export:" exp))))
(define (install-transformer! name code)
  (hash-table-set! serialized-transformers name (make-transformer code `(error "can't re-serialize" ',name))))
(define (install-rib! id prev mappings)
  (set-env-table! (%make-rib (alist->hash-table (map deserialize-export mappings)) 0 (if prev (get-env-table prev) #f) id)))
(define (expand-make-library name exports imports)
  (make-library name (map deserialize-export exports) imports))
