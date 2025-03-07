
(import (scheme base)
	(prefix (flow sys) sys:)
	(scheme case-lambda)
	(rename (expand)
		(make-ident expand-make-ident)
		(identifier? expand-identifier?)
		(free-identifier=? expand-free-identifier=?)
		(install-transformer! expand-install-transformer!)
		(install-rib! expand-install-rib!))
	(rename (library-manager)
		(install-library! manager-install-library!))
	(rename (util) (ilength util-ilength))
	(match))

;; Eval: these are generated from the expander without a namespace: export them here sans-namespace:
;; TODO: fix up symbol expansion so this isn't necessary.
(define ilength util-ilength)
(define make-ident expand-make-ident)
(define identifier? expand-identifier?)
(define free-identifier=? expand-free-identifier=?)
(define install-transformer! expand-install-transformer!)
(define install-rib! expand-install-rib!)
(define install-library! manager-install-library!)
(define make-library expand-make-library)

(expander-init runtime-man)

(define (environment . lst)
  (let ((env (make-env #f)))
    (eval `(import ,@lst) env)
    env))

(define interaction-environment
  (let ((env #f))
    (lambda ()
      (unless env
	;; Build an environment with all the scheme report imported.
	(set! env (make-env #f))
	(eval '(import (scheme base)
		       (scheme repl)
		       (scheme write)
		       (scheme read)
		       (scheme file)
		       (scheme case-lambda)
		       (scheme char)
		       (scheme complex)
		       (scheme cxr)
		       (scheme eval)
		       (scheme inexact)
		       (scheme lazy)
		       (scheme load)
		       (scheme process-context)
		       (scheme r5rs)
		       (scheme time))  env))
      env)))
(define (get-compile-path)
  (let ((slash (memq #\/ (reverse (string->list (car (command-line)))))))
    (if slash (list->string (reverse slash))
	"./")))
(define path (get-compile-path))
(set! library-search-paths (cons (string-append path "lib/srfi2") library-search-paths))
(set! library-search-paths (cons (string-append path "lib/headers") library-search-paths))
(set! library-search-paths (cons (string-append path "lib") library-search-paths))

;; A simple ast-walking interpreter.  It is very slow.
;; It directly parses what comes out of the expander.

;; It could be improved by:
;; a) running it through some of the optimization passes (closure conversion, etc)
;; b) Pre-compile, compiling-with-closures.

;; This 'env' is only the lexical environment, the global environment
;; is handled by the expander.
(define (base-eval e env)
  (match e
    ((lambda ,largs ,lbody)
     (lambda args
       (if (pair? largs)
           (let loop ((env env) (args args) (params largs))
	     (if (pair? params)
                 (loop
                  (cons (cons (car params) (car args)) env)
                  (cdr args)
                  (cdr params))
                 (base-eval
		  lbody
                  (if (null? params)
		      env
		      (cons (cons params args) env)))))
           (let ((env (cons (cons largs args) env)))
	     (base-eval lbody env)))))
    ((case-lambda (,cargs-all ,bodies) ___)
     (lambda args
       (let loop ((cargs-all cargs-all) (bodies bodies))
	 (when (null? bodies)
	   (error "Can't find case for call"))
	 (let ((cargs (car cargs-all))
	       (body (car bodies)))
	   (if (or (and (list? cargs) (eq? (length cargs) (length args)))
		   (and (not (list? cargs)) (>= (length args) (ilength cargs))))
	       (base-eval `((lambda ,cargs ,body) ,@args) env)
	       (loop (cdr cargs-all) (cdr bodies)))))))
    ((if ,test ,true ,false)
     (if (base-eval test env)
         (base-eval true env)
         (base-eval false env)))
    ((letrec* ((,vars ,inits) ___) ,body)
     (let* ((values (map (lambda (var) (cons var #f)) vars))
	    (new-env (append values env)))
       (let loop ((vars vars) (inits inits) (values values))
	 (if (pair? vars)
	     (begin (set-cdr! (car values) (base-eval (car inits) new-env))
		    (loop (cdr vars) (cdr inits) (cdr values)))))
       (base-eval body new-env)))
    ((begin ,exprs ___ ,expr)
     (for expr exprs
	  (base-eval expr env))
     (base-eval expr env))
    ((quote ,a)
     a)
    ((define ,sym ,val)
     (sys:FOREIGN_CALL "SCM_SET_GLOBAL" sym (base-eval val env))
     val)
    ((set! ,sym ,val)
     (let ((val (base-eval val env)))
       (cond
	((assq sym env) => (lambda (x) (set-cdr! x val)))
	(else (sys:FOREIGN_CALL "SCM_SET_GLOBAL" sym val)))))
    ((,args ___)
     (let ((args (map (lambda (arg) (base-eval arg env)) args)))
       (apply (car args) (cdr args))))
    (,sym
     (guard (symbol? sym))
     (cond
      ((assq e env) => cdr)
      (else (sys:FOREIGN_CALL "SCM_LOAD_GLOBAL" sym))))
    (,self-eval
     self-eval)))

(define (eval expr env)
  (unless (environment? env) (error "Eval: not an environment" env))
  (match expr
    ((no-expand . ,expr) (evals (list expr) env #f))
    (,expr (evals (list expr) env #t))))

;; Evaluate a list of expressions, possibly expanding them as well.
(define (evals prog expand-env expand)
  (let ((expanded (if expand (expand-program prog "PROG-" runtime-man expand-env)
		      prog)))
    (let loop ((exprs expanded) (res #f))
      (if (null? exprs)
	  res
	  (loop (cdr exprs) (base-eval (car exprs) '()))))))
      
(define (load file)
  (define input (with-input-from-file file (lambda () (read-file))))
  (evals input (interaction-environment) #t))

(define (scheme-report-environment version)
  (unless (= 5 version) (error "scheme-report-environment supports only version 5" version))
  (environment '(scheme r5rs)))

(define (null-environment version)
  (unless (= 5 version) (error "null-environment supports only version 5" version))
  (environment '(only (scheme r5rs) syntax-rules define quasiquote let let* letrec letrec-syntax
		      do case cond and or delay force
		      lambda begin if quote define-syntax let-syntax set!)))
