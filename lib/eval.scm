
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

;; Eval
(define ilength util-ilength)
(define make-ident expand-make-ident)
(define identifier? expand-identifier?)
(define free-identifier=? expand-free-identifier=?)
(define install-transformer! expand-install-transformer!)
(define install-rib! expand-install-rib!)
(define install-library! manager-install-library!)
(define make-library expand-make-library)

(expander-init runtime-man)
(define env (make-env #f))
(define (environment . lst) (interaction-environment))
(define (interaction-environment) 
  env)
(define (get-compile-path)
  (let ((slash (memq #\/ (reverse (string->list (car (command-line)))))))
    (if slash (list->string (reverse slash))
	"./")))
(define path (get-compile-path))
(set! library-search-paths (cons (string-append path "lib/srfi2") library-search-paths))
(set! library-search-paths (cons (string-append path "lib/headers") library-search-paths))
(set! library-search-paths (cons (string-append path "lib") library-search-paths))
 (expand-program '((import (scheme base)
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
			   (scheme time))) "PROG-" runtime-man env)

;; A simple ast-walking interpreter.  It is very slow.
;; It directly parses what comes out of the expander.

;; It could be improved by:
;; a) running it through some of the optimization passes (closure conversion, etc)
;; b) Pre-compile, compiling-with-closures. 
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
(define (eval prog env2)
  (define expand (not (and (pair? prog) (eq? 'no-expand (car prog)))))
  (define e (if expand (let ((v (expand-program (list prog) "PROG-" runtime-man env)))
			 v) (list (cdr prog))))
  (define res '())
  ;; (when expand
  ;;   (display "My eval: ") (display e) (newline))
  (for expr e
       (set! res (base-eval expr '())))
  res)
      
(define (load file)
  (define input (with-input-from-file file (lambda () (read-file))))
  (for e input
       (eval e (interaction-environment))))
