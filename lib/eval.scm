;; Two evaluators that can run the output of the expander.
#|

Used for syntax-case / syntax-rules expansion.  It's pretty bad.

It currently relies on constants not needing to be
serialized (identifiers + envs are records, which don't serialize in
scheme)

 |#


; (import (scheme base) (scheme r5rs) (srfi 1) (scheme process-context) (expand) (library-manager) (util))
(import (scheme base)
	(prefix (flow sys) sys:)
	(scheme case-lambda)
	(expand)
	(match))

(define (environment . lst) `((not . ,not)
			      (pair? . ,pair?)
			      (car . ,car)
			      (cdr . ,cdr)
			      (null? . ,null?)
			      (- . ,-)
			      (+ . ,+)
			      (= . ,=)
			      (reverse . ,reverse)
			      (equal? . ,equal?)
			      (cons . ,cons)
			      (ilength . ,ilength)
			      (negative? . ,negative?)
			      (make-ident . ,make-ident)
			      (append . ,append)
			      (map . ,map)
			      (identifier? . ,identifier?)
			      (free-identifier=? . ,free-identifier=?)
			      (vector? . ,vector?)
			      (vector->list . ,vector->list)
			      (error . ,error)
			      (apply . ,apply)
			      (list . ,list)))

(define (eval e env)
  ;(display "My eval: ") (display e) (newline)
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
                 (eval
		  lbody
                  (if (null? params)
		      env
		      (cons (cons params args) env)))))
           (let ((env (cons (cons largs args) env)))
	     (eval lbody env)))))
    ((if ,test ,true ,false)
     (if (eval test env)
         (eval true env)
         (eval false env)))
    ((letrec* ((,vars ,inits) ___) ,body)
     (let* ((values (map (lambda (var) (cons var #f)) vars))
	    (new-env (append values env)))
       (let loop ((vars vars) (inits inits) (values values))
	 (if (pair? vars)
	     (begin (set-cdr! (car values) (eval (car inits) new-env))
		    (loop (cdr vars) (cdr inits) (cdr values)))))
       (eval body new-env)))
    ((begin ,exprs ___ ,expr)
     (for expr exprs
	  (eval expr env))
     (evap expr env))
    ((quote ,a)
     a)
    ((,args ___)
     (let ((args (map (lambda (arg) (eval arg env)) args)))
       (apply (car args) (cdr args))))
    (,sym
     (guard (symbol? sym))
     (cond
      ((assq e env) => cdr)
      (else (error "Unbound symbol in eval: " sym))))
    (,self-eval
     self-eval)))
      
	;; ((let)
        ;;  (if (pair? (second e))
        ;;      (let* ((evaled
        ;;              (map (lambda (e) (my-eval (second e) env)) (second e)))
        ;;             (vars (map car (second e)))
        ;;             (new-env (append (map cons vars evaled) env)))
        ;;        (let loop ((res #f) (sexps (cddr e)))
	;; 	 (if (pair? sexps)
        ;;              (loop (my-eval (car sexps) new-env) (cdr sexps))
        ;;              res)))
        ;;      (my-eval
        ;;       `(let ((,(second e) #f))
	;; 	 (set! ,(second e) (lambda ,(map car (third e)) ,@(cdddr e)))
	;; 	 (,(second e) ,@(map second (third e))))
        ;;       env)))
	;; ((set!)
        ;;  (let ((lookup (assq (second e) env)))
        ;;    (unless lookup (error "Unbound set! var:" e))
        ;;    (set-cdr! lookup (my-eval (third e) env))))
	;((letrec*) (error "TODO letrec* my-eval"))

;; (define libman (make-libman))
;; (set! library-search-paths (cons "./srfi2" library-search-paths))
;; (define file `((import (scheme base) (scheme r5rs) (scheme time) (scheme file) (scheme inexact)) ,@(with-input-from-file (second (command-line)) read-file)) )
;; (expander-init libman)
;; (define expanded (expand-program file "PROG-" libman))
;; (display expanded ) (newline)
;; (for line expanded
;;      (my-eval line `((display . ,display))))

