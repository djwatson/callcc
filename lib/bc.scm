(include "qq.scm")
(include "memory_layout.scm")
(include "opcodes.scm")

(define next-id
  (let ((id 0))
    (lambda ()
      (let ((res id))
	(set! id (+ id 1))
	res))))

(define-record-type fun (%make-fun code name last-label args) fun?
		    (code fun-code fun-code-set!)
		    (name fun-name fun-name-set!)
		    (last-label fun-last-label fun-last-label-set!)
		    (args fun-args fun-args-set!))

(define (make-fun name)
  (%make-fun '() name "entry" '()))

(define (push-instr! fun instr)
  (fun-code-set! fun (cons instr (fun-code fun))))

(define functions '())
(define consts '())
(define const-hash (make-hash-table equal?))

(define (emit-const c)
  (cond
   ((hash-table-ref/default const-hash c #f))
   (else
    (let ((val (add-const c)))
      (hash-table-set! const-hash  c val)
      val))))

(define-record-type loop-var (make-loop-var dest phis) loop-var?
		    (dest loop-var-dest)
		    (phis loop-var-phis loop-var-phis-set!))

(define (abort str)
  (display "TODO:")
  (display str)
  (newline)
  (exit))

(define (emit sexp env fun tail)
  (define (finish res)
    (if tail
	(begin
	  (when res
	    (push-instr! fun (format "ret i64 ~a" res)))
	  #f)
	res))
  (match sexp
    ((begin ,sexps ___ ,end)
     (for sexp sexps
	  (emit sexp env fun #f))
     (emit end env fun tail))
    ((set! ,var ,val)
     (emit `(define ,var ,val) env fun #f)
     (finish undefined-tag))
    ((define ,var ,val)
     (let ((val (emit val env fun #f))
	   (sym (emit-const var))
	   (id (next-id))
	   (pid (next-id)))
       ;; Offset in to the global.
       (push-instr! fun (format "%v~a = add i64 ~a, ~a" id sym (+ (- ptr-tag) 16)))
       (push-instr! fun (format "%v~a = inttoptr i64 %v~a to i64*" pid id))
       (push-instr! fun (format "store i64 ~a, i64* %v~a" val pid)))
     (finish undefined-tag))
    ((primcall ,op ,cell ,val ,loc)
     (guard (memq op '(STORE STORE_CHAR)))
     (let* ((val-cell (emit cell env fun #f))
	    (val-reg (emit val env fun #f))
	    (val-loc (emit loc env fun #f)))
       (abort op)
       (finish rg)))
    ((primcall FOREIGN_CALL ,name ,args ___)
     (let* ((id (next-id))
	    (args (omap arg args (format "i64 ~a" (emit arg env fun #f))))
	    (argstr (join ", " args)))
       (push-instr! fun (format "%v~a = call ~a i64 @\"~a\"(~a) ~a"
				id
				(if (equal? "SCM_CALLCC" name)
				    "preserve_nonecc" "")
				name argstr
				(if (equal? "SCM_CALLCC" name)
				    "#0" "")))
       (finish (format "%v~a" id))))
    ((primcall ,var ,vals ___)
     (let* ((vals (omap val vals (emit val env fun #f)))
	    (arglist (join ", " (omap val vals (format "i64 ~a" val))))
	    (id (next-id)))
       (push-instr! fun (format "%v~a = call i64 @\"~a\"(~a)"
		 id (string-append "SCM_" (symbol->string var)) arglist))
       (finish (format "%v~a" id))))
    ((if ,a ,b ,c)
     (let ((id (next-id))
	   (join-id (next-id))
	   (true (format "true~a" (next-id)))
	   (false (format "false~a" (next-id)))
	   (join (format "join~a" (next-id)))
	   (test (emit a env fun #f)))
       (push-instr! fun (format "%test~a = icmp ne i64 ~a, ~a" id test false-rep))
       (push-instr! fun (format "br i1 %test~a, label %~a, label %~a" id true false))
       (push-instr! fun (format "~a:" true))
       (fun-last-label-set! fun true)
       (let* ((t-res (emit b env fun tail))
	      (t-last-label (fun-last-label fun)))
	 (if (or tail (not t-res))
	     #f
	     (push-instr! fun (format "br label %~a" join)))
	 (push-instr! fun (format "~a:" false))
	 (fun-last-label-set! fun false)
	 (let* ((f-res (emit c env fun tail))
		(f-last-label (fun-last-label fun)))
	   (if (or tail (not f-res))
	       #f
	       (push-instr! fun (format "br label %~a" join)))
	   (if (or tail (not (or f-res t-res)))
	       #f
	       (begin
		 (push-instr! fun (format "~a:" join))
		 (fun-last-label-set! fun join)
		 (if (and f-res t-res)
		     (begin
		       (push-instr! fun (format "%v~a = phi i64 [~a, %~a], [~a, %~a]"
						join-id t-res t-last-label f-res f-last-label))
		       (format "%v~a" join-id))
		     (or f-res t-res))))))))
    ((call ,loop-var ,args ___)
     (guard (and (assq loop-var env) (loop-var? (cdr (assq loop-var env)))))
     (let ((args (omap arg args (emit arg env fun #f)))
	   (loop (cdr (assq loop-var env))))
       (push-instr! fun (format "br label %~a" (loop-var-dest loop)))
       (loop-var-phis-set! loop
			   (omap (arg phi) (args (loop-var-phis loop))
				 (string-append phi (format ", [~a, %~a]" arg (fun-last-label fun)))))
       #f))
    ((call ,args ___)
     (let* ((args (omap arg args (emit arg env fun #f)))
	    (arglist (join ", " (omap arg args (format "i64 ~a" arg))))
	    (id (next-id))
	    (pid (next-id))
	    (clo-id (next-id))
	    (fid (next-id)))
       (push-instr! fun (format "%v~a = add i64 ~a, ~a" id (car args) (+ (- ptr-tag) 8)))
       (push-instr! fun (format "%v~a = inttoptr i64 %v~a to ptr" pid id))
       (push-instr! fun (format "%v~a = load ptr, ptr %v~a" clo-id pid))

       (push-instr! fun (format "%v~a = call i64 %v~a(~a)" fid clo-id arglist))
       (finish (format "%v~a" fid)))
     )
    ((label-call ,label ,args ___)
     (let* ((args (omap arg args (emit arg env fun #f)))
	    (arglist (join ", " (omap arg args (format "i64 ~a" arg))))
	    (id (next-id)))
       (push-instr! fun (format "%v~a = call i64 @\"~a\"(~a)" id label arglist))
       (finish (format "%v~a" id))))
    ((let ((,vars ,vals) ___) ,body)
     (let ((args (omap val vals (emit val env fun #f))))
       (emit body (append (map cons vars args) env) fun tail)))
    ((closure (label ,label) ,args ___)
     (let* ((args (omap arg args (emit arg env fun #f)))
	    (id (next-id)))
       (push-instr! fun ( format "%v~a = call i64 @SCM_CLOSURE(i64 ptrtoint (ptr @~a to i64), i64 ~a)"
			  id label (length args)))
       
       (for (arg i) (args (iota (length args)))
	    (push-instr! fun (format "call void @SCM_CLOSURE_SET(i64 %v~a, i64 ~a, i64 ~a)"
				     id arg i)))
       (format "%v~a" id)))
    ((const-closure (label ,label))
     (finish (emit-const `($const-closure ,label))))
    ((labels ((,vars ,lambdas) ___) ,body)
     (let* ((label-ids (iota (length vars) (length functions)))
	    (funs (map-in-order (lambda (id) (define fun (make-fun 1))
					(push! functions fun)
					fun) label-ids))
	    (env (append (map cons vars label-ids) env)))
       (for-each (lambda (func-p nlambda var id)
		   (define name (second nlambda))
		   (define cases (cddr nlambda))
		   (define last-case (last cases))
		   (define last-case-jmp #f)
		   (fun-name-set! func-p var)
		   (for case cases
			(define argcnt (length (to-proper (second case))))
			(define args (second case))
			(define arg-ids (omap arg args (format "%v~a" (next-id))))
			(define v-type (list? (second case)))
			(fun-args-set! func-p arg-ids)
			(emit (third case)
			      (append (map cons (to-proper (second case)) arg-ids)
				      env)
			      func-p
			      #t)))
		 funs lambdas vars label-ids)
       (emit body env fun tail)))
    (,var
     ;; Only lookup, doesn't gen code.
     (guard (symbol? var))
     (finish
      (cond
       ((assq var env) => cdr)
       (else
	(error "Unknown sym:" var)))))
    ((loop  ,vars ,name ,body ,inits ___)
     (let* ((args (omap init inits (emit init env fun #f)))
	    (label (format "loop~a" (next-id)))
	    (phi-ids (omap init inits (format "%v~a" (next-id))))
	    (phis (omap (phi init) (phi-ids args) (format "~a = phi i64 [~a, %~a]" phi init (fun-last-label fun))))
	    (loop (make-loop-var label phis)))
       (push-instr! fun (format "br label %~a" label))
       (push-instr! fun (format "~a:" label))
       (push-instr! fun loop)
       (fun-last-label-set! fun label)
       (emit body (append (list (cons name loop)) (map cons vars phi-ids) env) fun tail)))
    ((lookup ,var)
     (let ((sym (emit-const var))
	   (id (next-id))
	   (pid (next-id))
	   (resid (next-id)))
       ;; Offset in to the global.
       (push-instr! fun (format "%v~a = add i64 ~a, ~a" id sym (+ (- ptr-tag) 16)))
       (push-instr! fun (format "%v~a = inttoptr i64 %v~a to i64*" pid id))
       (push-instr! fun (format "%v~a = load i64, i64* %v~a" resid pid))
       (finish (format "%v~a" resid))))
    (,const
     (guard (not (pair? const)))
     (finish (emit-const const)))
    ((quote ,const)
     (finish (emit-const const)))
    (,else (error "UNKOWN EMIT:" sexp))))

(define (dformat . args)
  (display (apply format args)))

(define (flonum? c)
  (and (number? c) (inexact? c)))

(define (fixnum? c)
  (and (number? c) (exact? c)
       (< (abs c) #x1fffffffffffffff)))

(define (add-const c)
  (cond
   ((symbol? c)
    (let ((str (emit-const (symbol->string c)))
	  (id (next-id)))
      (push! consts (format "@sym~a = internal unnamed_addr global {i64, i64, i64} {i64 ~a, i64 ~a, i64 ~a}, align 8\n"
			    id symbol-tag str undefined-tag))
      (format "add (i64 ~a, i64 ptrtoint ({i64, i64, i64}* @sym~a to i64))"
	      ptr-tag id)))
   ((flonum? c)
    (let* ((id (next-id))
	   (u (get-double-as-u64 c))
	   (urot (bit-field-rotate u 4 0 64))
	   (plus (+ urot 1))
	  (low (bitwise-and urot 7)))
      (unless (memq low '(0 3 4))
	(error "Bad flonum:" c))
      plus))
    ((fixnum? c)
     (* 8 c))
   ((char? c) (+ char-tag (* 256 (char->integer c))))
   ((boolean? c)
    (if c true-rep false-rep))
   ((null? c) nil-tag)
   ((string? c)
    (let ((id (next-id)))
      (push! consts (format "@str~a = internal unnamed_addr constant {i64, i64, [~a x i8]} {i64 ~a, i64 ~a, [~a x i8] c\"~a\"}, align 8\n"
		       id (string-length c) string-tag (* 8 (string-length c)) (string-length c) c ))
      (format "add (i64 ~a, i64 ptrtoint ({i64, i64, [~a x i8]}* @str~a to i64))"
	      ptr-tag (string-length c) id)))
   ((and (pair? c) (eq? '$label (car c)))
    (abort 'const-label)
    (let ((fun (list-ref functions (cdr c))))
      (fun-bc-start fun)))
   ;; TODO use a record
   ((and (pair? c) (eq? '$const-closure (car c)))
    (let ((id (next-id)))
      (push! consts (format "@clo~a = internal unnamed_addr constant [2 x i64] [i64 ~a, i64 ptrtoint (ptr @\"~a\" to i64)], align 8" id closure-tag (second c)))
      (format "add (i64 ~a, i64 ptrtoint ([2 x i64]* @clo~a to i64))"
	      ptr-tag id)))
   ((vector? c)
    (let* ((vals (omap val (vector->list c) (add-const val)))
	   (val-str (join ", " (omap val vals (format "i64 ~a" val))))
	   (id (next-id)))
      (push! consts (format "@vec~a = internal unnamed_addr constant [~a x i64] [i64 ~a, ~a], align 8\n"
			    id (+ 1 (vector-length c)) (* 8 (vector-length c)) val-str ))
      (format "add (i64 ~a, i64 ptrtoint ([~a x i64]* @vec~a to i64))"
	      vector-tag (+ 1 (vector-length c)) id)))
   ((pair? c)
    (let* ((id (next-id))
	   (a (emit-const (car c)))
	   (b (emit-const (cdr c))))
      (push! consts (format "@cons~a = internal unnamed_addr constant {i64, i64} {i64 ~a, i64 ~a}, align 8"
			    id a b))
      (format "add (i64 ~a, i64 ptrtoint ({i64, i64}* @cons~a to i64))"
	      cons-tag id)))
   (else (error "Unknown Const: " c))))

(define (emit-header)
  (display
   "target triple = \"x86_64-pc-linux-gnu\"\n
declare i64 @display (i64)
declare i64 @SCM_ADD (i64, i64)
declare i64 @SCM_DIV (i64, i64)
declare i64 @SCM_LT (i64, i64)
declare i64 @SCM_GT (i64, i64)
declare i64 @SCM_GTE (i64, i64)
declare i64 @SCM_SUB (i64, i64)
declare i64 @SCM_NUM_EQ (i64, i64)
declare i64 @SCM_GUARD (i64, i64)
declare i64 @SCM_CLOSURE (i64, i64)
declare i64 @SCM_CLOSURE_GET (i64, i64)
declare void @SCM_CLOSURE_SET (i64, i64, i64)
declare preserve_nonecc i64 @SCM_CALLCC (i64)  #0
declare i64 @append (i64, i64)
declare i64 @cons (i64, i64)
declare i64 @car (i64)
declare i64 @cdr (i64)
declare i64 @setcar (i64,i64)
declare i64 @setcdr (i64,i64)
declare i64 @make_vector (i64)
declare i64 @vector_length (i64)
declare i64 @vector_ref (i64, i64)
declare i64 @vector_set (i64, i64, i64)
declare void @gc_init ()
attributes #0 = { noinline returns_twice }
"))

(define (compile file verbose)
  (set! functions '())
  (let* ((libman (make-libman))
	 (runtime-input (with-input-from-file "runtime2.scm" read-file))
	 (pre-input (with-input-from-file file read-file))
	 ;; Auto-add 'import' to scripts.
	 (input
	  (match pre-input
	    (((import ,anything ___) ,body ___) pre-input)
	    (,else `((import (scheme base) (scheme r5rs) (scheme time) (scheme file)) ,@pre-input))))
	 (unused (expander-init libman))
	 (runtime (expand-program runtime-input "" libman))
	 (prog (expand-program input "PROG-" libman))
	 (lowered (r7-pass `(begin	,@runtime
			      ,@prog
			      ) #f))
	 (main-fun (make-fun "main")))
    (when verbose
      (display (format "Compiling ~a\n" file) (current-error-port))
      (pretty-print lowered (current-error-port)))

    (emit lowered '() main-fun #t)
    (emit-header)

    (for const (reverse! consts)
	 (display const)
	 (newline))

    (set! functions (reverse! (cons main-fun functions)))
    (for func functions
	 (fun-code-set! func (reverse! (fun-code func)))

	 (newline)
	 (display (format "define i64 @\"~a\"(~a) {\n" (fun-name func)
			  (join ", " (omap arg (fun-args func) (format "i64 ~a" arg)))))
	 (display (format " entry:\n"))
	 (when (equal? "main" (fun-name func))
	   (display (format "  call void @gc_init()\n")))
	 (for line (fun-code func)
	      (if (loop-var? line)
		  (for phi (loop-var-phis line)
		       (display "  ") (display phi) (newline))
		  (begin
		    (display "  ") (display line) (newline))))
	 (display "}\n"))
    ))

(for file (cdr (command-line))
     (compile file #t))

