(include "qq.scm")
(include "memory_layout.scm")
(include "opcodes.scm")

(define next-id
  (let ((id 0))
    (lambda ()
      (let ((res id))
	(set! id (+ id 1))
	res))))

(define-record-type fun (%make-fun code name last-label args thunk debug-id debug-loc-id) fun?
		    (code fun-code fun-code-set!)
		    (name fun-name fun-name-set!)
		    (last-label fun-last-label fun-last-label-set!)
		    (args fun-args fun-args-set!)
		    (thunk fun-thunk fun-thunk-set!)
		    (debug-id fun-debug-id)
		    (debug-loc-id fun-debug-loc-id))

(define next-debug-id
  (let ((id 5))
    (lambda ()
      (set! id (+ id 1))
      id)))
(define debug-strings '())

(define (make-fun name)
  (%make-fun '() name "entry" '() #f (next-debug-id) (next-debug-id)))

(define (push-instr! fun instr)
  (fun-code-set! fun (cons instr (fun-code fun))))

(define functions '())
(define consts '())
(define complex-consts '())
(define const-hash (make-hash-table equal?))
(define symbol-table '())

(define (emit-const c)
  (when (symbol? c)
    (push! symbol-table c))
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

(define (emit-function fun nlambda var env)
  (define name (second nlambda))
  (define cases (cddr nlambda))
  (define last-case (last cases))
  (define last-case-jmp #f)
  (fun-name-set! fun var)

  (when (hash-table-exists? escapes-table var)
    (fun-thunk-set! fun #t)
    (fun-args-set! fun default-param-list)
    (push-instr! fun (format "%argcnt = load i64, ptr @argcnt"))
    (for (case i) (cases (iota (length cases)))
	 (let ((id (next-id))
	       (call-res (next-id))
	       (true-label (next-id))
	       (false-label (next-id)))
	   (if (list? (second case))
	       (push-instr! fun (format "%v~a = icmp eq i64 %argcnt, ~a" id (length (second case))))
	       (push-instr! fun (format "%v~a = icmp uge i64 %argcnt, ~a" id (- (length (to-proper (second case))) 1))))
	   (push-instr! fun (format "br i1 %v~a, label %l~a, label %l~a" id true-label false-label))
	   (push-instr! fun (format "l~a:" true-label))
	   (let ((arglist
		  (if (list? (second case))
		      default-param-list
		      (let* ((len (length (to-proper (second case))))
			     (argpos (next-id)))
			(push-instr! fun (format "store i64 ~a, ptr @wanted_argcnt" (- len 1)))
			(push-instr! fun (format "%v~a = call i64 @consargs_stub(~a), !dbg !~a" argpos
						 (join ", "
						       (omap arg default-param-list
							     (format "i64 ~a" arg)))
						 (fun-debug-loc-id fun)))
			(if (> len max-reg-args)
			    default-param-list
			    (append (take default-param-list (- len 1)) (list (format "%v~a" argpos))))))))
	     (push-instr! fun (format "%v~a = musttail call i64 @\"~a\"(~a), !dbg !~a"
				      call-res (format "~a_case~a" var i)
				      (reg-args-to-call-list arglist)
				      (fun-debug-loc-id fun))))
	   (push-instr! fun (format "ret i64 %v~a" call-res))
	   (push-instr! fun (format "l~a:" false-label)))))
  (push-instr! fun (format "%res = call i64 @SCM_ARGCNT_FAIL(), !dbg !~a" (fun-debug-loc-id fun)))
  (push-instr! fun (format "ret i64 %res"))
  
  (for (case i) (cases (iota (length cases)))
       (define cfun (make-fun 1))
       (define argcnt (length (to-proper (second case))))
       (define args (to-proper (second case)))
       (define arg-ids (omap arg args (format "%v~a" (next-id))))
       (define v-type (list? (second case)))
       (define case-label (format "~a_case~a" var i))
       (fun-args-set! cfun arg-ids)
       (fun-name-set! cfun case-label)
       (push! functions cfun)
       (emit (third case)
	     (append (map cons (to-proper (second case)) arg-ids)
		     env)
	     cfun
	     #t)))

(define (find-label-for-case nlambda argcnt var)
  (let loop ((cases (cddr nlambda)) (i 0))
    (if (null? cases)
	(begin (display (format "Warning: Can't find case for call:~a cnt ~a\n"
				 var argcnt) (current-error-port))
	       var)
	(let* ((case (car cases))
	       (arglist (second case)))
	  (if (list? arglist)
	      (if (= argcnt (length arglist))
		  (format "~a_case~a" var i)
		  (loop (cdr cases) (+ i 1)))
	      (if (>= argcnt (- (length (to-proper arglist)) 1))
		  ;; TODO: package up varargs.  For now, return thunk.
		  var
		  (loop (cdr cases) (+ i 1))))))))



;;;;;;;;;; Arglist management

(define (split-arglist args)
  (if (> (length args) max-reg-args)
      (split-at args max-reg-args)
      (values args '())))

;; Pad out param list to max-reg-args.
(define (reg-args-to-param-list args)
  (join ", "
	(omap arg (append args (if (< (length args) max-reg-args)
				   (make-list (- max-reg-args (length args)) "")
				   '()))
	      (format "i64 ~a" arg))))

(define (reg-args-to-call-list args)
  (join ", "
	(omap arg (append args (if (< (length args) max-reg-args)
				   (make-list (- max-reg-args (length args)) "undef")
				   '()))
	      (format "i64 ~a" arg))))

(define default-param-list (omap arg (iota max-reg-args)
				 (format "%a~a" arg)))

;;;;;;;; Main emit

(define (emit-call fun tail label args)
  (let ((id (next-id)))
    (let-values (((reg-args stack-args) (split-arglist args)))
      (for (arg i) (stack-args (iota (length stack-args)))
	   (push-instr! fun (format "call void @SCM_WRITE_SHADOW_STACK(i64 ~a, i64 ~a), !dbg !~a"
				    (* 8 i) arg
				    (fun-debug-loc-id fun))))
      (push-instr! fun (format "%v~a = ~a call i64 ~a(~a) #0, !dbg !~a"
			       id (if tail "musttail" "") label (reg-args-to-call-list reg-args)
			       (fun-debug-loc-id fun))))
    (format "%v~a" id)))

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
    ((global-set! ,var ,val)
     (emit `(define ,var ,val) env fun #f)
     (finish undefined-tag))
    ((define ,var ,val)
     (let ((val (emit val env fun #f))
	   (sym (emit-const var)))
       (push-instr! fun (format "call void @SCM_SET_GLOBAL(i64 ~a, i64 ~a), !dbg !~a"
				sym val (fun-debug-loc-id fun))))
     (finish undefined-tag))
    ((flonum-op ,op ,a ,b)
     (let ((av (emit a env fun #f))
	   (bv (emit b env fun #f))
	   (id (next-id)))
       (push-instr! fun (format "%f~a = ~a double ~a, ~a" id op av bv))
       (format "%f~a" id)))
    ((primcall const-init ,const ,call)
     (let ((res (emit call env fun #f)))
       (push-instr! fun (format "store i64 ~a, ptr @~a" res (const-label-label const)))
       #f))
    ((primcall APPLY ,args ___)
     (let* ((args (omap arg args (emit arg env fun #f)))
	    (clo-id (next-id))
	    (len (car args))
	    (funp (cadr args))
	    (stack-args (cdr args))
	    (lenshift (next-id)))
       (push-instr! fun (format "%v~a = call ptr @SCM_LOAD_CLOSURE_PTR(i64 ~a), !dbg !~a"
				clo-id funp (fun-debug-loc-id fun)))
       (push-instr! fun (format "%v~a = ashr i64 ~a, 3" lenshift len))
       (push-instr! fun (format "store i64 %v~a, ptr @argcnt" lenshift))
       (finish (emit-call fun tail (format "%v~a" clo-id) stack-args))))
    ;; TODO: callcc probably also needs to be musttail.
    ;; but, the calling conventions don't match.
    ((primcall FOREIGN_CALL ,name ,args ___)
     (let* ((id (next-id))
	    (args (omap arg args (format "i64 ~a" (emit arg env fun #f))))
	    (argstr (join ", " args)))
       (push-instr! fun (format "%v~a = call ~a i64 @\"~a\"(~a) #0, !dbg !~a"
				id
				(if (equal? "SCM_CALLCC" (substring name 0 (min (string-length name) 10)))
				    "preserve_nonecc" "")
				name argstr (fun-debug-loc-id fun)))
       (finish (format "%v~a" id))))
    ((primcall ,var ,vals ___)
     (let* ((vals (omap val vals (emit val env fun #f)))
	    (arglist (join ", " (omap val vals (format "i64 ~a" val))))
	    (id (next-id)))
       (push-instr! fun (format "%v~a = call i64 @\"~a\"(~a), !dbg !~a"
				id (string-append "SCM_" (symbol->string var)) arglist
				(fun-debug-loc-id fun)))
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
	    (clo-id (next-id)))
       (push-instr! fun (format "%v~a = call ptr @SCM_LOAD_CLOSURE_PTR(i64 ~a), !dbg !~a"
				clo-id (car args) (fun-debug-loc-id fun)))
       (push-instr! fun (format "store i64 ~a, ptr @argcnt" (length args)))
       (finish (emit-call fun tail (format "%v~a" clo-id) args))))
    ((label-call ,label ,args ___)
     (let* ((args (omap arg args (emit arg env fun #f)))
	    (id (next-id))
	    (lfun (cond
		   ((assq label env) => cdr)
		   (else (hash-table-ref global-labels label))))
	    (case-label (find-label-for-case lfun (length args) label)))
       ;; TODO: varargs inline calls
       (when (equal? case-label label)
	 (push-instr! fun (format "store i64 ~a, ptr @argcnt" (length args))))
       (finish (emit-call fun tail (format "@\"~a\"" case-label) args))))
    ((let ((,vars ,vals) ___) ,body)
     (let ((args (omap val vals (emit val env fun #f))))
       (emit body (append (map cons vars args) env) fun tail)))
    ((closure (label ,label) ,args ___)
     (let* ((args (omap arg args (emit arg env fun #f)))
	    (id (next-id)))
       (push-instr! fun ( format "%v~a = call i64 @SCM_CLOSURE(i64 ptrtoint (ptr @\"~a\" to i64), i64 ~a), !dbg !~a"
			  id label (length args) (fun-debug-loc-id fun)))
       
       (for (arg i) (args (iota (length args)))
	    (push-instr! fun (format "call void @SCM_CLOSURE_SET_FAST(i64 %v~a, i64 ~a, i64 ~a), !dbg !~a"
				     id arg i (fun-debug-loc-id fun))))
       (format "%v~a" id)))
    ((const-closure (label ,label))
     (finish (emit-const `($const-closure ,label))))
    ((labels ((,vars ,lambdas) ___) ,body)
     (let* ((funs (map-in-order
		   (lambda (id) (define fun (make-fun 1))
			   (push! functions fun)
			   fun)
		   vars))
	    (env (append (map cons vars lambdas) env)))
       (for (func-p lambda var) (funs lambdas vars)
	    (emit-function func-p lambda var env))
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
       (push-instr! fun (format "%v~a = call i64 @SCM_LOAD_GLOBAL(i64 ~a), !dbg !~a" id sym (fun-debug-loc-id fun)))
       ;; ;; Offset in to the global.
       ;; (push-instr! fun (format "%v~a = add i64 ~a, ~a" id sym (+ (- ptr-tag) 16)))
       ;; (push-instr! fun (format "%v~a = inttoptr i64 %v~a to i64*" pid id))
       ;; (push-instr! fun (format "%v~a = load i64, i64* %v~a" resid pid))
       (finish (format "%v~a" id))))
    (,const
     (guard (const-label? const))
     (push! consts (format "@~a = private unnamed_addr global i64 0, align 8\n"
			   (const-label-label const)))
     (let ((id (next-id)))
       (push-instr! fun (format "%v~a = load i64, ptr @~a" id (const-label-label const)))
       (push! complex-consts (format "@~a" (const-label-label const)))
       (finish (format "%v~a" id))))
    (,const
     (guard (not (pair? const)))
     (finish (emit-const const)))
    ((quote ,const)
     (finish (emit-const const)))
    (,else (error "UNKOWN EMIT:" sexp))))


(define (flonum? c)
  (and (number? c) (inexact? c)))

(define (fixnum? c)
  (and (number? c) (exact? c) (integer? c)  (real? c)
       (< (abs c) #x1fffffffffffffff)))

(define (fix-string-format str) ;; some extra work since we have no char-type.
  (define (str-ref-int str pos) (char->integer (string-ref str pos)))
  (define (esc-char hex1 hex2 rest)
    (cons (integer->char 92) 
          (cons (integer->char hex1) (cons (integer->char hex2) rest))))
  (define (fix-str-format str pos end)
    (cond ((= pos end) '())
          ((eq? (str-ref-int str pos) 34) 
           (esc-char 50 50 (fix-str-format str (+ pos 1) end)))
          ((eq? (str-ref-int str pos) 92)
           (esc-char 53 67 (fix-str-format str (+ pos 1) end)))
          (else (cons (string-ref str pos) 
                      (fix-str-format str (+ pos 1) end)))))
  (list->string (fix-str-format str 0 (string-length str))))

(define (add-const c)
  (cond
   ((symbol? c)
    (let ((str (emit-const (symbol->string c)))
	  (sym-name (string-append "SYM-" (symbol->string c))))
      (push! consts (format "@\"~a\" = private unnamed_addr global {i64, i64, i64} {i64 ~a, i64 ~a, i64 ~a}, align 8\n"
			    sym-name symbol-tag str undefined-tag))
      (format "add (i64 ~a, i64 ptrtoint ({i64, i64, i64}* @\"~a\" to i64))"
	      ptr-tag sym-name)))
   ((flonum? c)
    (let* ((id (next-id))
	   (u (get-double-as-u64 c))
	   (urot (bit-field-rotate u 4 0 64))
	   (plus (+ urot 1))
	   (low (bitwise-and urot 7)))
      (if (memq low '(0 3 4))
	  plus
	  (let ((id (next-id)))
	    (push! consts (format "@flonum~a = private unnamed_addr global [2 x i64] [i64 ~a, i64 ~a], align 8\n"
				  id flonum-tag u))
	    (format "add (i64 ~a, i64 ptrtoint ([2 x i64]* @flonum~a to i64))"
		    ptr-tag id)	    
	    ))))
   ((fixnum? c)
    (* 8 c))
   ((char? c) (+ char-tag (* 256 (char->integer c))))
   ((boolean? c)
    (if c true-rep false-rep))
   ((null? c) nil-tag)
   ((string? c)
    (let ((id (next-id)))
      (push! consts (format "@str~a = private unnamed_addr constant {i64, i64, [~a x i8]} {i64 ~a, i64 ~a, [~a x i8] c\"~a\"}, align 8\n"
			    id (string-length c) string-tag (* 8 (string-length c)) (string-length c) (fix-string-format c) ))
      (format "add (i64 ~a, i64 ptrtoint ({i64, i64, [~a x i8]}* @str~a to i64))"
	      ptr-tag (string-length c) id)))
   ((and (pair? c) (eq? '$label (car c)))
    (abort 'const-label)
    (let ((fun (list-ref functions (cdr c))))
      (fun-bc-start fun)))
   ;; TODO use a record
   ((and (pair? c) (eq? '$const-closure (car c)))
    (let ((id (next-id)))
      (push! consts (format "@clo~a = private unnamed_addr constant [2 x i64] [i64 ~a, i64 ptrtoint (ptr @\"~a\" to i64)], align 8" id closure-tag (second c)))
      (format "add (i64 ~a, i64 ptrtoint ([2 x i64]* @clo~a to i64))"
	      ptr-tag id)))
   ((vector? c)
    ;; Prepend the length & nullptr for slab ptr
    (let* ((vals (append (list (* 8 (vector-length c)) 0)
			 (omap val (vector->list c) (emit-const val))))
	   (val-str (join ", " (omap val vals (format "i64 ~a" val))))
	   (id (next-id)))
      (push! consts (format "@vec~a = private unnamed_addr constant [~a x i64] [~a], align 8\n"
			    id (+ 2 (vector-length c)) val-str ))
      (format "add (i64 ~a, i64 ptrtoint ([~a x i64]* @vec~a to i64))"
	      vector-tag (+ 2 (vector-length c)) id)))
   ((pair? c)
    (let* ((id (next-id))
	   (a (emit-const (car c)))
	   (b (emit-const (cdr c))))
      (push! consts (format "@cons~a = private unnamed_addr constant {i64, i64} {i64 ~a, i64 ~a}, align 8"
			    id a b))
      (format "add (i64 ~a, i64 ptrtoint ({i64, i64}* @cons~a to i64))"
	      cons-tag id)))
   (else (display (format "WARNING: Unknown Const: ~a\n" c) (current-error-port))
	 0)))

(define (emit-header)
  (display
   "target triple = \"x86_64-pc-linux-gnu\"\n
declare i64 @SCM_DISPLAY (i64)
declare i64 @SCM_ADD (i64, i64)
declare i64 @SCM_DIV (i64, i64)
declare i64 @SCM_QUOTIENT (i64, i64)
declare i64 @SCM_MUL (i64, i64)
declare i64 @SCM_MOD (i64, i64)
declare i64 @SCM_LT (i64, i64)
declare i64 @SCM_LTE (i64, i64)
declare i64 @SCM_GT (i64, i64)
declare i64 @SCM_GTE (i64, i64)
declare i64 @SCM_SUB (i64, i64)
declare i64 @SCM_NUM_EQ (i64, i64)
declare i64 @SCM_GUARD (i64, i64)
declare i64 @SCM_CLOSURE (i64, i64)
declare i64 @SCM_CLOSURE_GET (i64, i64)
declare i64 @SCM_ARGCNT_FAIL ()
declare void @SCM_CLOSURE_SET (i64, i64, i64)
declare void @SCM_CLOSURE_SET_FAST (i64, i64, i64)
declare i64 @SCM_LOAD_GLOBAL(i64)
declare void @SCM_SET_GLOBAL(i64, i64)
declare ptr @SCM_LOAD_CLOSURE_PTR(i64)
declare preserve_nonecc i64 @SCM_CALLCC (i64)  #0
declare preserve_nonecc i64 @SCM_CALLCC_ONESHOT (i64)  #0
declare void @consargs_stub () #0
declare i64 @SCM_CONS (i64, i64)
declare i64 @SCM_CAR (i64)
declare i64 @SCM_CDR (i64)
declare i64 @SCM_SETCAR(i64,i64)
declare i64 @SCM_SETCDR(i64,i64)
declare i64 @SCM_MAKE_VECTOR(i64)
declare i64 @SCM_VECTOR_LENGTH(i64)
declare i64 @SCM_VECTOR_REF(i64, i64)
declare i64 @SCM_VECTOR_SET (i64, i64, i64)
declare i64 @SCM_VECTOR_SET_FAST (i64, i64, i64)
declare i64 @SCM_STRING_LENGTH (i64)
declare i64 @SCM_EQ (i64, i64)
declare i64 @SCM_MAKE_STRING (i64, i64)
declare i64 @SCM_IS_FLONUM(i64)
declare i64 @SCM_IS_BIGNUM(i64)
declare i64 @SCM_IS_RATNUM(i64)
declare i64 @SCM_CHAR_INTEGER(i64)
declare i64 @SCM_INTEGER_CHAR(i64)
declare i64 @SCM_SYMBOL_STRING(i64)
declare i64 @SCM_MAKE_SYMBOL(i64)
declare i64 @SCM_STRING_SET(i64,i64,i64)
declare i64 @SCM_STRING_REF(i64,i64)
declare i64 @SCM_EXACT(i64)
declare i64 @SCM_INEXACT(i64)
declare i64 @SCM_MAKE_RECORD(i64)
declare i64 @SCM_RECORD_SET(i64, i64, i64)
declare i64 @SCM_RECORD_REF(i64, i64)
declare i64 @SCM_GET_SYM_TABLE()
declare i64 @SCM_SIN(i64)
declare i64 @SCM_COS(i64)
declare i64 @SCM_ASIN(i64)
declare i64 @SCM_ACOS(i64)
declare i64 @SCM_ATAN(i64)
declare i64 @SCM_TAN(i64)
declare i64 @SCM_SQRT(i64)
declare i64 @SCM_ROUND(i64)
declare i64 @SCM_FLOOR(i64)
declare i64 @SCM_CEILING(i64)
declare i64 @SCM_EXP(i64)
declare i64 @SCM_LOG(i64)
declare i64 @SCM_EQ_HASH(i64)
declare i64 @SCM_BIGNUM_STR(i64)
declare i64 @SCM_RATNUM_STR(i64)
declare i64 @SCM_BIGNUM_SQRT(i64)
declare i64 @SCM_FLONUM_STR(i64)
declare i64 @SCM_STRING_HASH(i64)
declare i64 @SCM_STRING_CPY(i64,i64,i64,i64,i64)
declare i64 @SCM_AND(i64, i64)
declare i64 @SCM_OPEN_FD(i64, i64)
declare i64 @SCM_CLOSE_FD(i64)
declare i64 @SCM_READ_FD(i64, i64)
declare i64 @SCM_WRITE_FD(i64, i64, i64)
declare i64 @SCM_FILE_EXISTS(i64)
declare i64 @SCM_DELETE_FILE(i64)
declare i64 @SCM_READ_SHADOW_STACK(i64)
declare void @SCM_WRITE_SHADOW_STACK(i64, i64)

declare void @gc_init ()
@argcnt = dso_local global i64 0
@wanted_argcnt = dso_local global i64 0
attributes #0 = { returns_twice}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}
!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: \"popl\", emissionKind: FullDebug, nameTableKind: None)
!1 = !DIFile(filename: \"fib2.scm\", directory: \"/home/davejwatson/projects/popl\")
!2 = !{i32 7, !\"Dwarf Version\", i32 3}
!3 = !{i32 2, !\"Debug Info Version\", i32 3}
!4 = !DISubroutineType(types: !5)
!5 = !{null}

"))

(define (compile file verbose)
  (set! functions '())
  (let* ((libman (make-libman))
	 (unused (set! library-search-paths (cons "./srfi2" library-search-paths)))
	 (runtime-input (with-input-from-file "runtime2.scm" read-file))
	 (eval-input (with-input-from-file "eval.scm" read-file))

	 (pre-input (with-input-from-file file read-file))

	 ;; Auto-add 'import' to scripts.
	 (input
	  (match pre-input
	    (((import ,anything ___) ,body ___) pre-input)
	    (,else `((import  (scheme base) (scheme r5rs) (scheme time) (scheme file) (scheme inexact) ) ,@pre-input))))
	 (unused (expander-init libman))
	 (runtime (expand-program runtime-input "" libman))
	 (evals (expand-program eval-input "" libman))
	 (prog (expand-program input "PROG-" libman))
	 (lowered (r7-pass `(begin  	,@runtime
					,@evals
					,@prog
					0 ;; return value.
					) #f))
	 (main-fun (make-fun "main")))
    (when verbose
      (display (format "Compiling ~a\n" file) (current-error-port))
      ;(pretty-print lowered (current-error-port))
      )
					;;(exit)
    (emit lowered '() main-fun #t)
    (emit-header)

    (let ((sym-vec (emit-const (list->vector symbol-table))))
      (for const (reverse! consts)
	   (display const)
	   (newline))

      (display (format "@symbol_table = constant i64 ~a\n"
		       sym-vec)))

    (display (format "@complex_constants = constant [~a x ptr] [~a]\n"
		     (length complex-consts) (join ", " (omap const complex-consts (format "ptr ~a" const)))))
    (display (format "@complex_constants_len = constant i64 ~a\n"
		     (length complex-consts)))

    (set! functions (reverse! (cons main-fun functions)))
    (for func functions
	 (fun-code-set! func (reverse! (fun-code func)))
	 (newline)

	 (let-values (((reg-args stack-args) (split-arglist (fun-args func)))
		      ((debug-id) (fun-debug-id func))
		      ((debug-loc-id) (fun-debug-loc-id func)))
	   ;; Set debug info
	   (push! debug-strings
		  (format "!~a = distinct !DISubprogram(name: \"~a\", scope: !1, file: !1, type: !4, unit: !0)"
			  debug-id (fun-name func)))
	   (push! debug-strings
		  (format "!~a = !DILocation(line:1, scope: !~a)" debug-loc-id debug-id))
	   
	   (display (format "define ~a i64 @\"~a\"(~a) #0 !dbg !~a {\n" 
			    (if (equal? (fun-name func) "main")
				"" "internal")
			    (fun-name func)
			    (reg-args-to-param-list reg-args)
			    (fun-debug-id func)))
	   (display (format " entry:\n"))
	   (when (equal? "main" (fun-name func))
	     (display (format "  call void @gc_init(), !dbg !~a\n" (fun-debug-loc-id func))))
	   (for (arg i) (stack-args (iota (length stack-args)))
		(display (format "  ~a = call i64 @SCM_READ_SHADOW_STACK(i64 ~a), !dbg !~a\n"
				 arg i (fun-debug-loc-id func))))
	   (for line (fun-code func)
		(if (loop-var? line)
		    (for phi (loop-var-phis line)
			 (display (format "  ~a\n" phi)))
		    (display (format "  ~a\n" line))))
	   (display "}\n"))))
  (for line (reverse! debug-strings)
       (display line) (newline)))

(for file (cdr (command-line))
     (compile file #t))

