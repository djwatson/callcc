(include "config.scm")

(define next-id (make-counter 0))

(define-record-type fun (%make-fun code name last-label args thunk debug-id debug-loc-id) fun?
		    (code fun-code fun-code-set!)
		    (name fun-name fun-name-set!)
		    (last-label fun-last-label fun-last-label-set!)
		    (args fun-args fun-args-set!)
		    (thunk fun-thunk fun-thunk-set!)
		    (debug-id fun-debug-id)
		    (debug-loc-id fun-debug-loc-id))

(define next-debug-id (make-counter 6))
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
  (or (hash-table-ref/default const-hash c #f)
      (let ((val (add-const c)))
	(hash-table-set! const-hash c val)
	val)))

(define-record-type loop-var (make-loop-var dest phis) loop-var?
		    (dest loop-var-dest)
		    (phis loop-var-phis loop-var-phis-set!))

;; TODO: parameterize
;; This is so we don't have GENSYM'd names emitted for functions
;; if possible: The ASM label is known in advance (assuming the function
;; is only defined once)

(define global-fun-labels (make-hash-table eq?))
(define global-fun-names (make-hash-table equal?))
(define (fun-to-label lam var)
  (if (hash-table-exists? global-fun-labels lam)
      (hash-table-ref/default global-fun-labels lam #f)
      (if (hash-table-exists? global-fun-names (second lam))
	  (begin
	    (hash-table-set! global-fun-labels lam var)
	    var)
	  (let ((label (string-append "S_" (second lam))))
	    (hash-table-set! global-fun-names (second lam) #t)
	    (hash-table-set! global-fun-labels lam label)
	    label))))

(define (emit-function fun nlambda var env)
  (let ((cases (cddr nlambda)))
    (fun-name-set! fun (fun-to-label nlambda var))

    (when (hash-table-exists? escapes-table var)
      (fun-thunk-set! fun #t)
      (fun-args-set! fun default-param-list)
      (push-instr! fun "%argcnt = load i64, ptr @argcnt")
      (for (case i) (cases (iota (length cases)))
	   (let ((id (next-id))
		 (call-res (next-id))
		 (true-label (next-id))
		 (false-label (next-id)))
	     (push-instr! fun
			  (if (list? (second case))
			      (format "%v~a = icmp eq i64 %argcnt, ~a" id (length (second case)))
			      (format "%v~a = icmp uge i64 %argcnt, ~a" id (- (length (to-proper (second case))) 1))))
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
	       #t))))

(define (find-label-for-case nlambda argcnt var)
  (let loop ((cases (cddr nlambda)) (i 0))
    (if (null? cases)
	(begin
	  ;; (display (format "Warning: Can't find case for call:~a cnt ~a\n"
	  ;; 		   var argcnt) (current-error-port))
	  var)
	(let ((arglist (second (car cases))))
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

(define foreign-funcs (make-hash-table string=?))

(define (add-foreign-call name cnt)
  (unless (hash-table-exists? foreign-funcs name)
    ;; These are either only emitted from the code generator,
    ;; or need custom flags (like preserve_none)
    (unless (member name '("SCM_CALLCC" "SCM_CALLCC_ONESHOT" "SCM_SET_GLOBAL" "SCM_LOAD_GLOBAL"
			   "SCM_CLOSURE_SET"
			   "SCM_WRITE_SHADOW_STACK"))
      (hash-table-set! foreign-funcs name cnt))))

;;;;;;;; Main emit

(define (emit-call fun tail label args)
  (let-values (((id) (next-id))
	       ((reg-args stack-args) (split-arglist args)))
    (for (arg i) (stack-args (iota (length stack-args)))
	 (push-instr! fun (format "call void @SCM_WRITE_SHADOW_STACK(i64 ~a, i64 ~a), !dbg !~a"
				  (* 8 i) arg
				  (fun-debug-loc-id fun))))
    (push-instr! fun (format "%v~a = ~a call i64 ~a(~a) #0, !dbg !~a"
			     id (if tail "musttail" "") label (reg-args-to-call-list reg-args)
			     (fun-debug-loc-id fun)))
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
    ((global-set! ',var ,val)
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
       ;; Result *should be* unused, but fix-letrec pass uses it in complex??
       res))
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
     (add-foreign-call name (length args))
     (let* ((id (next-id))
	    (argstr (join ", " (omap arg args (format "i64 ~a" (emit arg env fun #f))))))
       (push-instr! fun (format "%v~a = call ~a i64 @\"~a\"(~a) #0, !dbg !~a"
				id
				(if (string=? "SCM_CALLCC" (substring name 0 (min (string-length name) 10)))
				    "preserve_nonecc" "")
				name argstr (fun-debug-loc-id fun)))
       (finish (format "%v~a" id))))
    ;; TODO merge with foreign call above?
    ((primcall ,var ,vals ___)
     (let* ((vals (omap val vals (emit val env fun #f)))
	    (arglist (join ", " (omap val vals (format "i64 ~a" val))))
	    (id (next-id)))
       (add-foreign-call (format "SCM_~a" var) (length vals))
       (push-instr! fun (format "%v~a = call i64 @\"SCM_~a\"(~a), !dbg !~a"
				id var arglist (fun-debug-loc-id fun)))
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
	 (unless (or tail (not t-res))
	   (push-instr! fun (format "br label %~a" join)))
	 (push-instr! fun (format "~a:" false))
	 (fun-last-label-set! fun false)
	 (let* ((f-res (emit c env fun tail))
		(f-last-label (fun-last-label fun)))
	   (unless (or tail (not f-res))
	     (push-instr! fun (format "br label %~a" join)))
	   (unless (or tail (not (or f-res t-res)))
	     (push-instr! fun (format "~a:" join))
	     (fun-last-label-set! fun join)
	     (if (and f-res t-res)
		 (begin
		   (push-instr! fun (format "%v~a = phi i64 [~a, %~a], [~a, %~a]"
					    join-id t-res t-last-label f-res f-last-label))
		   (format "%v~a" join-id))
		 (or f-res t-res)))))))
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
	    (lfun (cond
		   ((assq label env) => cdr)
		   (else (hash-table-ref global-labels label))))
	    (case-label (find-label-for-case lfun (length args) label)))
       ;; TODO: varargs inline calls
       (when (equal? case-label label)
	 (set! case-label (fun-to-label lfun label))
	 (push-instr! fun (format "store i64 ~a, ptr @argcnt" (length args))))
       (finish (emit-call fun tail (format "@\"~a\"" case-label) args))))
    ((let ((,vars ,vals) ___) ,body)
     (let ((args (omap val vals (emit val env fun #f))))
       (emit body (append (map cons vars args) env) fun tail)))
    ((closure (label ,label) ,args ___)
     (let* ((args (omap arg args (emit arg env fun #f)))
	    (id (next-id))
	    (label (fun-to-label (emit label env fun #f) label)))
       (push-instr! fun ( format "%v~a = call i64 @SCM_CLOSURE(i64 ptrtoint (ptr @\"~a\" to i64), i64 ~a), !dbg !~a"
			  id label (length args) (fun-debug-loc-id fun)))
       
       (for (arg i) (args (iota (length args)))
	    (push-instr! fun (format "call void @SCM_CLOSURE_SET_FAST(i64 %v~a, i64 ~a, i64 ~a), !dbg !~a"
				     id arg i (fun-debug-loc-id fun))))
       (format "%v~a" id)))
    ((const-closure (label ,label))
     ;; Look up the constant label.
     (let ((label (fun-to-label (emit label env fun #f) label)))
       (finish (emit-const `($const-closure ,label)))))
    ((labels ((,vars ,lambdas) ___) ,body)
     (let ((funs (map-in-order
		  (lambda (id)
		    (let ((fun (make-fun 1)))
		      (push! functions fun)
		      fun))
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
       (emit body (cons (cons name loop) (append (map cons vars phi-ids) env)) fun tail)))
    ((lookup ,var)
     (let ((sym (emit-const var))
	   (id (next-id)))
       (push-instr! fun (format "%v~a = call i64 @SCM_LOAD_GLOBAL(i64 ~a), !dbg !~a" id sym (fun-debug-loc-id fun)))
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
  (and (number? c)  (integer? c) 
       (< (abs c) #x1fffffffffffffff)))

(define (escape-quotes str)
  (list->string 
   (apply append 
          (map (lambda (ch) 
                 (if (char=? ch #\")
                     (string->list "\\22")
                     (list ch)))
               (string->list str)))))

(define (add-const c)
  (cond
   ((symbol? c)
    (let ((str (emit-const (symbol->string c)))
	  (sym-name (string-append "SYM-" (escape-quotes (symbol->string c)))))
      (push! consts (format "@\"~a\" = private unnamed_addr global {i64, i64, i64} {i64 ~a, i64 ~a, i64 ~a}, align 8\n"
			    sym-name symbol-tag str undefined-tag))
      (format "add (i64 ~a, i64 ptrtoint ({i64, i64, i64}* @\"~a\" to i64))"
	      ptr-tag sym-name)))
   ((flonum? c)
    (let* ((u (get-double-as-u64 c))
	   (urot (bit-field-rotate u 4 0 64))
	   (plus (+ urot 1))
	   (low (bitwise-and urot 7)))
      (if (memv low '(0 3 4))
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
    (let* ((id (next-id))
	   (bv (string->utf8 c))
	   (bytes (bytevector-length bv))
	   (len (string-length c)))
      (push! consts (format "@strdata~a = private unnamed_addr constant [~a x i8] [~a]"
			    id bytes (join ", " (omap num (bytevector->list bv) (format "i8 ~a" num)))))
      (push! consts (format "@str~a = private unnamed_addr constant {i64, i64, i64, ptr} {i64 ~a, i64 ~a, i64 ~a, ptr @strdata~a}, align 8\n"
			    id  string-tag (* 8 len) (* 8 bytes)  id ))
      (format "add (i64 ~a, i64 ptrtoint ({i64, i64, ptr}* @str~a to i64))"
	      ptr-tag  id)))
   ;; TODO use a record
   ((and (pair? c) (eq? '$const-closure (car c)))
    (let ((id (next-id)))
      (push! consts (format "@clo~a = private unnamed_addr constant [2 x i64] [i64 ~a, i64 ptrtoint (ptr @\"~a\" to i64)], align 8"
			    id closure-tag (second c)))
      (format "add (i64 ~a, i64 ptrtoint ([2 x i64]* @clo~a to i64))"
	      ptr-tag id)))
   ((vector? c)
    ;; Prepend the length & nullptr for slab ptr
    (let* ((vals (cons (* 8 (vector-length c))
		       (omap val (vector->list c) (emit-const val))))
	   (val-str (join ", " (omap val vals (format "i64 ~a" val))))
	   (id (next-id)))
      (push! consts (format "@vec~a = private unnamed_addr constant [~a x i64] [~a], align 8\n"
			    id (+ 1 (vector-length c)) val-str ))
      (format "add (i64 ~a, i64 ptrtoint (ptr @vec~a to i64))"
	      vector-tag  id)))
   ((pair? c)
    (let* ((id (next-id))
	   (a (emit-const (car c)))
	   (b (emit-const (cdr c))))
      (push! consts (format "@cons~a = private unnamed_addr constant {i64, i64} {i64 ~a, i64 ~a}, align 8"
			    id a b))
      (format "add (i64 ~a, i64 ptrtoint ({i64, i64}* @cons~a to i64))"
	      cons-tag id)))
   ((bytevector? c)
    (let* ((vals (omap val (bytevector->list c) (format "i8 ~a" val)))
	   (len (bytevector-length c))
	   (val-str (join ", "  vals))
	   (id (next-id)))
      (push! consts (format "@bvec~a = private unnamed_addr constant {i64, i64, [~a x i8]} {i64 ~a, i64 ~a, [~a x i8] [~a]}, align 8\n"
			    id len bytevector-tag (* 8 len) len val-str ))
      (format "add (i64 ~a, i64 ptrtoint (ptr @bvec~a to i64))"
	      ptr-tag id)))
   (else (error "Unknown Const: " c))))

(define (emit-header)
  (display (format "target triple = \"~a\"\n\n" platform-target-triple))
  (display "
declare void @SCM_CLOSURE_SET (i64, i64, i64)
declare void @SCM_CLOSURE_SET_FAST (i64, i64, i64)
declare ptr @SCM_LOAD_CLOSURE_PTR(i64)
declare void @SCM_ARGCNT_FAIL ()
declare void @SCM_WRITE_SHADOW_STACK (i64, i64)
declare void @SCM_READ_SHADOW_STACK (i64)
declare void @SCM_SET_GLOBAL (i64, i64)
declare void @SCM_LOAD_GLOBAL (i64)
declare i64 @SCM_CLOSURE (i64, i64)
declare preserve_nonecc i64 @SCM_CALLCC (i64)  #0
declare preserve_nonecc i64 @SCM_CALLCC_ONESHOT (i64)  #0
declare void @consargs_stub ()

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

(define (get-compile-path)
  (let ((path (get-exe-path)))
    (string-append (if path path ".") "/../lib/callcc/")))

(let ((path (get-compile-path)))
  (set! library-search-paths (cons (string-append path "lib/srfi2") library-search-paths))
  (set! library-search-paths (cons (string-append path "compiler/headers") library-search-paths))
  (set! library-search-paths (cons (string-append path "lib") library-search-paths))
  (set! library-search-paths (cons (string-append path "compiler") library-search-paths)))

(define (compile file verbose include-eval)
  (set! functions '())
  (let* ((path (get-compile-path))
	 (libman (make-libman path)))
    (expander-init libman)
    
    (let* ((runtime-input (with-input-from-file (string-append path "lib/runtime.scm") read-file))
	   (eval-input (with-input-from-file (string-append path "lib/eval.scm") read-file))
	   (pre-input (with-input-from-file file read-file))

	   ;; Auto-add 'import' to scripts.
	   (input
	    (match pre-input
	      (((import ,anything ___) ,body ___) pre-input)
	      (,else `((import  (scheme base) (scheme r5rs) (scheme time) (scheme file) (scheme inexact) (scheme complex)) ,@pre-input))))
	   (runtime   (expand-program runtime-input "" libman)
		    )
	   (evals   (if include-eval (expand-program eval-input "" libman) '())
		  )
	   (prog (expand-program input "PROG-" libman))
	   (eval-and-macros (if include-eval (append evals (serialize-libraries libman)) '()))
	   (lowered (r7-pass `(begin  	,@runtime
					,@eval-and-macros
					,@prog
					(exit 0) ;; default return value.
					)))
	   (main-fun (make-fun "SCM_MAIN")))
      (when verbose
	(display (format "Compiling ~a\n" file) (current-error-port))
	(flush-output-port (current-error-port)))
      (emit lowered '() main-fun #t)
      (emit-header)
      (for foreign-func (hash-table->alist foreign-funcs)
	   (display (format "declare i64 @~a(~a)\n" (car foreign-func)
			    (join ", " (make-list (cdr foreign-func) "i64")))))

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
	     
	     (display (format "define ~a i64 @\"~a\"(~a) ~a #0 !dbg !~a {\n" 
			      (if (member (fun-name func) '("SCM_MAIN" "S_error"))
				  "" "internal")
			      (fun-name func)
			      (reg-args-to-param-list reg-args)
			      (if (equal? (fun-name func) "S_error")
				  "noreturn" "")
			      (fun-debug-id func)))
	     (display (format " entry:\n"))
	     (for (arg i) (stack-args (iota (length stack-args)))
		  (display (format "  ~a = call i64 @SCM_READ_SHADOW_STACK(i64 ~a), !dbg !~a\n"
				   arg i (fun-debug-loc-id func))))
	     (for line (fun-code func)
		  (if (loop-var? line)
		      (for phi (loop-var-phis line)
			   (display (format "  ~a\n" phi)))
		      (display (format "  ~a\n" line))))
	     (display "}\n")))))
  (for line (reverse! debug-strings)
       (display line) (newline)))

(define (get-link-command output output-file opts)
  (let ((link (format "~a --target=~a ~a -g -o ~a ~a ~alibcallcc.a ~a"
		      platform-cc platform-target-triple opts output-file output (get-compile-path) platform-link-opts)))
    (display (format "Running link: ~a\n" link) (current-error-port))
    (flush-output-port (current-error-port))
    link))
