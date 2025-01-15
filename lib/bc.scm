(include "qq.scm")
(include "memory_layout.scm")
(include "opcodes.scm")

(define next-id
  (let ((id 0))
    (lambda ()
      (let ((res id))
	(set! id (+ id 1))
	res))))

(define-record-type fun (%make-fun code name) fun?
		    (code fun-code fun-code-set!)
		    (name fun-name fun-name-set!))

(define (make-fun name)
  (%make-fun '() name))

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

(define-record-type loop-var (make-loop-var dest rg) loop-var?
		    (dest loop-var-dest)
		    (rg loop-var-rg))

(define (abort str)
  (display "TODO:")
  (display str)
  (newline)
  (exit))

(define last-label #f)

(define (emit sexp env fun tail)
  (define (finish res)
    (if tail
	(begin
	  (when res (push-instr! fun (format "ret i64 ~a" res)))
	  #f)
	res))
  (match sexp
    ((begin ,sexps ___ ,end)
     (for sexp sexps
	  (emit sexp env fun #f))
     (emit end env fun tail))
    ((set! ,var ,val)
     (abort "set!")
     (finish rg))
    ((define ,var ,val)
     (abort "define")
     (finish rg))
    ((primcall closure-ref ,var ,cnt)
     (abort "closure-ref")
     (finish rg))
    ((primcall closure-set! ,var ,cnt ,res)
     (abort "closure-set!")
     (finish rg))
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
       (push-instr! fun (format "%v~a = call i64 @~a(~a)" id name argstr))
       (finish (format "%v~a" id))))
    ((primcall ,var ,vals ___)
     (abort 'primcall)
     (finish rg))
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
       (set! last-label true)
       (let ((t-res (emit b env fun tail)))
	 (if (or tail (not t-res))
	     (finish t-res)
	     (push-instr! fun (format "br label %~a" join)))
	 (push-instr! fun (format "~a:" false))
	 (set! last-label false)
	 (let ((f-res (emit c env fun tail)))
	   (if (or tail (not f-res))
	       (finish f-res)
	       (push-instr! fun (format "br label %~a" join)))
	   (unless tail
	     (push-instr! fun (format "~a:" join))
	     (set! last-label join)
	     (push-instr! fun (format "%v~a = phi i64 [~a, %~a], [~a, %~a]"
				  join-id t-res true f-res false)))))
       (finish (format "%v~a" join-id))))
    ((call ,loop-var ,args ___)
     (guard (and (assq loop-var env) (loop-var? (cdr (assq loop-var env)))))
     (abort 'loop-call))
    ((call ,args ___)
     (abort 'call))
    ((label-call ,label ,args ___)
     (abort 'label-call))
    ((let ((,vars ,vals) ___) ,body)
     (abort 'let))
    ((closure (label ,label) ,args ___)
     (abort 'closure))
    ((const-closure (label ,label))
     (abort 'const-closure))
    ((labels ((,vars ,lambdas) ___) ,body)
     (abort 'labels)
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
		   (fun-name-set! func-p name)
		   (for case cases
			(define argcnt (length (to-proper (second case))))
			(define v-type (list? (second case)))
			(define last (eq? case last-case))
			(when last-case-jmp
			  (list-set! last-case-jmp 2
				     (- (length (fun-code func-p)) (list-ref last-case-jmp 2))))
			(push-instr! func-p (list (if v-type (if last 'FUNC 'CFUNC) (if last 'FUNCV 'CFUNCV)) argcnt))
			(unless last
			  (let ((jmp `(JMP 0 ,(length (fun-code func-p)))))
			    (set! last-case-jmp jmp)
			    (push-instr! func-p jmp)))
			(emit (third case)
			      (append (map cons (to-proper (second case)) (iota argcnt))
				      env)
			      func-p
			      #t)))
		 funs lambdas vars label-ids)
       (let ((res (emit body (append  env) fun tail)))
	 (if res
	     (begin
	       (unless (check-need-arg res rg)
		 (push-instr! fun (list 'ARG rg res)))
	       rg)
	     #f))))
    (,var
     ;; Only lookup, doesn't gen code.
     (guard (symbol? var))
     (finish
      (cond
       ((assq var env) => cdr)
       (else
	(error "Unknown sym:" var)))))
    ((loop  ,vars ,name ,body ,inits ___)
     (abort 'loop))
    ((lookup ,var)
     (abort 'lookup)
     (finish rg))
    (,const
     (guard (not (pair? const)))
     (emit-const const))
    ((quote ,const)
     (emit-const const))
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
    (abort 'const-clo)
    (write-u32 closure-tag p) (advance 4)
    (write-u32 0 p) (advance 4)
    (write-u64 (* 8 1) p) (advance 8)
    (write-u64 (fun-bc-start (list-ref functions (cdr c))) p) (advance 8)
    (+ closure-tag start-pos))
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
   (else (abort 'Unknown-const))))

(define (emit-header)
  (display
   "target triple = \"x86_64-pc-linux-gnu\"\n
declare i64 @display (i64)
"))

(define (compile file verbose)
  (set! functions '())
  (let* ((libman (make-libman))
	 (runtime-input (with-input-from-file "runtime.scm" read-file))
	 (pre-input (with-input-from-file file read-file))
	 ;; Auto-add 'import' to scripts.
	 (input
	  (match pre-input
	    (((import ,anything ___) ,body ___) pre-input)
	    (,else `((import (scheme base) (scheme r5rs) (scheme time) (scheme file)) ,@pre-input))))
	 (unused (expander-init libman))
	 (runtime (expand-program runtime-input "" libman))
	 (prog (expand-program input "PROG-" libman))
	 (lowered (r7-pass `(begin	;,@runtime
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
	 (display (format "define i64 @~a() {\n" (fun-name func)))
	 (for line (fun-code func)
	      (display "  ") (display line) (newline))
	 (display "}\n"))
    ))

(for file (cdr (command-line))
     (compile file #t))

