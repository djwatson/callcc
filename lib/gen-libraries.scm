(import (scheme base) (scheme write) (third-party pp) (scheme file) (srfi 1) (util))

;; List of primitives that the code generator knows about, and will emit
;; raw asm for.
(define sys-primitives
  '(
    ADD SUB DIV MUL MOD
    LT GT EQ LTE GTE NUM_EQ
    EQ
    GUARD
    ;; Objects (64-bits).
    LOAD
    STORE
    ;; U32's.
    LOAD_CHAR
    STORE_CHAR
    ALLOC
    INTEGER_CHAR
    CHAR_INTEGER
    INEXACT
    EXACT
    FOREIGN_CALL
    APPLY
    CALLCC
    CALLCC_RESUME
    ;; TODO remove, only for debugging.
    WRITE 
    ))
;; Standard macros, defined below.
(define standard-macros
  '(
    syntax-rules
    define
    qq-done
    qq-make-vector
    qq-do-pair
    qq-finish-pair
    qq-finish-splice
    qq-help
    quasiquote
    let
    let*
    letrec-syntax
    let-values
    let*-values
    define-values
    letrec*
    letrec
    do
    case
    cond
    and
    or
    when
    unless
    guard
    guard-aux
    parameterize
    define-record-type
    define-record-field
    delay-force
    delay
    with-syntax
    quasisyntax

    ;; Undef?
    unquote-splicing
    unquote
    ...

    ;; Other
    lambda
    case-lambda
    begin
    if
    quote
    define-syntax
    let-syntax
    syntax
    syntax-case
    with-ellipsis
    def
    set!
    include
    cond-expand
    syntax->datum
    ))

;; Standard libraries, with paths.
(define libraries
  '(
    (r5rs (scheme r5rs))
    (base (scheme base))
    (case-lambda (scheme case-lambda))
    (char (scheme char))
    (complex (scheme complex))
    (cxr (scheme cxr))
    (process-context (scheme process-context))
    (eval (scheme eval))
    (file (scheme file))
    (inexact (scheme inexact))
    (lazy (scheme lazy))
    (load (scheme load))
    (read (scheme read))
    (repl (scheme repl))
    ;(hash (srfi |69|))
    (time (scheme time))
    (write (scheme write))
    ;; TODO flow instead
    (syntax-case (scheme syntax-case))))

;; Mapping of standard identifiers -> library name
(define identifier->library
  '(
    (* base r5rs)
    (+ base r5rs)
    (- base r5rs)
    (... base)
    (/ base r5rs)
    (< base r5rs)
    (<= base r5rs)
    (= base r5rs)
    (=> base r5rs)
    (> base r5rs)
    (>= base r5rs)
    (abs base r5rs)
    (and base r5rs)
    (append base r5rs)
    (apply base r5rs)
    (assoc base r5rs)
    (assq base r5rs)
    (assv base r5rs)
    (begin base r5rs)
    (binary-port? base)
    (boolean=? base)
    (boolean? base r5rs)
    (bytevector base)
    (bytevector-append base)
    (bytevector-copy base)
    (bytevector-copy! base)
    (bytevector-length base)
    (bytevector-u8-ref base)
    (bytevector-u8-set! base)
    (bytevector? base)
    (caar base r5rs)
    (cadr base r5rs)
    (call-with-current-continuation base r5rs)
    (call-with-port base)
    (call-with-values base r5rs)
    (call/cc base)
    (car base r5rs)
    (case base r5rs)
    (cdar base r5rs)
    (cddr base r5rs)
    (cdr base r5rs)
    (ceiling base r5rs)
    (char->integer base r5rs)
    (char-ready? base r5rs)
    (char? base r5rs)
    (close-input-port base r5rs)
    (close-output-port base r5rs)
    (close-port base)
    (complex? base r5rs)
    (cond base r5rs)
    (cond-expand base)
    (cons base r5rs)
    (current-error-port base)
    (current-input-port base r5rs)
    (current-output-port base r5rs)
    (define base r5rs)
    (define-record-type base)
    (define-syntax base r5rs)
    (define-values base)
    (denominator base r5rs)
    (do base r5rs)
    (dynamic-wind base r5rs)
    (else base r5rs)
    (eof-object base)
    (eof-object? base r5rs)
    (eq? base r5rs)
    (equal? base r5rs)
    (eqv? base r5rs)
    (error base)
    (error-object-irritants base)
    (error-object-type base)
    (error-object-message base)
    (error-object? base)
    (even? base r5rs)
    (exact base)
    (exact-integer-sqrt base)
    (exact-integer? base)
    (exact? base r5rs)
    (expt base)
    (features base)
    (file-error? base)
    (floor base)
    (floor-quotient base)
    (floor-remainder base)
    (floor/ base)
    (flush-output-port base)
    (for-each base r5rs)
    (gcd base r5rs)
    (get-output-bytevector base)
    (get-output-string base)
    (guard base)
    (if base r5rs)
    (include base)
    (include-ci base)
    (inexact base)
    (inexact? base r5rs)
    (input-port-open? base)
    (input-port? base r5rs)
    (integer->char base r5rs)
    (integer? base r5rs)
    (lambda base r5rs)
    (lcm base r5rs)
    (length base r5rs)
    (let base r5rs)
    (let* base r5rs)
    (let*-values base)
    (let-syntax base r5rs)
    (let-values base)
    (letrec base r5rs)
    (letrec* base)
    (letrec-syntax base r5rs)
    (list base r5rs)
    (list->string base r5rs)
    (list->vector base r5rs)
    (list-copy base)
    (list-ref base r5rs)
    (list-set! base)
    (list-tail base r5rs)
    (list? base r5rs)
    (make-bytevector base)
    (make-list base)
    (make-parameter base)
    (make-string base r5rs)
    (make-vector base r5rs)
    (map base r5rs)
    (max base r5rs)
    (member base r5rs)
    (memq base r5rs)
    (memv base r5rs)
    (min base r5rs)
    (modulo base r5rs)
    (negative? base r5rs)
    (newline base r5rs)
    (not base r5rs)
    (null? base r5rs)
    (number->string base r5rs)
    (number? base r5rs)
    (numerator base r5rs)
    (odd? base r5rs)
    (open-input-bytevector base)
    (open-input-string base)
    (open-output-bytevector base)
    (open-output-string base)
    (or base r5rs)
    (output-port-open? base)
    (output-port? base r5rs)
    (pair? base r5rs)
    (parameterize base)
    (peek-char base r5rs)
    (peek-u8 base)
    (port? base)
    (positive? base r5rs)
    (procedure? base r5rs)
    (quasiquote base r5rs)
    (quote base r5rs)
    (quotient base r5rs)
    (raise base)
    (raise-continuable base)
    (rational? base r5rs)
    (rationalize base r5rs)
    (read-bytevector base)
    (read-bytevector! base)
    (read-char base r5rs)
    (read-error? base)
    (read-line base)
    (read-string base)
    (read-u8 base)
    (real? base r5rs)
    (remainder base r5rs)
    (reverse base r5rs)
    (round base r5rs)
    (set! base r5rs)
    (set-car! base r5rs)
    (set-cdr! base r5rs)
    (square base)
    (string base r5rs)
    (string->list base r5rs)
    (string->number base r5rs)
    (string->symbol base r5rs)
    (string->utf8 base)
    (string->vector base)
    (string-append base r5rs)
    (string-copy base r5rs)
    (string-copy! base)
    (string-fill! base r5rs)
    (string-for-each base)
    (string-length base r5rs)
    (string-map base)
    (string-ref base r5rs)
    (string-set! base r5rs)
    (string<=? base r5rs)
    (string<? base r5rs)
    (string=? base r5rs)
    (string>=? base r5rs)
    (string>? base r5rs)
    (string? base r5rs)
    (substring base r5rs)
    (char=? base r5rs)
    (char<? base r5rs)
    (char<=? base r5rs)
    (char>? base r5rs)
    (char>=? base r5rs)
    (symbol->string base r5rs)
    (symbol=? base)
    (symbol? base r5rs)
    (syntax-error base)
    (syntax-rules base r5rs) ;; Not in r5rs in original report?
    (textual-port? base)
    (truncate base r5rs)
    (truncate-quotient base)
    (truncate-remainder base)
    (truncate/ base)
    (u8-ready? base)
    (unless base)
    (unquote base)
    (unquote-splicing base)
    (utf8->string base)
    (values base r5rs)
    (vector base r5rs)
    (vector->list base r5rs)
    (vector->string base)
    (vector-append base)
    (vector-copy base)
    (vector-copy! base)
    (vector-fill! base r5rs)
    (vector-for-each base)
    (vector-length base r5rs)
    (vector-map base)
    (vector-ref base r5rs)
    (vector-set! base r5rs)
    (vector? base r5rs)
    (when base)
    (with-exception-handler base)
    (write-bytevector base)
    (write-char base r5rs)
    (write-string base)
    (write-u8 base)
    (zero? base r5rs)

    (case-lambda case-lambda)

    (char-alphabetic? char r5rs)
    (char-ci<=? char r5rs)
    (char-ci<? char r5rs)
    (char-ci=? char r5rs)
    (char-ci>=? char r5rs)
    (char-ci>? char r5rs)
    (char-downcase char r5rs)
    (char-foldcase char)
    (char-lower-case? char r5rs)
    (char-numeric? char r5rs)
    (char-upcase char r5rs)
    (char-upper-case? char r5rs)
    (char-whitespace? char r5rs)
    (digit-value char)
    (string-ci<=? char r5rs)
    (string-ci<? char r5rs)
    (string-ci=? char r5rs)
    (string-ci>=? char r5rs)
    (string-ci>? char r5rs)
    (string-downcase char)
    (string-foldcase char)
    (string-upcase char)

    (angle complex r5rs)
    (magnitude complex r5rs)
    (imag-part complex)
    (make-polar complex r5rs)
    (make-rectangular complex r5rs)
    (real-part complex r5rs)

    (caaaar cxr r5rs)
    (caaadr cxr r5rs)
    (caaar cxr r5rs)
    (caadar cxr r5rs)
    (caaddr cxr r5rs)
    (caadr cxr r5rs)
    (cadaar cxr r5rs)
    (cadadr cxr r5rs)
    (cadar cxr r5rs)
    (caddar cxr r5rs)
    (cadddr cxr r5rs)
    (caddr cxr r5rs)
    (cdaaar cxr r5rs)
    (cdaadr cxr r5rs)
    (cdaar cxr r5rs)
    (cdadar cxr r5rs)
    (cdaddr cxr r5rs)
    (cdadr cxr r5rs)
    (cddaar cxr r5rs)
    (cddadr cxr r5rs)
    (cddar cxr r5rs)
    (cdddar cxr r5rs)
    (cddddr cxr r5rs)
    (cdddr cxr r5rs)

    (eval eval r5rs)
    (environment eval)

    (call-with-input-file file r5rs)
    (call-with-output-file file r5rs)
    (delete-file file)
    (file-exists? file)
    (open-binary-input-file file)
    (open-binary-output-file file)
    (open-input-file file r5rs)
    (open-output-file file r5rs)
    (with-input-from-file file r5rs)
    (with-output-to-file file r5rs)

    (acos inexact r5rs)
    (asin inexact r5rs)
    (atan inexact r5rs)
    (cos inexact r5rs)
    (exp inexact)
    (finite? inexact)
    (infinite? inexact)
    (log inexact r5rs)
    (nan? inexact)
    (sin inexact r5rs)
    (sqrt inexact r5rs)
    (tan inexact r5rs)

    (delay lazy r5rs)
    (delay-force lazy)
    (force lazy r5rs)
    (make-promise lazy)
    (promise? lazy)

    (load load r5rs)

    (command-line process-context)
    (emergency-exit process-context)
    (exit process-context)
    (get-environment-variable process-context)
    (get-environment-variables process-context)

    (read read r5rs)
    
    (interaction-environment repl r5rs)

    (current-jiffy time)
    (current-second time)
    (jiffies-per-second time)

    (display write r5rs)
    (write write r5rs)
    (write-shared write)
    (write-simple write)

    ;;r5rs
    (exact->inexact r5rs)
    (inexact->exact r5rs)
    (null-environment r5rs)
    (scheme-report-environment r5rs)

    ;; syntax-case
    (bound-identifier=? syntax-case)
    (with-syntax syntax-case)
    (syntax-case syntax-case)
    (syntax syntax-case)
    (identifier? syntax-case)
    (free-identifier=? syntax-case)
    (datum->syntax syntax-case)
    (syntax->datum syntax-case)
    (quasisyntax syntax-case)
    (generate-temporaries syntax-case)

    ;; hash
    ;; (make-hash-table hash)
    ;; (hash-table? hash)
    ;; (alist->hash-table hash)
    ;; (hash-table-equivalence-function hash)
    ;; (hash-table-hash-function hash)
    ;; (hash-table-ref hash)
    ;; (hash-table-ref/default hash)
    ;; (hash-table-set! hash)
    ;; (hash-table-delete! hash)
    ;; (hash-table-exists? hash)
    ;; (hash-table-update! hash)
    ;; (hash-table-update!/default hash)
    ;; (hash-table-size hash)
    ;; (hash-table-keys hash)
    ;; (hash-table-values hash)
    ;; (hash-table-walk hash)
    ;; (hash-table-fold hash)
    ;; (hash-table->alist hash)
    ;; (hash-table-copy hash)
    ;; (hash-table-merge! hash)
    ;; (hash hash)
    ;; (string-hash hash)
    ;; (string-ci-hash hash)
    ;; (hash-by-identity hash)

    ;; Internal only - identifiers from expanded macros.
    (make-record-type int) ;; records
    (record? int)
    (record-ref int)
    (record-set! int)
    (record-accessor int)
    (record-constructor int)
    (cons* int) ;; quasiquote
    (%make-promise int) ;; lazy promise
    (make-ident int) ;; syntax expansion, hygienic macros
    (add-feature int) ;; (feature) identifiers.
    )
  )

;; Definitions of the standard macros, in syntax-case or syntax-rules.
(define macros
  '(
    ;; Base
    (define-syntax syntax-rules
      (lambda (x)
	(syntax-case x ()
	  ((_ (k ...) ((keyword . pattern) template) ...)
	   (syntax
	    (lambda (x)
	      (syntax-case x (k ...)
		((_ . pattern) (syntax template))
		...))))
	  ((_ dots (k ...) ((keyword . pattern) template) ...)
	   (syntax
	    (lambda (x)
	      (with-ellipsis dots
			     (syntax-case x (k ...)
			       ((_ . pattern) (syntax template))
			       ...))))))))

    (define-syntax define
      (syntax-rules ()
	((_ (name . args) . body)
	 (def name (lambda args . body)))
	((_ name expr)
	 (def name expr))))

    ;; Proper support for quasiquote is a pain:
    ;; we must
    ;; 1) Support normal quasiquote, unquote, unquote-splicing.
    ;; 2) Support *nested* quasiquote
    ;; 3) Support the nested cases that result in multiple values in unquote, e.g.
    ;;      (unquote 1 2 3) or (unquote-splicing (1 2 3) (4 5 6))
    ;; 4) Ensure any literal tail portions remain literal, and don't result in
    ;;    new, freshly allocated structure at runtime (required per the standard).
    ;;
    ;; And we have to do all this in syntax-rules.

    ;; In order not to explode the output, we merge cons in to either list
    ;; or cons* (depending on if it is improper or not).  This is just an
    ;; optimization, and is correct either way.

    (define-syntax qq-done
      (syntax-rules ()
	((_ arg) arg)))
    (define-syntax qq-make-vector
      (syntax-rules ()
	((_ '(args ...) (s sargs ...) )
	 (s '#(args ...) sargs ...))
	((_ args (s sargs ...) )
	 (s (list->vector args) sargs ...))))
    (define-syntax qq-do-pair
      (syntax-rules ()
	((_ x-done lvl y-not-done s)
	 (qq-help (qq-finish-pair x-done s) lvl y-not-done))))
    (define-syntax qq-finish-pair
      (syntax-rules (quote cons cons* list)
	;; Required by r6rs/r7rs: "Portions that do not
	;; need to be rebuilt are always literal"
	((_ 'y-done 'x-done (s sargs ...))
	 (s '(x-done . y-done) sargs ...))
	;; Optimization only
	((_ '() x-done (s sargs ...))
	 (s (list x-done)  sargs ...))
	((_ (list y ...) x-done (s sargs ...))
	 (s (list x-done y ...)  sargs ...))
	((_ (cons* y1 ...) x-done (s sargs ...))
	 (s (cons* x-done y1 ...) sargs ...))
	((_ (cons y1 y2) x-done (s sargs ...))
	 (s (cons* x-done y1 y2) sargs ...))
	;; end optimization
	((_ y-done x-done (s sargs ...))
	 (s (cons x-done y-done) sargs ...))))
    (define-syntax qq-finish-splice
      (syntax-rules (quote list )
        ;;; Optimizations
	((_ '() ((list 'arg ...) ...) (s sargs ...))
	 (s '(arg ... ...) sargs ...))
	((_ '() (splice) (s sargs ...))
	 (s splice sargs ...))
	((_ '() ('splice ...) (s sargs ...))
	 (s '(splice ...) sargs ... ))
	((_ '() (splice ...) (s sargs ...))
	 (s (append splice ...) sargs ...))
	((_ 'y ((list 'arg ...) ...) (s sargs ...))
	 (s '(arg ... ... y) sargs ...))
	((_ 'y ('splice ...) (s sargs ...))
	 (s '(splice ...  y) sargs ...))
	;; end optimization
	((_ y (splice ...) (s sargs ...))
	 (s (append splice ...  y) sargs ...))
	))
    (define-syntax qq-help
      (syntax-rules (quote quasiquote unquote unquote-splicing)
	;; Nested quasiquote
	((_ s lvl `x)
	 (qq-help (qq-finish-pair 'quasiquote s) (lvl) (x)))
	;; Unquote with multiple args -> similar to unquote splicing.
	;; lvl 0 only.
	((_ s #t ((unquote x ...) . y))
	 (qq-help (qq-finish-splice  ((list x ...)) s) #t y))
	;; Unquote splicing with multiple args -> (append args ...)
	((_ s #t ((unquote-splicing x ...) . y))
	 (qq-help (qq-finish-splice (x ...) s) #t y))
	;; Unquote splicing
	((_ s #t ,@x)
	 (syntax-error "Invalid depth for unquote splicing:" x))
	((_ s (lvl) ,@x)
	 (qq-help (qq-finish-pair 'unquote-splicing s) lvl (x)))
	;; Vector
	((_ s lvl #(vec ...))
	 (qq-help (qq-make-vector s) lvl (vec ...)))
	;; Unquote.
	((_ (s sargs ...) #t ,arg)
	 (s arg sargs ...))
	((_ s (lvl) ,arg)
	 (qq-help (qq-finish-pair 'unquote s) lvl (arg)))
	;; Dotted pair, recurse.
	((_ s lvl (x . y))
	 (qq-help (qq-do-pair lvl y s) lvl x))
	;; Normal literal, quote it.
	((_ (s sargs ...) lvl x)
	 (s 'x sargs ...))))
    (define-syntax quasiquote
      (syntax-rules ()
	((_ args ...)
	 (qq-help (qq-done) #t args ...))))

    (define-syntax let
      (syntax-rules ()
	((_ ()  body) (let-syntax () body))
	((_ ()  body . rest) (let-syntax () (begin body . rest)))
	((_ ((var init) ...)  .  body) ((lambda (var ...)  .  body) init ...))
	((_ name ((var init) ...)  .  body)
	 ((letrec ((name (lambda (var ...)  .  body))) name) init ...))))
    (define-syntax let*
      (syntax-rules ()
	((_ ()  .  body) (let ()  .  body))
	((let* ((var init)  .  bindings)  .  body)
	 (let ((var init)) (let* bindings  .  body)))))
    (define-syntax letrec-syntax
      (syntax-rules ()
	((_ ((kw init) ...)  .  body)
	 (let () (define-syntax kw init) ... (let ()  .  body)))))
    (define-syntax let-values
      (syntax-rules ()
	((let-values (binding ...) body0 body1 ...)
	 (let-values "bind" (binding ...) () (begin body0 body1 ...)))
	((let-values "bind" () tmps body) (let tmps body))
	((let-values "bind" ((b0 e0) binding ...) tmps body)
	 (let-values "mktmp" b0 e0 () (binding ...) tmps body))
	((let-values "mktmp" () e0 args bindings tmps body)
	 (call-with-values
	     (lambda () e0)
	   (lambda args (let-values "bind" bindings tmps body))))
	((let-values "mktmp" (a  .  b) e0 (arg ...) bindings (tmp ...) body)
	 (let-values "mktmp" b e0 (arg ... x) bindings (tmp ... (a x)) body))
	((let-values "mktmp" a e0 (arg ...) bindings (tmp ...) body)
	 (call-with-values
	     (lambda () e0)
	   (lambda (arg ...  .  x)
	     (let-values "bind" bindings (tmp ... (a x)) body))))))
    (define-syntax let*-values
      (syntax-rules ()
	((let*-values () body0 body1 ...) (let () body0 body1 ...))
	((let*-values (binding0 binding1 ...) body0 body1 ...)
	 (let-values (binding0) (let*-values (binding1 ...) body0 body1 ...)))))
    (define-syntax define-values
      (syntax-rules ()
	((define-values () expr)
	 (define dummy (call-with-values (lambda () expr) (lambda args #f))))
	((define-values (var) expr) (define var expr))
	((define-values (var0 var1 ... varn) expr)
	 (begin
	   (define var0 (call-with-values (lambda () expr) list))
	   (define var1 (let ((v (cadr var0))) (set-cdr! var0 (cddr var0)) v))
	   ...
	   (define varn (let ((v (cadr var0))) (set! var0 (car var0)) v))))
	((define-values (var0 var1 ...  .  varn) expr)
	 (begin
	   (define var0 (call-with-values (lambda () expr) list))
	   (define var1 (let ((v (cadr var0))) (set-cdr! var0 (cddr var0)) v))
	   ...
	   (define varn (let ((v (cdr var0))) (set! var0 (car var0)) v))))
	((define-values var expr)
	 (define var (call-with-values (lambda () expr) list)))))
    (define-syntax letrec*
      (syntax-rules ()
	((_ ((var init) ...)  .  body)
	 (let () (define var init) ... (let ()  .  body)))))
    ;; TODO these could / should be different.
    ;; Note that chibi *does* interpret letrec and letrec*
    ;; with the fixing letrec (reloaded) algorithm, i.e.,
    ;; the r5rs_pitfalls first two tests should fail.
    (define-syntax letrec
      (syntax-rules ()
	((_ ((var init) ...)  .  body)
	 (let () (define var init) ... (let ()  .  body)))))
    (define-syntax do
      (syntax-rules ()
	((do ((var init step ...) ...)
	     (test expr ...)
	   command ...)
	 (let loop ((var init) ...)
	   (if test
	       (begin (if #f #f) expr ...)
	       (begin
		 command ...
		 (loop (do "step" var step ...) ...)))))
	((do "step" x) x)
	((do "step" x y) y)))
    (define-syntax case
      (syntax-rules (else =>)
	((case (key ...) clauses ...)
	 (let ((atom-key (key ...))) (case atom-key clauses ...)))
	((case key (else => result)) (result key))
	((case key (else result1 result2 ...)) (begin result1 result2 ...))
	((case key ((atoms ...) result1 result2 ...))
	 (if (memv key '(atoms ...))
	     (begin result1 result2 ...)))
	((case key ((atoms ...) => result))
	 (if (memv key '(atoms ...))
	     (result key)))
	((case key ((atoms ...) => result) clause clauses ...)
	 (if (memv key '(atoms ...))
	     (result key)
	     (case key clause clauses ...)))
	((case key ((atoms ...) result1 result2 ...) clause clauses ...)
	 (if (memv key '(atoms ...))
	     (begin result1 result2 ...)
	     (case key clause clauses ...)))))
    (define-syntax cond
      (syntax-rules (else =>)
	((cond (else result1 result2 ...)) (let () result1 result2 ...))
	((cond (test => result)) (let ((temp test)) (if temp (result temp))))
	((cond (test => result) clause1 clause2 ...)
	 (let ((temp test))
	   (if temp
	       (result temp)
	       (cond clause1 clause2 ...))))
	((cond (test)) test)
	((cond (test) clause1 clause2 ...)
	 (let ((temp test)) (if temp temp (cond clause1 clause2 ...))))
	((cond (test result1 result2 ...))
	 (if test
	     (begin result1 result2 ...)))
	((cond (test result1 result2 ...) clause1 clause2 ...)
	 (if test
	     (begin result1 result2 ...)
	     (cond clause1 clause2 ...)))))
    (define-syntax and
      (syntax-rules ()
	((_) #t)
	((_ test) (let () test))
	((_ test  .  tests) (if test (and  .  tests) #f))))
    (define-syntax or
      (syntax-rules ()
	((_) #f)
	((_ test) (let () test))
	((_ test  .  tests) (let ((x test)) (if x x (or  .  tests))))))
    (define-syntax when
      (syntax-rules () ((_ a body rest ...) (if a (begin body rest ...)))))
    (define-syntax unless
      (syntax-rules ()
	((_ a body rest ...) (if (not a) (begin body rest ...)))))
    (define-syntax guard
      (syntax-rules ()
	((guard (var clause ...) e1 e2 ...)
	 ((call/cc
	   (lambda (guard-k)
	     (with-exception-handler
		 (lambda (condition)
		   ((call/cc
		     (lambda (handler-k)
		       (guard-k
			(lambda ()
			  (let ((var condition))
			    (guard-aux
			     (handler-k
			      (lambda () (raise-continuable condition)))
			     clause
			     ...))))))))
	       (lambda ()
		 (call-with-values
		     (lambda () e1 e2 ...)
		   (lambda args (guard-k (lambda () (apply values args)))))))))))))
    (define-syntax guard-aux
      (syntax-rules (else =>)
	((guard-aux reraise (else result1 result2 ...))
	 (begin result1 result2 ...))
	((guard-aux reraise (test => result))
	 (let ((temp test)) (if temp (result temp) reraise)))
	((guard-aux reraise (test => result) clause1 clause2 ...)
	 (let ((temp test))
	   (if temp
	       (result temp)
	       (guard-aux reraise clause1 clause2 ...))))
	((guard-aux reraise (test)) (or test reraise))
	((guard-aux reraise (test) clause1 clause2 ...)
	 (let ((temp test))
	   (if temp
	       temp
	       (guard-aux reraise clause1 clause2 ...))))
	((guard-aux reraise (test result1 result2 ...))
	 (if test
	     (begin result1 result2 ...)
	     reraise))
	((guard-aux reraise (test result1 result2 ...) clause1 clause2 ...)
	 (if test
	     (begin result1 result2 ...)
	     (guard-aux reraise clause1 clause2 ...)))))

    (define-syntax parameterize
      (syntax-rules ()
	((parameterize ("step")
	   ((param value p old new) ...)
	   ()
	   body)
	 (let ((p param) ...)
	   (let ((old (p)) ...
		 (new value) ...)
	     (dynamic-wind
	       (lambda () (p new) ...)
	       (lambda () . body)
	       (lambda () (p old) ...)))))
	((parameterize ("step")
	   args
	   ((param value) . rest)
	   body)
	 (parameterize ("step")
	   ((param value p old new) . args)
	   rest
	   body))
	((parameterize ((param value) ...) . body)
	 (parameterize ("step")
	   ()
	   ((param value) ...)
	   body))))


;;; Lazy
    (define-syntax delay-force
      (syntax-rules ()
	((delay-force expression)
	 (%make-promise #f (lambda () expression)))))

    (define-syntax delay
      (syntax-rules ()
	((delay expression)
	 (delay-force (%make-promise #t expression)))))

    ;; Syntax-case
    (define-syntax with-syntax
      (lambda (x)
	(syntax-case x ()
	  ((_ ((p e0) ...) e1 e2 ...)
	   (syntax (syntax-case (list e0 ...) ()
		     ((p ...) (begin e1 e2 ...))))))))
    (define-syntax quasisyntax
      (lambda (e)

	;; Expand returns a list of the form
	;;    [template[t/e, ...] (replacement ...)]
	;; Here template[t/e ...] denotes the original template
	;; with unquoted expressions e replaced by fresh
	;; variables t, followed by the appropriate ellipses
	;; if e is also spliced.
	;; The second part of the return value is the list of
	;; replacements, each of the form (t e) if e is just
	;; unquoted, or ((t ...) e) if e is also spliced.
	;; This will be the list of bindings of the resulting
	;; with-syntax expression.

	(define (expand x level)
	  ;;(display "Expand ") (display x) (newline)
	  (syntax-case x (quasisyntax unsyntax unsyntax-splicing)
	    ((quasisyntax e)
	     (with-syntax (((k _)     x) ;; original identifier must be copied
			   ((e* reps) (expand (syntax e) (+ level 1))))
	       (syntax ((k e*) reps))))
	    ((unsyntax e)
	     (= level 0)
	     (with-syntax (((t) (generate-temporaries '(t))))
	       (syntax (t ((t e))))))
	    (((unsyntax e ...) . r)
	     (= level 0)
	     (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
			   ((t ...)        (generate-temporaries (syntax (e ...)))))
	       (syntax ((t ... . r*)
			((t e) ... rep ...)))))
	    (((unsyntax-splicing e ...) . r)
	     (= level 0)
	     (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
			   ((t ...)        (generate-temporaries (syntax (e ...)))))
	       (with-syntax ((((t ...) ...) (syntax ((t (... ...)) ...))))
		 (syntax ((t ... ... . r*)
			  (((t ...) e) ... rep ...))))))
	    ((k . r)
	     (and (> level 0)
		  (identifier? (syntax k))
		  (or (free-identifier=? (syntax k) (syntax unsyntax))
		      (free-identifier=? (syntax k) (syntax unsyntax-splicing))))
	     (with-syntax (((r* reps) (expand (syntax r) (- level 1))))
	       (syntax ((k . r*) reps))))
	    ((h . t)
	     (with-syntax (((h* (rep1 ...)) (expand (syntax h) level))
			   ((t* (rep2 ...)) (expand (syntax t) level)))
	       (syntax ((h* . t*)
			(rep1 ... rep2 ...)))))
	    (#(e ...)
	     (with-syntax ((((e* ...) reps)
			    (expand (vector->list (syntax #(e ...))) level)))
	       (syntax (#(e* ...) reps))))
	    (other
	     (syntax (other ())))))

	(syntax-case e ()
	  ((_ template)
	   (with-syntax (((template* replacements) (expand (syntax template) 0)))
	     (syntax
	      (with-syntax replacements (syntax template*))))))))

        ;;;;;;;;;;;;;;;;;;;;;;;;; SRFI 9 records
    (define-syntax define-record-type
      (lambda (stx)
	(syntax-case stx ()
	  ((define-record-type type
	     (constructor constructor-tag ...)
	     predicate
	     (field-tag accessor . more) ...)
	   (with-syntax (((index ...)
			   (let f ((i 1) (ids (syntax (field-tag ...))))
			     (if (null? ids)
				 '()
				 (cons i (f (+ i 1) (cdr ids)))))))
	    (syntax
	     (begin
	       (define type
		 (make-record-type 'type '(field-tag ...)))
	       (define constructor
		 (record-constructor type '(constructor-tag ...)))
	       (define (predicate thing)
		 (and (record? thing)
		      (eq? (record-ref thing 0) type)))
	       (define-record-field type index field-tag predicate accessor . more) ...
	       )))))))

    ;; An auxilliary macro for define field accessors and modifiers.
    ;; This is needed only because modifiers are optional.

    (define-syntax define-record-field
      (syntax-rules ()
	((define-record-field type index field-tag predicate accessor)
	 ;(define accessor (record-accessor type 'field-tag))
	 (define (accessor thing)
	   (unless (predicate thing)
	     (error "Invalid accessor"))
	   (record-ref thing index)))
	((define-record-field type index field-tag predicate accessor modifier)
	 (begin
	   (define (accessor thing)
	     (unless (predicate thing)
	       (error "Invalid accessor"))
	     (record-ref thing index))
	   (define (modifier thing value)
	     (unless (predicate thing)
	       (error "Invalid modifier"))
	     (record-set! thing index value))))))))

;; Everthing gets lumped in a 'flow all' namespace, then
;; individual re-exporting libraries are defined below.
(define (gen-all)
  `(define-library (flow all)
     (import (flow builtins) (flow primitives) (prefix (flow sys) sys:))
     (export ,@(map car identifier->library))
     (begin ,@macros)))

(with-output-to-file "headers/flow/all.sld"
  (lambda ()
    (pretty-print (gen-all))))

(define (gen-lib lib)
  (define name (first lib))
  (define path (string-append (join "/" (cons "headers" (map symbol->string (second lib)))) ".sld"))
  (define (output-ident ident)
    (and (memq name (cdr ident))
	 (car ident)))
  (with-output-to-file path
    (lambda ()
      (define idents (filter-map output-ident identifier->library))
      (pretty-print
       `(define-library ,(second lib)
	  (import (only (flow all) ,@idents))
	  (export ,@idents))))))

(for-each gen-lib libraries)
(define lib-prims (filter-map (lambda (ident)
		    (and (not (memq ident standard-macros))
			 ident))
		  (map car identifier->library)))
(define lib-p (map (lambda (p) (cons p p)) lib-prims))

;; Generate the bootup file for the expander:
;; Import the system primitives, the eval bindings,
;; and the runtime primitives.
(with-output-to-file "stdlib.scm"
  (lambda ()
    (pretty-print
     `(define (install-primitives man)
	(install-library!
	 (make-library '(flow primitives) ',lib-p '())
	 man)))

    (pretty-print
     `(define (install-sys man)
	(install-library!
	 (make-library
	  '(flow sys)
	  ',(map (lambda (x) `(,x . (primcall ,x))) sys-primitives)
	  '())
	 man)))))



