(define delims '(#\( #\) #\; #\| #\" #\space #\tab #\return #\newline #\alarm #\backspace #\delete))
(define named-chars '(("tab" . #\tab)
		      ("space" . #\space)
		      ("return" . #\return)
		      ("newline" . #\newline)
		      ("alarm" . #\alarm)
		      ("backspace" . #\backspace)
		      ("delete" . #\delete)
		      ("escape" . #\x1B)
		      ("null" . #\x0)))

(define (read-error msg . irritants)
  (raise (make-error-object 'read msg irritants)))
(define (lower-case s)
  (string-map char-downcase s))
(define (maybe-lower-case port s)
  (if (port-fold-case port) (lower-case s) s))
;; This is just read-char, but without checking:
;; port must be open, textual, input
;; char must be available (previous peek-char call).
(define (read-char-fast port)
  (let ((c (peek-char-fast port)))
    (advance-char port)
    c))
(define (advance-char port)
  (sys:FOREIGN_CALL "SCM_RECORD_SET_FAST"  port 6
		    (+ 1 (sys:FOREIGN_CALL "SCM_RECORD_REF" port 6))))
;; Must be an input/textual port.
(define (peek-char-fast port)
  (if (eq? #t (sys:FOREIGN_CALL "SCM_RECORD_REF" port 3))
      (let ((pos (sys:FOREIGN_CALL "SCM_RECORD_REF" port 6))
	    (len (sys:FOREIGN_CALL "SCM_RECORD_REF" port 7))
	    (buf (sys:FOREIGN_CALL "SCM_RECORD_REF" port 8)))
	(if (< pos len)
	    (sys:FOREIGN_CALL "SCM_STRING_REF_FAST" buf pos)
	    (peek-char port)))
      (peek-char port)))

(define (reverse! lis)
  (let lp ((lis lis) (ans '()))
    (if (null? lis) ans
        (let ((tail (cdr lis)))
          (set-cdr! lis ans)
          (lp tail lis)))))

(define (list->u8 lst)
  (let* ((len (length lst))
	 (bv (make-bytevector len)))
    (do ((i 0 (+ i 1))
	 (lst lst (cdr lst)))
	((= i len) bv)
      (bytevector-u8-set! bv i (car lst)))))
(define strbuf (make-string 100))
(define strbuf-loc 0)
(define (strbuf-add c)
  (if (< strbuf-loc (string-length strbuf))
      (begin
	(sys:FOREIGN_CALL "SCM_STRING_SET_FAST" strbuf strbuf-loc c)
	(set! strbuf-loc (+ strbuf-loc 1)))
      (let ((oldbuf strbuf))
	(set! strbuf (make-string (* 2 (string-length oldbuf))))
	(string-copy! strbuf 0 oldbuf)
	(strbuf-add c))))
(define (get-strbuf)
  (let ((res (substring strbuf 0 strbuf-loc)))
    (set! strbuf-loc 0)
    res))
(define read
  (case-lambda
   (() (read (current-input-port)))
   ((port)
    ;; Ensure this is a valid open input/textual port.
    (peek-char port)
    (let ((line 1)
	  (holes #f))
      (define (read2 port)
	(define (read-to-delimited)
	  (let loop ((c (peek-char-fast port)))
	    (cond
	     ((not (eof-object? c))
	      (case c
		((#\( #\) #\" #\| #\newline #\return #\space #\tab #\;)
		 (get-strbuf))
		(else
		 (advance-char port)
		 (strbuf-add c)
		 (loop (peek-char-fast port)))))
	     ((= 0 strbuf-loc) c)
	     (else (get-strbuf)))))
	(define (skip-whitespace)
	  (let ((c (peek-char-fast port)))
	    (cond
	     ((eof-object? c) c)
	     ((char=? #\newline c) (set! line (+ 1 line)) (advance-char port) (skip-whitespace))
	     ((char-whitespace? c) (advance-char port) (skip-whitespace)))))
	(define (skip-horizontal-whitespace)
	  (let ((c (peek-char-fast port)))
	    (cond
	     ((eof-object? c) c)
	     ((char=? #\newline c))
	     ((char-whitespace? c) (advance-char port) (skip-horizontal-whitespace)))))
	(define (skip-whitespace-and-comments)
	  (let ((c (peek-char-fast port)))
	    (if (eof-object? c) c
		(case c 
		  ((#\newline) (set! line (+ 1 line)) (advance-char port) (skip-whitespace-and-comments))
		  ((#\space #\tab #\return) (advance-char port) (skip-whitespace-and-comments))
		  ((#\;) (skip-line) (skip-whitespace-and-comments))))))
	(define (skip-whitespace-and-sexp-comments)
	  (let ((c (peek-char-fast port)))
	    (cond
	     ((eof-object? c) c)
	     ((char=? #\# c) (advance-char port)
	      (unless (eq? #\; (read-char-fast port)) (read-error "Invalid hash"))
	      (read-one)
	      (skip-whitespace-and-sexp-comments))
	     ((char=? #\newline c) (set! line (+ 1 line)) (advance-char port) (skip-whitespace-and-sexp-comments))
	     ((char-whitespace? c) (advance-char port) (skip-whitespace-and-sexp-comments))
	     ((char=? #\; c) (skip-line) (skip-whitespace-and-sexp-comments)))))
	(define (skip-line)
	  (let ((c (read-char-fast port)))
	    (cond
	     ((eof-object? c) c)
	     ((char=? c #\newline) (set! line (+ 1 line)))
	     (else (skip-line)))))
	(define (read-escape)
	  (let ((c (read-char-fast port)))
	    (if (eof-object? c) (read-error "Incomplete escape sequence"))
	    (case c
	      ((#\a) #\alarm)
	      ((#\n) #\newline)
	      ((#\r) #\return)
	      ((#\t) #\tab)
	      ((#\b) #\backspace)
	      ((#\tab #\space) (skip-line) (skip-horizontal-whitespace) #f)
	      ((#\newline) (skip-horizontal-whitespace) #f)
	      ((#\x #\X)
	       (let ((ch (string->number (read-to-delimited) 16)))
		 (if (not (eq? #\; (read-char-fast port)))
		     (read-error "Invalid hex string escape")
		     (integer->char ch))))
	      (else  c))))
	(define (read-delimited term)
	  (let loop ((res '()) (c (read-char-fast port)))
	    (cond
	     ((eof-object? c) (read-error "incomplete object:" (list->string (reverse res)) "line: " line))
	     ((char=? #\\ c)
	      (let ((es (read-escape)))
		(loop (if es (cons es res) res) (read-char-fast port))))
	     ((char=? term c) (list->string (reverse res)))
	     (else
	      (loop (cons c res) (read-char-fast port))))))
	(define (read-list)
	  (let ((line-start line))
	    (let loop ((res '()))
	      (skip-whitespace-and-comments)
	      (let ((c (peek-char-fast port)))
		(if (eof-object? c)
		    (read-error "EOF found while parsing list starting on line " line-start " and ending " line)
		    (case c 
		      ((#\)) (advance-char port) (reverse! res))
		      ((#\.) (let ((token (read-to-delimited)))
			       (if (= 1 (string-length token))
				   (let ((fin (read-one)))
				     (skip-whitespace-and-sexp-comments)
				     (if (not (eq? #\) (read-char-fast port)))
					 (read-error "Invalid dotted list:" fin)
					 (append (reverse! res) fin)))
				   (loop (cons (cond
						((string->number token) => (lambda (num) num))
						(else (string->symbol (maybe-lower-case port token)))) res)))))
		      (else (loop (cons (read-one) res)))))))))
	

	(define (do-read-char)
	  (let ((ch (peek-char-fast port)))
	    (if (memv ch delims)
		(read-char-fast port)
		(let ((token (read-to-delimited)))
		  (cond
		   ((= 1 (string-length token)) (string-ref token 0))
		   ((assoc token named-chars) => cdr)
		   ((eq? #\x (string-ref token 0)) (integer->char (string->number (string-copy token 1) 16)))
		   (else (read-error "Error invalid char: " token)))))))
	(define (skip-comment)
	  (let loop ((depth 0))
	    (case (read-char-fast port)
	      ((#\#) (loop (if (char=? #\| (peek-char-fast port)) (+ 1 depth) depth)))
	      ((#\|) (if (char=? #\# (peek-char-fast port))
			 (if (= 0 depth)
			     (read-char-fast port)
			     (loop (- depth 1)))
			 (loop depth)))
	      ((#\newline) (set! line (+ 1 line))
	       (loop depth))
	      (else (if (eof-object? (peek-char-fast port))
			(read-error "unterminated comment")
			(loop depth))))))
	(define (read-label)
	  (let read-label ((c (peek-char-fast port)) (res '()))
	    (case c
	      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (advance-char port) (read-label (peek-char-fast port) (cons c res)))
	      ((#\= #\#)  (string->number (list->string res)))
	      (else (error "Invalid label")))))
	(define (read-hash)
	  (let ((c (peek-char-fast port)))
	    (case c
	      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	       (let* ((label (read-label))
		      (next (read-char-fast port)))
		 (case next
		   ((#\#)
		    (unless (and holes (hash-table-exists? holes label)) (error "Label referenced before defined:"label))
		    (hash-table-ref holes label))
		   ((#\=)
		    (when (and holes (hash-table-exists? holes label)) (error "Multiple labels:" label))
		    (let* ((cell (cons #f #f))
			   (thunk (lambda () (car cell))))
		      (unless holes (set! holes (make-hash-table eqv?)))
		      (hash-table-set! holes label thunk)
		      (let ((res (read-one)))
			(when (procedure? res) (error "self label reference:" label))
			(set-car! cell res)
			res))))))
	      ((#\|) (skip-comment) (read-one))
	      ((#\!) (advance-char port) (read-bang) (read-one))
	      ((#\;) (advance-char port) (read-one) (read-one))
	      ((#\() (advance-char port) (list->vector (read-list)))
	      ((#\\) (advance-char port) (do-read-char))
	      ((#\`) (advance-char port) (list 'quasisyntax (read-one)))
	      ((#\,) (advance-char port)
	       (case (peek-char-fast port)
		 ((#\@) (advance-char port) (list 'unsyntax-splicing (read-one)))
		 (else
		  (list 'unsyntax (read-one)))))
	      ((#\t #\T #\f #\F)
	       (let ((v (lower-case (read-to-delimited))))
		 (cond
		  ((equal? "f" v) #f)
		  ((equal? "t" v) #t)
		  ((equal? "true" v) #t)
		  ((equal? "false" v) #f)
		  (else (read-error "Can't parse hash token:" v)))))
	      ((#\b #\B #\o #\O #\d #\D #\x #\X #\i #\I #\e #\E)
	       (string->number (string-append "#" (read-to-delimited))))
	      ((#\u) (advance-char port)
	       (unless (char=? #\8 (read-char-fast port)) (read-error "Not a bytevector"))
	       (let ((ls (read-one)))
		 (unless (list? ls) (read-error "Not a bytevector list:" ls))
		 (list->u8 ls)))
	      (else (read-error "Unknown hash reader: " c)))))
	(define (read-bang)
	  (let ((bang (read-to-delimited)))
	    (port-fold-case-set! port
				 (cond
				  ((equal? bang "no-fold-case") #f)
				  ((equal? bang "fold-case") #t)
				  (else (error "Invalid #!:" bang))))))
	(define (read-one)
	  (skip-whitespace)
	  (let ((c (peek-char-fast port)))
	    (case c
	      ((#\#) (advance-char port) (read-hash))
	      ((#\() (advance-char port)	 (read-list))
	      ((#\)) (advance-char port)	  (read-error "Extra list terminator found"))
	      ((#\") (advance-char port) (read-delimited #\"))
	      ((#\;) (advance-char port) (skip-line) (read-one))
	      ((#\|) (advance-char port) (string->symbol (read-delimited #\|)))
	      ((#\') (advance-char port) (list 'quote (read-one)))
	      ((#\`) (advance-char port) (list 'quasiquote (read-one)))
	      ((#\,) (advance-char port)
	       (case (peek-char-fast port)
		 ((#\@) (advance-char port) (list 'unquote-splicing (read-one)))
		 (else
		  (list 'unquote (read-one)))))
	      (else
	       (let  ((token (read-to-delimited)))
		 (cond 
		  ((eof-object? token) token)
		  ((string->number token))
		  (else (string->symbol (maybe-lower-case port token)))
		  ))))))
	(define (patch x)
	  (define (fill-hole x)
	    (if (procedure? x)
		(fill-hole (x))
		x))
	  (cond
	   ((pair? x)
	    (patch (car x)) (patch (cdr x))
	    (when (procedure? (car x)) (set-car! x (fill-hole (car x))))
	    (when (procedure? (cdr x)) (set-cdr! x (fill-hole (cdr x)))))
	   ((vector? x)
	    (do ((i 0 (+ i 1)))
		((= i (vector-length x)))
	      (patch (vector-ref x i))
	      (when (procedure? (vector-ref x i))
		(vector-set! x i (fill-hole (vector-ref x i))))))))
	(unless (port? port) (error "read: not a port" port))
	(let ((res (read-one)))
	  (unless (or (not holes) (= 0 (hash-table-size holes)))
	    (patch res))
	  res))
      (read2 port)))))
