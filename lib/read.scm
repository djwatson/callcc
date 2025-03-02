(define delims '(#\( #\) #\; #\| #\" #\space #\tab #\return #\newline #\alarm #\backspace #\delete))
(define named-chars '(("tab" . #\tab)
		      ("space" . #\space)
		      ("return" . #\return)
		      ("newline" . #\newline)
		      ("alarm" . #\alarm)
		      ("backspace" . #\backspace)
		      ("delete" . #\delete)
		      ("escape" . #\x1B)
		      ("null" . #\x0)
		      ))

(define (read-error msg . irritants)
  (apply error msg irritants)
  (raise (make-error-object 'read msg irritants)))
(define (lower-case s)
  (string-map char-downcase s))
(define (maybe-lower-case port s)
  (if (port-fold-case port) (lower-case s) s))

(define (list->u8 lst)
  (let* ((len (length lst))
	 (bv (make-bytevector len)))
    (do ((i 0 (+ i 1))
	 (lst lst (cdr lst)))
	((= i len) bv)
      (bytevector-u8-set! bv i (car lst)))))
(define read
  (case-lambda
   (() (read (current-input-port)))
   ((port)
    (let ((line 1)
	  (holes #f))
      (define (read2 port)
	(define (read-to-delimited)
	  (let loop ((res '()) (c (peek-char port)))
	    (if (eof-object? c)
		(if (null? res)
		    c
		    (list->string (reverse res)))
		(case c
		  ((#\( #\) #\" #\| #\newline #\return #\space #\tab #\;)
		   (list->string (reverse res)))
		  (else
		   (loop (cons (read-char port) res) (peek-char port)))))))
	(define (skip-whitespace)
	  (let loop ()
	    (let ((c (peek-char port)))
	      (cond
	       ((eof-object? c) c)
	       ((char=? #\newline c) (set! line (+ 1 line)) (read-char port) (loop))
	       ((char-whitespace? c) (read-char port) (loop))))))
	(define (skip-horizontal-whitespace)
	  (let loop ()
	    (let ((c (peek-char port)))
	      (cond
	       ((eof-object? c) c)
	       ((char=? #\newline c))
	       ((char-whitespace? c) (read-char port) (loop))))))
	(define (skip-whitespace-and-comments)
	  (let loop ()
	    (let ((c (peek-char port)))
	      (cond
	       ((eof-object? c) c)
	       ((char=? #\newline c) (set! line (+ 1 line)) (read-char port) (loop))
	       ((char-whitespace? c) (read-char port) (loop))
	       ((char=? #\; c) (skip-line) (loop))))))
	(define (skip-whitespace-and-sexp-comments)
	  (let loop ()
	    (let ((c (peek-char port)))
	      (cond
	       ((eof-object? c) c)
	       ((char=? #\# c) (read-char port)
		(unless (eq? #\; (read-char port)) (read-error "Invalid hash"))
		(read-one)
		(loop))
	       ((char=? #\newline c) (set! line (+ 1 line)) (read-char port) (loop))
	       ((char-whitespace? c) (read-char port) (loop))
	       ((char=? #\; c) (skip-line) (loop))))))
	(define (skip-line)
	  (let loop ()
	    (let ((c (read-char port)))
	      (if (eof-object? c)
		  c
		  (if (char=? c #\newline)
		      (set! line (+ 1 line))
		      (loop))))))
	(define (read-escape)
	  (let ((c (read-char port)))
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
	       (let* ((delim (read-to-delimited))
		      (ch (string->number delim 16))
		      (next (read-char port)))
		 (if (not (eq? #\; next))
		     (read-error "Invalid hex string escape")
		     (integer->char ch))))
	      (else  c)))
	  )
	(define (read-delimited term)
	  (let loop ((res '()) (c (read-char port)))
	    (cond
	     ((eof-object? c) (read-error "incomplete object:" (list->string (reverse res)) "line: " line))
	     ((char=? #\\ c)
	      (let* ((es (read-escape)))
		(if es
		    (loop (cons es res) (read-char port))
		    (loop res (read-char port)))))
	     ((char=? term c) (list->string (reverse res)))
	     (else
	      (loop (cons c res) (read-char port))))))
	(define (read-list)
	  (let ((line-start line))
	    (let loop ((res '()))
	      (skip-whitespace-and-comments)
	      (let ((c (peek-char port)))
		(cond
		 ((eof-object? c) (read-error "EOF found while parsing list starting on line " line-start " and ending " line))
		 ((char=? c #\)) (read-char port) (reverse res))
		 ((char=? c #\.) (let ((token (read-to-delimited)))
				   (if (= 1 (string-length token))
				       (let ((fin (read-one)))
					 (skip-whitespace-and-sexp-comments)
					 (if (not (eq? #\) (read-char port)))
					     (read-error "Invalid dotted list:" fin)
					     (append (reverse res) fin)))
				       (loop (cons (cond
						    ((string->number token) => (lambda (num) num))
						    (else (string->symbol (maybe-lower-case port token)))) res)))))
		 (else (loop (cons (read-one) res))))))))
	

	(define (do-read-char)
	  (let ((ch (peek-char port)))
	    (if (memv ch delims)
		(read-char port)
		(let ((token (read-to-delimited)))
		  (cond
		   ((= 1 (string-length token)) (string-ref token 0))
		   ((assoc token named-chars) => cdr)
		   ((eq? #\x (string-ref token 0)) (integer->char (string->number (substring token 1 (string-length token)) 16)))
		   (else (read-error "Error invalid char: " token)))))))
	(define (skip-comment)
	  (let loop ((depth 0))
	    (case (read-char port)
	      ((#\#) (loop (if (char=? #\| (peek-char port)) (+ 1 depth) depth)))
	      ((#\|) (if (char=? #\# (peek-char port))
			 (if (= 0 depth)
			     (read-char port)
			     (loop (- depth 1)))
			 (loop depth)))
	      ((#\newline) (set! line (+ 1 line))
	       (loop depth))
	      (else (if (eof-object? (peek-char port))
			(read-error "unterminated comment")
			(loop depth))))))
	(define (read-label)
	  (let read-label ((c (peek-char port)) (res '()))
	    (case c
	      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (read-char port) (read-label (peek-char port) (cons c res)))
	      ((#\= #\#)  (string->number (list->string res)))
	      (else (error "Invalid label")))))
	(define (read-hash)
	  (let ((c (peek-char port)))
	    (case c
	      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	       (let* ((label (read-label))
		      (next (read-char port)))
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
	      ((#\!) (read-char port) (read-bang) (read-one))
	      ((#\;) (read-char port) (read-one) (read-one))
	      ((#\() (read-char port) (list->vector (read-list)))
	      ((#\\) (read-char port) (do-read-char))
	      ((#\`) (read-char port) (list 'quasisyntax (read-one)))
	      ((#\,) (read-char port)
	       (case (peek-char port)
		 ((#\@) (read-char port) (list 'unsyntax-splicing (read-one)))
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
	      ((#\b #\B #\o #\O #\d #\D #\x #\X #\i #\I #\e #\E) (string->number (string-append "#" (read-to-delimited))))
	      ((#\u) (read-char port)
	       (if (not (char=? #\8 (peek-char port))) (read-error "Not a bytevector:" (peek-char port))
		   (read-char port))
	       (let ((ls (read-one)))
		 (if (not (list? ls))
		     (read-error "Not a bytevector list:" ls)
		     (list->u8 ls))))
	      (else (read-error "Unknown hash reader: " c)))))
	(define (read-bang)
	  (let ((bang (read-to-delimited)))
	    (cond
	     ((equal? bang "no-fold-case") (port-fold-case-set! port #f))
	     ((equal? bang "fold-case") (port-fold-case-set! port #t))
	     (else (error "Invalid #!:"bang)))))
	(define (read-one)
	  (skip-whitespace)
	  (let ((c (peek-char port)))
	    (case (peek-char port)
	      ((#\#) (read-char port) (read-hash))
	      ((#\() (read-char port)	 (read-list))
	      ((#\)) (read-char port)	  (read-error "Extra list terminator found"))
	      ((#\") (read-char port) (read-delimited #\"))
	      ((#\;) (read-char port) (skip-line) (read-one))
	      ((#\|) (read-char port) (string->symbol (read-delimited #\|)))
	      ((#\') (read-char port) (list 'quote (read-one)))
	      ((#\`) (read-char port) (list 'quasiquote (read-one)))
	      ((#\,) (read-char port)
	       (case (peek-char port)
		 ((#\@) (read-char port) (list 'unquote-splicing (read-one)))
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
