(define *default-bound* (- (expt 2 29) 3))

(define string-hash
  (case-lambda
   ((s) (string-hash *default-bound*))
   ((s bound)
    (modulo (sys:FOREIGN_CALL "SCM_STRING_HASH" s) bound))))

(define string-ci-hash
  (case-lambda
   ((s) (string-ci-hash s *default-bound*))
   ((s bound)
    (string-hash (string-foldcase s) bound))))

(define symbol-hash
  (case-lambda
   ((s) (symbol-hash s *default-bound*))
   ((s bound)
    (modulo (sys:FOREIGN_CALL "SCM_STRING_HASH" (symbol->string s)) bound))))

(define hash 
  (case-lambda
   ((obj) (hash obj *default-bound*))
   ((obj bound)
    (cond ((inexact? obj) 0)
	  ((integer? obj) (modulo obj bound))
	  ((string? obj) (string-hash obj bound))
	  ((symbol? obj) (symbol-hash obj bound))
	  ((real? obj) (modulo (+ (numerator obj) (denominator obj)) bound))
	  ((number? obj)
	   (modulo (+ (hash (real-part obj)) (* 3 (hash (imag-part obj))))
		   bound))
	  ((char? obj) (modulo (char->integer obj) bound))
	  ((vector? obj) (vector-hash obj bound))
	  ((pair? obj) (modulo (+ (hash (car obj)) (* 3 (hash (cdr obj))))
			       bound))
	  ((null? obj) 0)
	  ((not obj) 0)
	  ((boolean? obj ) 0)
	  ((procedure? obj) (error "hash: procedures cannot be hashed" obj))
	  (else
	   (error "Unknown object in hash:" obj)
	   1
	   )))))

(define hash-by-identity
  (case-lambda
   ((x) (hash-by-identity x *default-bound*))
   ((x bound)
    (modulo (sys:FOREIGN_CALL "SCM_EQ_HASH" x) bound))))

(define (vector-hash v bound)
  (let ((hashvalue 571)
	(len (vector-length v)))
    (do ((index 0 (+ index 1)))
	((>= index len) (modulo hashvalue bound))
      (set! hashvalue (modulo (+ (* 257 hashvalue) (hash (vector-ref v index)))
			      *default-bound*)))))

(define %make-hash-node cons)
(define %hash-node-set-value! set-cdr!)
(define %hash-node-key car)
(define %hash-node-value cdr)

(define-record-type <srfi-hash-table>
  (%make-hash-table size hash compare associate entries)
  hash-table?
  (size hash-table-size hash-table-set-size!)
  (hash hash-table-hash-function)
  (compare hash-table-equivalence-function)
  (associate hash-table-association-function)
  (entries hash-table-entries hash-table-set-entries!))

(define *default-table-size* 64)

(define (appropriate-hash-function-for comparison)
  (or (and (eq? comparison eq?) hash-by-identity)
      (and (eq? comparison string=?) string-hash)
      (and (eq? comparison string-ci=?) string-ci-hash)
      hash))

(define make-hash-table
  (case-lambda
    (() (make-hash-table equal? hash *default-table-size*))
    ((cmp) (make-hash-table cmp (appropriate-hash-function-for cmp ) *default-table-size*))
    ((cmp hf) (make-hash-table cmp hf *default-table-size*))
    ((comparison hash size)
     (let* ((association
	     (or (and (eq? comparison eq?) assq)
		 (and (eq? comparison eqv?) assv)
		 (and (eq? comparison equal?) assoc)
		 (letrec
		     ((associate
		       (lambda (val alist)
			 (cond ((null? alist) #f)
			       ((comparison val (caar alist)) (car alist))
			       (else (associate val (cdr alist)))))))
		   associate))))
       (%make-hash-table 0 hash comparison association (make-vector size '()))))))

(define (make-hash-table-maker comp hash)
  (lambda args (apply make-hash-table (cons comp (cons hash args)))))
(define make-symbol-hash-table
  (make-hash-table-maker eq? symbol-hash))
(define make-string-hash-table
  (make-hash-table-maker string=? string-hash))
(define make-string-ci-hash-table
  (make-hash-table-maker string-ci=? string-ci-hash))
(define make-integer-hash-table
  (make-hash-table-maker = modulo))

(define (%hash-table-hash hash-table key)
  ((hash-table-hash-function hash-table)
   key (vector-length (hash-table-entries hash-table))))

(define (%hash-table-find entries associate hash key)
  (associate key (vector-ref entries hash)))

(define (%hash-table-add! entries hash key value)
  (vector-set! entries hash
	       (cons (%make-hash-node key value)
		     (vector-ref entries hash))))

(define (%hash-table-delete! entries compare hash key)
  (let ((entrylist (vector-ref entries hash)))
    (cond ((null? entrylist) #f)
	  ((compare key (caar entrylist))
	   (vector-set! entries hash (cdr entrylist)) #t)
	  (else
	   (let loop ((current (cdr entrylist)) (previous entrylist))
	     (cond ((null? current) #f)
		   ((compare key (caar current))
		    (set-cdr! previous (cdr current)) #t)
		   (else (loop (cdr current) current))))))))

(define (%hash-table-walk proc entries)
  (do ((index (- (vector-length entries) 1) (- index 1)))
      ((< index 0)) (for-each proc (vector-ref entries index))))

(define (%hash-table-maybe-resize! hash-table)
  (let* ((old-entries (hash-table-entries hash-table))
	 (hash-length (vector-length old-entries)))
    (if (> (hash-table-size hash-table) hash-length)
	(let* ((new-length (* 2 hash-length))
	       (new-entries (make-vector new-length '()))
	       (hash (hash-table-hash-function hash-table)))
	  (%hash-table-walk
	   (lambda (node)
	     (%hash-table-add! new-entries
			       (hash (%hash-node-key node) new-length)
			       (%hash-node-key node) (%hash-node-value node)))
	   old-entries)
	  (hash-table-set-entries! hash-table new-entries)))))

(define hash-table-ref
  (case-lambda
   ((hash-table key)
    (cond ((%hash-table-find (hash-table-entries hash-table)
			     (hash-table-association-function hash-table)
			     (%hash-table-hash hash-table key) key)
	   => %hash-node-value)
	  (else
	   (error "hash-table-ref: no value associated with" key))))
   ((hash-table key default)
    (cond ((%hash-table-find (hash-table-entries hash-table)
			     (hash-table-association-function hash-table)
			     (%hash-table-hash hash-table key) key)
	   => %hash-node-value)
	  (else default)))))

(define (hash-table-ref/default hash-table key default)
  (hash-table-ref hash-table key default))

(define (hash-table-set! hash-table key value)
  (let ((hash (%hash-table-hash hash-table key))
	(entries (hash-table-entries hash-table)))
    (cond ((%hash-table-find entries
			     (hash-table-association-function hash-table)
			     hash key)
	   => (lambda (node) (%hash-node-set-value! node value)))
	  (else (%hash-table-add! entries hash key value)
		(hash-table-set-size! hash-table
				      (+ 1 (hash-table-size hash-table)))
		(%hash-table-maybe-resize! hash-table)))))

(define hash-table-update!
  (case-lambda
   ((hash-table key function) (hash-table-update! hash-table key function #f))
   ((hash-table key function default)
    (let ((hash (%hash-table-hash hash-table key))
	  (entries (hash-table-entries hash-table)))
      (cond ((%hash-table-find entries
			       (hash-table-association-function hash-table)
			       hash key)
	     => (lambda (node)
		  (%hash-node-set-value!
		   node (function (%hash-node-value node)))))
	    ((not default)
	     (error "hash-table-update!: no value exists for key" key))
	    (else (%hash-table-add! entries hash key
				    (function (default)))
		  (hash-table-set-size! hash-table
					(+ 1 (hash-table-size hash-table)))
		  (%hash-table-maybe-resize! hash-table)))))))

(define (hash-table-update!/default hash-table key function default)
  (hash-table-update! hash-table key function (lambda () default)))

(define (hash-table-delete! hash-table key)
  (if (%hash-table-delete! (hash-table-entries hash-table)
			   (hash-table-equivalence-function hash-table)
			   (%hash-table-hash hash-table key) key)
      (hash-table-set-size! hash-table (- (hash-table-size hash-table) 1))))

(define (hash-table-exists? hash-table key)
  (and (%hash-table-find (hash-table-entries hash-table)
			 (hash-table-association-function hash-table)
			 (%hash-table-hash hash-table key) key) #t))

(define (hash-table-walk hash-table proc)
  (%hash-table-walk
   (lambda (node) (proc (%hash-node-key node) (%hash-node-value node)))
   (hash-table-entries hash-table)))

(define (hash-table-fold hash-table f acc)
  (hash-table-walk hash-table 
		   (lambda (key value) (set! acc (f key value acc))))
  acc)

(define alist->hash-table
  (case-lambda
   ((alist) (alist->hash-table alist equal?))
   ((alist comparison) (alist->hash-table alist comparison (appropriate-hash-function-for comparison)))
   ((alist comparison hash) (alist->hash-table alist comparison hash (max *default-table-size* (* 2 (length alist)))))
   ((alist comparison hash size)
    (let ((hash-table (make-hash-table comparison hash size)))
      (for-each
       (lambda (elem)
	 (hash-table-update!/default
	  hash-table (car elem) (lambda (x) x) (cdr elem)))
       alist)
      hash-table))))

(define (hash-table->alist hash-table)
  (hash-table-fold hash-table
		   (lambda (key val acc) (cons (cons key val) acc)) '()))

(define (hash-table-copy hash-table)
  (let ((new (make-hash-table (hash-table-equivalence-function hash-table)
  			      (hash-table-hash-function hash-table)
			      (max *default-table-size*
				   (* 2 (hash-table-size hash-table))))))
    (hash-table-walk hash-table
		     (lambda (key value) (hash-table-set! new key value)))
    new))

(define (hash-table-merge! hash-table1 hash-table2)
  (hash-table-walk
   hash-table2
   (lambda (key value) (hash-table-set! hash-table1 key value)))
  hash-table1)

(define (hash-table-keys hash-table)
  (hash-table-fold hash-table (lambda (key val acc) (cons key acc)) '()))

(define (hash-table-values hash-table)
  (hash-table-fold hash-table (lambda (key val acc) (cons val acc)) '()))
