;; Quasiquote, but includes ___ syntax for destructuring.

;; Somewhat broken, the one in base.sld is better, and properly supports
;; nested with multiple values, and tail-portions remain the same.
;; TODO: merge them.

(define-syntax qq/expand
  (syntax-rules (unquote unquote-splicing quasiquote ___)
    ((_ (name ...) foo)
     (let loop ((name name) ...)
       (if (and (pair? name) ...)
	   (append (let ((name (car name)) ...)
		     (list (qq-help #t foo)))
		   (loop (cdr name) ...))
	   '()
	   )))
    ((_ name foo) 'bad-qq/expand)))

(define-syntax identifier/literal
  (syntax-rules ()
    ((identifier/literal atom identifier literal)
     (let-syntax ((check-identifier (syntax-rules ()
                                      ((_ atom symbol _) symbol)
                                      ((_ datum _ value) value))))
       (check-identifier raw-symbol identifier literal)))))

(define-syntax qq/extract-variables
  (syntax-rules (unquote unquote-splicing quasiquote ___)
    ((_ () name . rest)
     (qq/expand name . rest))
    ((_ (() . pat-rest) name . rest)
     (qq/extract-variables pat-rest name . rest))
    ((_ ((,@pat) . pat-rest) name . rest)
     (identifier/literal pat
			 (qq/extract-variables pat-rest (pat . name) . rest)
			 (qq/extract-variables pat-rest name . rest)))
    ((_ ((,pat) . pat-rest) name . rest)
     (identifier/literal pat
			 (qq/extract-variables pat-rest (pat . name) . rest)
			 (qq/extract-variables pat-rest name . rest)))
    ((_ (((a . b)) .  pat-rest) name . rest)
     (qq/extract-variables ((a) (b) . pat-rest) name . rest))
    ((_ ((pat) . pat-rest) name . rest)
     (qq/extract-variables pat-rest name . rest))
    ((_ pat . rest)
     '(bad match pat "| " . rest))))

;;; It's a quasiquote that supports ... destructuring.
(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing quasiquote ___)
    ((_ foo) (qq-help #t foo))))

(define-syntax qq-help
  (syntax-rules (unquote unquote-splicing quasiquote ___)
    ;; 'New' ... syntax.  Currently only allowed in un-nested context?
    ((_ #t ((unquote-splicing foo) ___ . rest))
     (append (apply append (qq/extract-variables ((,@foo)) () (,@foo))) (qq-help #t rest)))
    ((_ #t (foo ___)) (qq/extract-variables ((foo)) () foo))
    ((_ #t (foo ___ . rest)) (append (qq/extract-variables ((foo)) () foo) (qq-help #t rest)))

    ;; Vector:
    ((_ depth #(vec ...))  (list->vector (qq-help depth (vec ...))))

    ;; Nested quasiquotes:
    ((_ depth (quasiquote x))  (list 'quasiquote (qq-help (#t . depth) x)))
    ((_ #t (unquote x)) x)
    ((_ (#t . prev-depth) (unquote x)) (list 'unquote (qq-help prev-depth x)))

    ;; Unquote splicing:
    ((_ #t ((unquote-splicing x) . rest)) (append x (qq-help #t rest)))
    ((_ #t (unquote-splicing x)) (error "Invalid depth for unquote splicing"))
    ((_ (#t . prev-depth) (unquote-splicing x)) (list 'unquote-splicing (qq-help prev-depth x)))

    ;; Pairs and singles:
    ((_ depth (x . y)) (cons (qq-help depth x) (qq-help depth y)))
    ((_ depth x) 'x)))

;; (when #f
;;   (let ()
;;     (define y '((a b c) (1 2 3 4 5)))
;;    (define x '(1 2 3))
;;    (display `((foob ,x ,y) ___))(newline)
;;    (display `(,x ___))(newline)
;;    (display `(,x ___ y)) (newline)
;;    (display `#(1 ,x 3)) (newline)
;;    (display `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)) (newline)
;;    (display `((,x ,1) ___)) (newline)
;;    (display `(`,@x)) (newline)))
