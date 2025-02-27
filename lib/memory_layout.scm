; Tags for dynamic  objects.
;; fixnum is zero, so add/sub can be done without untagging.
;; ptr-untag can be instruction combined with lookup on most
;; architectures (i.e. car would be consp[8 - 1 /* untag */])
(define max-reg-args 6)

(define fixnum-tag #b000)
(define flonum-tag1 #b001)
(define ptr-tag #b010)
(define cons-tag #b011)
(define flonum-tag2 #b100)
(define flonum-tag3 #b101)
(define literal-tag #b110)
(define vector-tag #b111)
;; ptr-tagged objects
;; Bottom bits must be '010'
;; First 8 bytes are always the tag.
(define string-tag #b000010)
(define record-tag #b001010)
(define closure-tag   #b010010)
(define symbol-tag   #b011010)
(define bytevector-tag    #b100010)
(define flonum-tag    #b101010)
;; literals, using literal-tag (so bottom 3 bits must be 0b111)
(define false-rep #x006)
(define true-rep #x106)
(define bool-tag #x6)
(define char-tag #x0e)
(define nil-tag #x16)
(define eof-tag #x1e)
(define undefined-tag #x26)
