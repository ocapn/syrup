(use-modules (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-9 gnu)
             (srfi srfi-64)
             (ice-9 iconv)
             (ice-9 vlist)
             (ice-9 hash-table)
             (rnrs bytevectors))

;;; Data format
;;; ===========

;; Booleans: t or f
;; Single flonum: F<ieee-single-float> (big endian)
;; Double flonum: D<ieee-double-float> (big endian)
;; (Signed) integers: i<maybe-sign><int>e
;; Bytestrings: 3:cat
;; Strings: 3"cat
;; Symbols: 3'cat
;; Dictionary: {<key1><val1><key1><val1>}
;; Lists: [<item1><item2><item3>]
;; Records: <<label><val1><val2><val3>> (the outer <> for realsies tho)
;; Sets: #<item1><item2><item3>$

(define-syntax-rule (define-char-bv id char)
  (define id
    (make-bytevector 1 (char->integer char))))

(define-char-bv i-bv #\i)
(define-char-bv e-bv #\e)
(define-char-bv squarebrac-left-bv #\[)
(define-char-bv squarebrac-right-bv #\])
(define-char-bv curly-left-bv #\{)
(define-char-bv curly-right-bv #\})
(define-char-bv anglebrac-left-bv #\<)
(define-char-bv anglebrac-right-bv #\>)
(define-char-bv doublequote-bv #\")
(define-char-bv singlequote-bv #\')
(define-char-bv colon-bv #\:)
(define-char-bv F-bv #\F)
(define-char-bv D-bv #\D)
(define-char-bv f-bv #\f)
(define-char-bv t-bv #\t)
(define-char-bv hash-bv #\#)
(define-char-bv dollar-bv #\$)

;;; Syrup records
;;; =============

;; Representation of syrup records... coersion to and from other
;; datastructures is left as an exercise to the reader (for now at least)
(define-record-type <syrec>
  (_make-syrec label args)
  syrec?
  (label  syrec-label)
  (args syrec-args))

(define (make-syrec tag . args)
  (_make-syrec tag args))


;;; TODO: Move all these

;;; Other datastructures
;;; ====================

;;; An extremely meh implementation of sets

(define-record-type <set>
  (_make-set ht)
  set?
  (ht _set-ht))

(define (print-set set port)
  (define items
    (vhash-fold
     (lambda (k _v prev)
       (cons k prev))
     '()
     (_set-ht set)))
  (format port "#<set ~a>" items))

(set-record-type-printer! <set> print-set)


;; pseudosingle is just to express that, from a syrup perspective,
;; this is encoded as single precision floating point... even though
;; it's really double precision floating point.
(define-record-type <pseudosingle>
  (make-pseudosingle float)
  pseudosingle?
  (float pseudosingle-float))

(define (pseudosingle->float psing)
  (pseudosingle-float psing))

(define (make-set . items)
  (define vh
    (fold
     (lambda (item vh)
       (vhash-consq item #t vh))
     vlist-null items))
  (_make-set vh))

(define (set-add set item)
  (_make-set (vhash-cons item #t (_set-ht set))))

(define (set-remove set item)
  (_make-set (vhash-delete (_set-ht set) item)))

(define (set-fold proc init set)
  (vhash-fold
   (lambda (key _val prev)
     (proc key prev))
   init
   (_set-ht set)))

(define (set->list set)
  (vhash-fold
   (lambda (key _val prev)
     (cons key prev))
   '()
   (_set-ht set)))

(define (set-member? set key)
  (match (vhash-assoc key (_set-ht set))
    [(_val . #t)
     #t]
    [#f #f]))

(define (test-sets)
  (test-begin "sets")
  ;;;; Well the negative out of order set equality doesn't work, so...
  ;;;; TODO: add set-equal?
  ;; (unless (equal? (make-set 1 2 3)
  ;;                 (make-set 1 2 3))
  ;;   (error 'test "set equality"))
  ;; (unless (equal? (make-set 3 2 1)
  ;;                 (make-set 1 2 3))
  ;;   (error 'test "out-of-order set equality"))
  (let ([s (make-set 1 2 3)])
    (test-assert "positive set membership"
      (set-member? s 1))
    (test-assert "negative set membership"
      (not (set-member? s 99))))
  (test-end "sets"))



;;; bytevector utils
;;; ================

(define (bytes-append . bvs)
  (define new-bv-len
    (fold
     (lambda (x prev)
       (+ (bytevector-length x) prev))
     0 bvs))
  (define new-bv
    (make-bytevector new-bv-len))
  (let lp ([cur-pos 0]
           [bvs bvs])
    (match bvs
      [(this-bv . next-bvs)
       (define this-bv-len
         (bytevector-length this-bv))
       (bytevector-copy! this-bv 0 new-bv cur-pos this-bv-len)
       ;; move onto next bytevector
       (lp (+ cur-pos this-bv-len)
           next-bvs)]
      ['() 'done]))
  new-bv)

;; expected: #vu8(0 0 0 0 11 11 22 22)
#; (bytes-append #vu8(00 00 00 00)
                 #vu8(11 11)
                 #vu8(22 22))

(define* (netstring-encode bstr #:key [joiner colon-bv])
  (define bstr-len
    (bytevector-length bstr))
  (define bstr-len-as-bytes
    (string->bytes/latin-1 (number->string bstr-len)))
  (bytes-append bstr-len-as-bytes
                joiner
                bstr))

(define (string->bytes/latin-1 str)
  (string->bytevector str "ISO-8859-1"))
(define (string->bytes/utf-8 str)
  (string->bytevector str "UTF-8"))

;; alias for simplicity
(define bytes string->bytes/latin-1)

;; Test: 
#;(bytevector->string
 (netstring-encode
  (string->bytevector "Hello world!" "ISO-8859-1"))
 "ISO-8859-1")
;; => "12:Hello world!"

(define (bytes<? bstr1 bstr2)
  (define bstr1-len
    (bytevector-length bstr1))
  (define bstr2-len
    (bytevector-length bstr2))
  (let lp ([pos 0])
    (cond
     ;; we've reached the end of both and they're the same bytestring
     ;; but this isn't <=?
     [(and (eqv? bstr1-len pos)
           (eqv? bstr2-len pos))
      #f]
     ;; we've reached the end of bstr1 but not bstr2, so yes it's less
     [(eqv? bstr1-len pos)
      #t]
     ;; we've reached the end of bstr2 but not bstr1, so no
     [(eqv? bstr2-len pos)
      #f]
     ;; ok, time to compare bytes
     [else
      (let ([bstr1-byte (bytevector-u8-ref bstr1 pos)]
            [bstr2-byte (bytevector-u8-ref bstr2 pos)])
        (if (eqv? bstr1-byte bstr2-byte)
            ;; they're the same, so loop
            (lp (1+ pos))
            ;; otherwise, just compare nubmers
            (< bstr1-byte bstr2-byte)))])))


(define (test-bytes-utils)
  (test-begin "bytes-utils")
  (test-assert "same bytestring isn't less"
    (not (bytes<? (bytes "meep")
                  (bytes "meep"))))
  (test-assert "greater bytestring of same length isn't less"
    (not (bytes<? (bytes "meep")
                  (bytes "beep"))))
  (test-assert "lesser bytestring of same length is less"
    (bytes<? (bytes "beep")
             (bytes "meep")))
  (test-assert "greater bytestring of same length isn't less 2"
    (not (bytes<? (bytes "meep")
                  (bytes "meeb"))))
  (test-assert "lesser bytestring of same length is less 2"
    (bytes<? (bytes "meeb")
             (bytes "meep")))
  (test-assert "shorter bytestring is less"
    (bytes<? (bytes "meep")
             (bytes "meeple")))
  (test-assert "longer bytestring is greater"
    (not (bytes<? (bytes "meeple")
                  (bytes "meep"))))
  (test-end "bytes-utils"))


;;; Encoding
;;; ========

;; TODO: Switch grafts over

(define (syrup-encode obj)
  (define (build-encode-hash hash-ref hash-fold)
    (lambda (obj)
      (let* ([keys-and-encoded
              (hash-fold
               (lambda (key _val prev)
                 (cons (cons (syrup-encode key)
                             key)
                       prev))
               '()
               obj)]
             [sorted-keys-and-encoded
              (sort keys-and-encoded
                    (match-lambda*
                      [((encoded1 . _k1)
                        (encoded2 . _k2))
                       (bytes<? encoded1 encoded2)]))]
             [encoded-hash-pairs
              (fold
               (lambda (ke prev)
                 (match ke
                   [(enc-key . key)
                    (let ([val (hash-ref obj key)])
                      (cons (bytes-append enc-key (syrup-encode val))
                            prev))]))
               '()
               sorted-keys-and-encoded)])
        (bytes-append curly-left-bv
                      (apply bytes-append encoded-hash-pairs)
                      curly-right-bv))))
  (define encode-hash
    (build-encode-hash hash-ref hash-fold))
  (define encode-vhash
    (build-encode-hash
     ;; we shouldn't need a not-found case here
     (lambda (vh key)
       (match (vhash-assoc key vh)
         [(_ . val) val]))
     vhash-fold))
  (match obj
    ;; Bytes are like <bytes-len>:<bytes>
    [(? bytevector?)
     (netstring-encode obj)]
    ;; Integers are like i<maybe-signed-integer>e
    [(? integer?)
     (bytes-append i-bv (string->bytes/latin-1 (number->string obj)) e-bv)]
    ;; Lists are like [<item1><item2><item3>]
    [(? pair?)
     (bytes-append squarebrac-left-bv
                   (apply bytes-append
                          (map syrup-encode obj))
                   squarebrac-right-bv)]
    ;; Dictionaries are like {<key1><val1><key2><val2>}
    ;; We sort by the key being fully encoded.
    [(? hash-table?)
     (encode-hash obj)]
    ;; TODO: I guess this throws encoding just-vlists out the window.
    ;;   Replace with fashes for our functional hashtables!
    [(? vlist?)
     (encode-vhash obj)]
    ;; Strings are like <encoded-bytes-len>"<utf8-encoded>
    [(? string?)
     (netstring-encode (string->bytes/utf-8 obj)
                       #:joiner doublequote-bv)]
    ;; Symbols are like <encoded-bytes-len>'<utf8-encoded>
    [(? symbol?)
     (netstring-encode (string->bytes/utf-8
                        (symbol->string obj))
                       #:joiner singlequote-bv)]
    ;; ;; Single flonum floats are like F<big-endian-encoded-single-float>
    ;; [(? single-flonum?)
    ;;  (bytes-append #"F"
    ;;                (real->floating-point-bytes obj 4 #t))]
    ;; Double flonum floats are like D<big-endian-encoded-double-float>
    [(and (? real?) (? inexact?))
     (let ([bv (make-bytevector 8)])
       (bytevector-ieee-double-set! bv 0 obj (endianness big))
       bv)]
    ;; Records are like <<tag><arg1><arg2>> but with the outer <> for realsies
    [(? syrec?)
     (bytes-append anglebrac-left-bv
                   (syrup-encode (syrec-label obj))
                   (apply bytes-append
                          (map syrup-encode (syrec-args obj)))
                   anglebrac-right-bv)]
    ;; #t is t, #f is f
    [#t t-bv]
    [#f f-bv]
    ;; Sets are like #<item1><item2><item3>$
    [(? set?)
     (let* ([encoded-items
             (set-fold
              (lambda (item prev)
                (cons (syrup-encode item)
                      prev))
              '() obj)]
            [sorted-items
             (sort encoded-items
                   bytes<?)])
       (bytes-append hash-bv
                     (apply bytes-append sorted-items)
                     dollar-bv))]
    [_ (error 'syrup-unsupported-type obj)]))


(define (tests)
  (test-sets)
  (test-bytes-utils))
