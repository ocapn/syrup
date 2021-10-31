;;; (C) 2020-2021 Christine Lemmer-Webber
;;; Licensed under Apache v2

(define-module (syrup)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-111)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 control)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 hash-table)
  #:use-module (rnrs bytevectors)

  #:export (;;; The main procedures
            ;;; -------------------
            syrup-encode
            syrup-decode
            syrup-read
            syrup-write

            ;;; Helper datastructure wrappers
            ;;; -----------------------------
            ;;; . o O (Move into their own module?)
            ;; syrec (Syrup Records)
            make-syrec
            make-syrec*
            <syrec>
            syrec?
            syrec-label syrec-args

            ;; pseudosingles (pretend to be a single precision float)
            make-pseudosingle pseudosingle?
            psuedosingle->float

            ;; sets
            make-set set?
            set-add set-remove set-fold set->list set-member?))

;;; Data format
;;; ===========

;; Booleans: t or f
;; Single flonum: F<ieee-single-float> (big endian)
;; Double flonum: D<ieee-double-float> (big endian)
;; Positive integer: <int>+
;; Negative integer: <int>-
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

(define-char-bv plus-bv #\+)
(define-char-bv minus-bv #\-)
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

#;(define zero-plus-bv
  )

;;; Syrup records
;;; =============

;; Representation of syrup records... coersion to and from other
;; datastructures is left as an exercise to the reader (for now at least)
(define-record-type <syrec>
  (make-syrec label args)
  syrec?
  (label  syrec-label)
  (args syrec-args))

(define (make-syrec* tag . args)
  (make-syrec tag args))


;;; TODO: Move all these

;;; Other datastructures
;;; ====================

;; pseudosingle is just to express that, from a syrup perspective,
;; this is encoded as single precision floating point... even though
;; it's really double precision floating point.
(define-record-type <pseudosingle>
  (_make-pseudosingle float)
  pseudosingle?
  (float pseudosingle-float))

(define (make-pseudosingle float)
  (unless (inexact? float)
    (error "Not a valid number to wrap in a pseudosingle" float))
  (_make-pseudosingle float))

(define (pseudosingle->float psing)
  (pseudosingle-float psing))

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
(define (bytes->string/utf-8 bstr)
  (bytevector->string bstr "UTF-8"))

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

(define* (syrup-encode obj #:key [marshallers '()])
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
              (fold-right
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
  (define (encode obj)
    (match obj
      ;; Bytes are like <bytes-len>:<bytes>
      [(? bytevector?)
       (netstring-encode obj)]
      [0 (string->bytes/latin-1 "0+")]
      ;; Integers are like <integer>+ or <integer>-
      [(? integer?)
       (if (positive? obj)
           (bytes-append (string->bytes/latin-1 (number->string obj)) plus-bv)
           (bytes-append (string->bytes/latin-1 (number->string (* obj -1))) minus-bv))]
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
      ;; Single flonum floats are like F<big-endian-encoded-single-float>
      [(? pseudosingle?)
       (let ([bv (make-bytevector 4)])
         (bytevector-ieee-single-set! bv 0 obj (endianness big))
         (bytes-append F-bv bv))]
      ;; Double flonum floats are like D<big-endian-encoded-double-float>
      [(and (? number?) (? inexact?))
       (let ([bv (make-bytevector 8)])
         (bytevector-ieee-double-set! bv 0 obj (endianness big))
         (bytes-append D-bv bv))]
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
      [_
       (call/ec
        (lambda (return)
          (for-each (match-lambda
                      ((handles-it? . translate)
                       (when (handles-it? obj)
                         (let ((translated (translate obj)))
                           (if (record? translated)
                               (return (encode translated))
                               (error 'syrup-marshaller-returned-unsupported-type))))))
                    marshallers)))]))
  (encode obj))

(define* (syrup-write obj out-port #:key (marshallers '()))
  (put-bytevector out-port
                  (syrup-encode obj #:marshallers marshallers)))


(define-syntax-rule (define-char-matcher proc-name char-set)
  (define (proc-name char)
    (char-set-contains? char-set char)))

(define-char-matcher digit-char? char-set:digit)
;; This path is much more liberal than what we allow in syrup.rkt or syrup.py:
;;   (define-char-matcher whitespace-char? char-set:whitespace)
;; So we're going to be conservative for now...
;; but does it really matter?  I mean who cares.  The whitespace will
;; never appear in the normalized version...
(define-char-matcher whitespace-char?
  (string->char-set " \t\n"))


(define* (syrup-read in-port #:key (unmarshallers '()))
  (call/ec
   (lambda (return-early)
     (define (return-eof)
       (return-early the-eof-object))
     ;; Renaming because it makes porting the racket code faster :P
     (define (peek-char ip)
       (match (lookahead-u8 ip)
         [(? eof-object?) (return-eof)]
         [char-int (integer->char char-int)]))
     (define (read-char ip)
       (match (get-u8 ip)
         [(? eof-object?) (return-eof)]
         [char-int (integer->char char-int)]))

     ;; consume whitespace
     (let lp ()
       (when (whitespace-char? (peek-char in-port))
         (get-u8 in-port)
         (lp)))

     (match (peek-char in-port)
       ;; it's either a bytestring, a symbol, a string, or an integer...
       ;; we tell via the divider
       [(? digit-char?)
        (let* ([type #f]
               [int-prefix
                (string->number
                 (list->string
                  (let lp ()
                    (match (read-char in-port)
                      [#\+
                       (set! type 'positive-int)
                       '()]
                      [#\-
                       (set! type 'negative-int)
                       '()]
                      [#\:
                       (set! type 'bstr)
                       '()]
                      [#\'
                       (set! type 'sym)
                       '()]
                      [#\"
                       (set! type 'str)
                       '()]
                      [(? digit-char? digit-char)
                       (cons digit-char
                             (lp))]
                      [other-char
                       (error 'syrup-invalid-digit
                              "Invalid digit"
                              #:pos
                              (- (file-position in-port)) 1
                              #:char
                              other-char)]))))])
          (match type
            ;; it's positive, so just return as-is
            ['positive-int int-prefix]
            ;; it's negative, so invert
            ['negative-int (* int-prefix -1)]
            ;; otherwise it's some byte-length thing
            [_
             (let ([bstr (get-bytevector-n in-port int-prefix)])
               (match type
                 ['bstr
                  bstr]
                 ['sym
                  (string->symbol (bytes->string/utf-8 bstr))]
                 ['str
                  (bytes->string/utf-8 bstr)]))]))]
       ;; TODO: Switch to fashes
       ;; it's a hashmap/dictionary
       [(or #\{ #\d)
        (get-u8 in-port)
        (let lp ([ht vlist-null])
          (match (peek-char in-port)
            [(or #\} #\e)
             (get-u8 in-port)
             ht]
            [_
             (define key
               (syrup-read in-port))
             (define val
               (syrup-read in-port))
             (lp (vhash-cons key val ht))]))]
       ;; it's a record
       [#\<
        (get-u8 in-port)
        (let ([label
               (syrup-read in-port)]
              [args
               (let lp ()
                 (match (peek-char in-port)
                   [#\> '()]
                   [_ (cons (syrup-read in-port) (lp))]))])
          (call/ec
           (lambda (return)
             (for-each
              (match-lambda
                [((and (or (? symbol?) (? string?) (? number?) (? boolean?) (? bytevector?))
                       expected-label)
                  . derecordify)
                 (when (equal? label expected-label)
                   (return (apply derecordify args)))]
                [(label-pred? . derecordify)
                 (when (label-pred? label)
                   (return (apply derecordify args)))])
              unmarshallers)
             ;; no handler, return as record
             (make-syrec label args))))]
       ;; it's a single float
       [#\F
        (get-u8 in-port)
        (bytevector-ieee-double-ref (get-bytevector-n in-port 4) 0
                                    (endianness big))]
       ;; it's a double float
       [#\D
        (get-u8 in-port)
        (bytevector-ieee-double-ref (get-bytevector-n in-port 8) 0
                                    (endianness big))]
       ;; it's a boolean
       [#\t
        (get-u8 in-port)
        #t]
       [#\f
        (get-u8 in-port)
        #f]
       ;; it's a set
       [#\#
        (get-u8 in-port)
        (let lp ([s (make-set)])
          (match (peek-char in-port)
            [#\$
             (read-char in-port)
             s]
            [_
             (lp (set-add s (syrup-read in-port)))]))]
       [_
        (error 'syrup-invalid-char "Unexpected character"
               #:pos
               (file-position in-port)
               #:char
               (peek-char in-port))]))))

(define* (syrup-decode bstr #:key (unmarshallers '()))
  (define bstr-port
    (open-bytevector-input-port bstr))
  (syrup-read bstr-port #:unmarshallers unmarshallers))


(define (test-syrup)
  (define zoo-structure
    (make-syrec* (bytes "zoo")
                 "The Grand Menagerie"
                 (map alist->hash-table
                      `(((species . ,(bytes "cat"))
                         (name . "Tabatha")
                         (age . 12)
                         (weight . 8.2)
                         (alive? . #t)
                         (eats . ,(make-set (bytes "mice") (bytes "fish")
                                            (bytes "kibble"))))
                        ((species . ,(bytes "monkey"))
                         (name . "George")
                         (age . 6)
                         (weight . 17.24)
                         (alive? . #f)
                         (eats . ,(make-set (bytes "bananas")
                                            (bytes "insects"))))
                        ((species . ,(bytes "ghost"))
                         (name . "Casper")
                         (age . -12)
                         (weight . -34.5)
                         (alive? . #f)
                         (eats . ,(make-set)))))))
  (define encoded-zoo
    (call-with-values
        (lambda ()
          (open-bytevector-output-port))
      (lambda (bvp get-bytevector)
        (put-bytevector bvp (syrup-encode zoo-structure))
        (get-bytevector))))
  (test-begin "syrup")
  (call-with-input-file "../../test-data/zoo.bin"
    (lambda (ip)
      (test-equal "zoo structure encodes as expected"
        (get-bytevector-all ip)
        encoded-zoo)))
  (test-end "syrup"))


(define (tests)
  (test-sets)
  (test-bytes-utils)
  (test-syrup))
