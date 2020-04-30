#lang racket/base

(require racket/match
         racket/set)

(struct record (label args)
  #:transparent)

(define (record* label . args)
  (record label args))

(define (netstring-encode bstr)
  (bytes-append (string->bytes/latin-1 (number->string (bytes-length bstr)))
                #":"
                bstr))

;; Booleans: #"t" or #"f"
;; Single flonum: F<ieee-single-float> (big endian)
;; Double flonum: D<ieee-double-float> (big endian)
;; (Signed) integers: i<maybe-sign><int>e
;; Bytestrings: netstrings, as so: 3:cat
;; Strings: "<utf8-encoded-netstring>
;; Symbols: '<utf8-encoded-netstring-of-symbol>
;; Floats: f<float>e
;; Dictionary: {<key1><val1><key1><val1>}
;; Lists: (<item1><item2><item3>)
;; Records: <<label><val1><val2><val3>> (the outer <> for realsies tho)
;; Sets: s<item1><item2><item3>e

(define (syrup-encode obj)
  (match obj
    ;; Bytes are like <bytes-len>:<bytes>
    [(? bytes?)
     (netstring-encode obj)]
    ;; Integers are like i<maybe-signed-integer>e
    [(? integer?)
     (bytes-append #"i" (string->bytes/latin-1 (number->string obj)) #"e")]
    ;; Lists are like l<item1><item2><item3>e
    [(? list?)
     (bytes-append #"("
                   (apply bytes-append
                          (map syrup-encode obj))
                   #")")]
    ;; Dictionaries are like d<key1><val1><key2><val2>e
    ;; We sort by the key being fully encoded.
    [(? hash?)
     (define keys-and-encoded
       (for/list ([key (hash-keys obj)])
         (cons (syrup-encode key) key)))
     (define sorted-keys-and-encoded
       (sort keys-and-encoded
             (match-lambda*
               [(list (and ke1 (cons encoded1 _k1))
                      (and ke2 (cons encoded2 _k2)))
                (bytes<? encoded1 encoded2)])))
     (define encoded-hash-pairs
       (for/list ([ke sorted-keys-and-encoded])
         (match ke
           [(cons enc-key key)
            (define val
              (hash-ref obj key))
            (bytes-append enc-key (syrup-encode val))])))
     (bytes-append #"{"
                   (apply bytes-append encoded-hash-pairs)
                   #"}")]
    [(? string?)
     (bytes-append #"\""
                   (netstring-encode (string->bytes/utf-8 obj)))]
    [(? symbol?)
     (bytes-append #"'" (netstring-encode
                         (string->bytes/utf-8
                          (symbol->string obj))))]
    ;; TODO: still semi-unsure if this is right!
    [(? single-flonum?)
     (bytes-append #"F"
                   (real->floating-point-bytes obj 4 #t))]
    [(? double-flonum?)
     (bytes-append #"D"
                   (real->floating-point-bytes obj 8 #t))]
    [(? record?)
     (bytes-append #"<"
                   (syrup-encode (record-label obj))
                   (apply bytes-append
                          (map syrup-encode (record-args obj)))
                   #">")]
    [#t #"t"]
    [#f #"f"]
    [(? set?)
     (define encoded-items
       (for/list ([item obj])
         (syrup-encode item)))
     (define sorted-items
       (sort encoded-items
             bytes<?))
     (bytes-append #"s"
                   (apply bytes-append sorted-items)
                   #"e")]
    [_ (error 'syrup-unsupported-type obj)]))

(define digit-chars
  (seteq #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define (digit-char? char)
  (set-member? digit-chars char))

(define (read-netstring in-port)
  (define bytes-len
    (string->number
     (list->string
      (let lp ()
        (match (read-char in-port)
          [#\:
           '()]
          [(? digit-char? digit-char)
           (cons digit-char
                 (lp))]
          [other-char
           (error 'syrup-invalid-digit
                  "Invalid digit at pos ~a: ~a"
                  (file-position in-port)
                  other-char)])))))
  (read-bytes bytes-len in-port))

(define (syrup-read in-port)
  (match (peek-char in-port)
    ;; it's a bytestring
    [(? digit-char?)
     (read-netstring in-port)]
    ;; it's an integer
    [#\i
     (read-byte in-port)
     (define negative?
       (if (eq? (peek-char in-port) #\-)
           (begin
             (read-byte in-port)
             #t)
           #f))
     (define num
       (string->number
        (list->string
         (let lp ()
           ;; TODO: more error handling here
           (match (read-char in-port)
             [#\e
              '()]
             [(? digit-char? digit-char)
              (cons digit-char
                    (lp))]
             [other-char
              (error 'syrup-invalid-digit
                     "Invalid digit at pos ~a: ~a"
                     (file-position in-port)
                     other-char)])))))
     (if negative?
         (* num -1)
         num)]
    ;; it's a list
    [#\(
     (read-byte in-port)
     (let lp ()
       (match (peek-char in-port)
         ;; We've reached the end
         [#\)
          (read-byte in-port)
          '()]
         ;; one more loop
         [_
          (cons (syrup-read in-port) (lp))]))]
    [#\{
     (read-byte in-port)
     (let lp ([ht #hash()])
       (match (peek-char in-port)
         [#\}
          (read-byte in-port)
          ht]
         [_
          (define key
            (syrup-read in-port))
          (define val
            (syrup-read in-port))
          (lp (hash-set ht key val))]))]
    [#\"
     (read-byte in-port)
     (bytes->string/utf-8 (read-netstring in-port))]
    [#\'
     (read-byte in-port)
     (string->symbol (bytes->string/utf-8 (read-netstring in-port)))]
    [#\<
     (read-byte in-port)
     (define label
       (syrup-read in-port))
     (define args
       (let lp ()
         (match (peek-char in-port)
           [#\> '()]
           [_ (cons (syrup-read in-port) (lp))])))
     (record label args)]
    [#\F
     (read-byte in-port)
     (floating-point-bytes->real (read-bytes 4 in-port) #t)]
    [#\D
     (read-byte in-port)
     (floating-point-bytes->real (read-bytes 8 in-port) #t)]
    [#\t
     (read-byte in-port)
     #t]
    [#\f
     (read-byte in-port)
     #f]
    [#\s
     (read-byte in-port)
     (let lp ([s (set)])
       (match (peek-char in-port)
         [#\e
          (read-byte in-port)
          s]
         [_
          (lp (set-add s (syrup-read in-port)))]))]
    [_
     (error 'syrup-invalid-char "Unexpected character at position ~a: ~a"
            (file-position in-port)
            (peek-char in-port))]))

(define (syrup-decode bstr)
  (syrup-read (open-input-bytes bstr)))


(module+ test
  (require rackunit)

  (define zoo-structure
    (record* #"zoo"
             "The Grand Menagerie"
             `(#hash((species . #"cat")
                     (name . "Tabatha")
                     (age . 12)
                     (weight . 8.2)
                     (alive? . #t)
                     (eats . ,(set #"mice" #"fish" #"kibble")))
               #hash((species . #"monkey")
                     (name . "George")
                     (age . 6)
                     (weight . 17.24)
                     (alive? . #f)
                     (eats . ,(set #"bananas" #"insects"))))))

  (define zoo-expected-bytes
    #"<3:zoo\"19:The Grand Menagerie({'3:agei12e'4:eatss4:fish4:mice6:kibblee'4:name\"7:Tabatha'6:alive?t'6:weightD@ ffffff'7:species3:cat}{'3:agei6e'4:eatss7:bananas7:insectse'4:name\"6:George'6:alive?f'6:weightD@1=p\243\327\n='7:species6:monkey})>")
  (test-equal?
   "Correctly encodes zoo structure"
   (syrup-encode zoo-structure)
   zoo-expected-bytes)
  
  (test-equal?
   "Correctly decodes zoo structure"
   (syrup-decode zoo-expected-bytes)
   zoo-structure))
