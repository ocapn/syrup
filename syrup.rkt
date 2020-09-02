#lang racket/base

(provide (struct-out record)
         record*
         syrup-encode syrup-decode
         syrup-read syrup-write)

(require racket/match
         racket/set
         racket/contract
         racket/struct)

;; For whatever reason struct/contract is dramatically faster
;; than using #:guard on a struct, but then I can't use #:methods
;; to define a custom printer... :\
(struct/contract record
  ([label any/c]
   [args list?])
  #:transparent)

(define (record* label . args)
  (record label args))

(define (netstring-encode bstr #:joiner [joiner #":"])
  (bytes-append (string->bytes/latin-1 (number->string (bytes-length bstr)))
                joiner
                bstr))

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

(define (syrup-encode obj
                      ;; an alist of (predicate . marshaller)... translates unknown
                      ;; object into a representation we can understand
                      #:marshallers [marshallers '()])
  (define (encode obj)
    (match obj
      ;; Bytes are like <bytes-len>:<bytes>
      [(? bytes?)
       (netstring-encode obj)]
      ;; Integers are like i<maybe-signed-integer>e
      [(? integer?)
       (bytes-append #"i" (string->bytes/latin-1 (number->string obj)) #"e")]
      ;; Lists are like [<item1><item2><item3>]
      [(? list?)
       (bytes-append #"["
                     (apply bytes-append
                            (map encode obj))
                     #"]")]
      ;; Dictionaries are like {<key1><val1><key2><val2>}
      ;; We sort by the key being fully encoded.
      [(? hash?)
       (define keys-and-encoded
         (for/list ([key (hash-keys obj)])
           (cons (encode key) key)))
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
              (bytes-append enc-key (encode val))])))
       (bytes-append #"{"
                     (apply bytes-append encoded-hash-pairs)
                     #"}")]
      ;; Strings are like <encoded-bytes-len>"<utf8-encoded>
      [(? string?)
       (netstring-encode (string->bytes/utf-8 obj)
                         #:joiner #"\"")]
      ;; Symbols are like <encoded-bytes-len>'<utf8-encoded>
      [(? symbol?)
       (netstring-encode (string->bytes/utf-8
                          (symbol->string obj))
                         #:joiner #"'")]
      ;; Single flonum floats are like F<big-endian-encoded-single-float>
      [(? single-flonum?)
       (bytes-append #"F"
                     (real->floating-point-bytes obj 4 #t))]
      ;; Double flonum floats are like D<big-endian-encoded-double-float>
      [(? double-flonum?)
       (bytes-append #"D"
                     (real->floating-point-bytes obj 8 #t))]
      ;; Records are like <<tag><arg1><arg2>> but with the outer <> for realsies
      [(? record?)
       (bytes-append #"<"
                     (encode (record-label obj))
                     (apply bytes-append
                            (map encode (record-args obj)))
                     #">")]
      ;; #t is t, #f is f
      [#t #"t"]
      [#f #"f"]
      ;; Sets are like #<item1><item2><item3>$
      [(? set?)
       (define encoded-items
         (for/list ([item obj])
           (encode item)))
       (define sorted-items
         (sort encoded-items
               bytes<?))
       (bytes-append #"#"
                     (apply bytes-append sorted-items)
                     #"$")]
      [_
       (call/ec
        (lambda (return)
          (for ([marshaller marshallers])
            (match marshaller
              [(cons handles-it? translate)
               (when (handles-it? obj)
                 (define translated (translate obj))
                 (if (record? translated)
                     (return (encode translated))
                     (error 'syrup-marshaller-returned-unsupported-type)))]))
          (error 'syrup-unsupported-type
                 "~a"
                 obj)))]))
  (encode obj))

(define (syrup-write obj op #:marshallers [marshallers '()])
  (write-bytes (syrup-encode obj #:marshallers marshallers)
               op))

(define digit-chars
  (seteq #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define whitespace-chars
  (seteq #\space #\tab #\newline))

(define (digit-char? char)
  (set-member? digit-chars char))

(define (syrup-read in-port
                    ;; inverse of syrup-encode's marshallers;
                    ;; alist of (label-pred? . derecorify)
                    #:unmarshallers [unmarshallers '()])
  (call/ec
   (lambda (return-early)
     (define (return-eof)
       (return-early eof))
     (define (_read-char)
       (match (read-char in-port)
         [(? eof-object?) (return-eof)]
         [char char]))
     (define (_peek-char)
       (match (peek-char in-port)
         [(? eof-object?) (return-eof)]
         [char char]))
     (define (read-next)
       ;; consume whitespace
       (let lp ()
         (when (set-member? whitespace-chars (_peek-char))
           (_read-char)
           (lp)))

       (match (_peek-char)
         ;; it's either a bytestring, a symbol, or a string...
         ;; we tell via the divider
         [(? digit-char?)
          (define type #f)
          (define bytes-len
            (string->number
             (list->string
              (let lp ()
                (match (_read-char)
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
                          "Invalid digit at pos ~a: ~a"
                          (file-position in-port)
                          other-char)])))))
          (define bstr
            (read-bytes bytes-len in-port))
          (match type
            ['bstr
             bstr]
            ['sym
             (string->symbol (bytes->string/utf-8 bstr))]
            ['str
             (bytes->string/utf-8 bstr)])]
         ;; it's an integer
         [#\i
          (read-byte in-port)
          (define negative?
            (if (eq? (_peek-char) #\-)
                (begin
                  (read-byte in-port)
                  #t)
                #f))
          (define num
            (string->number
             (list->string
              (let lp ()
                ;; TODO: more error handling here
                (match (_read-char)
                  [#\e
                   '()]
                  [(? digit-char? digit-char)
                   (cons digit-char
                         (lp))]
                  [other-char
                   (error 'syrup-invalid-digit
                          "Invalid digit at pos ~a: ~a"
                          (sub1 (file-position in-port))
                          other-char)])))))
          (if negative?
              (* num -1)
              num)]
         ;; it's a list
         [(or #\[ #\( #\l)
          (read-byte in-port)
          (let lp ()
            (match (_peek-char)
              ;; We've reached the end
              [(or #\] #\) #\e)
               (read-byte in-port)
               '()]
              ;; one more loop
              [_
               (cons (read-next) (lp))]))]
         ;; it's a hashmap/dictionary
         [(or #\{ #\d)
          (read-byte in-port)
          (let lp ([ht #hash()])
            (match (_peek-char)
              [(or #\} #\e)
               (read-byte in-port)
               ht]
              [_
               (define key
                 (read-next))
               (define val
                 (read-next))
               (lp (hash-set ht key val))]))]
         ;; it's a record
         [#\<
          (read-byte in-port)
          (define label
            (read-next))
          (define args
            (let lp ()
              (match (_peek-char)
                [#\>
                 (read-byte in-port)
                 '()]
                [_ (cons (read-next) (lp))])))
          (call/ec
           (lambda (return)
             (for ([unmarshaller unmarshallers])
               (match unmarshaller
                 [(cons (and (? (or/c symbol? string? number? boolean? bytes?))
                             expected-label)
                        derecordify)
                  (when (equal? label expected-label)
                    (return (apply derecordify args)))]
                 [(cons label-pred? derecordify)
                  (when (label-pred? label)
                    (return (apply derecordify args)))]))
             ;; no handler, return as record
             (record label args)))]
         ;; it's a single float
         [#\F
          (read-byte in-port)
          (let ([val (floating-point-bytes->real (read-bytes 4 in-port) #t)])
            (unless (real? val)
              (error 'not-a-real-number val))
            val)]
         ;; it's a double float
         [#\D
          (read-byte in-port)
          (let ([val (floating-point-bytes->real (read-bytes 8 in-port) #t)])
            (unless (real? val)
              (error 'not-a-real-number val))
            val)]
         ;; it's a boolean
         [#\t
          (read-byte in-port)
          #t]
         [#\f
          (read-byte in-port)
          #f]
         ;; it's a set
         [#\#
          (read-byte in-port)
          (let lp ([s (set)])
            (match (_peek-char)
              [#\$
               (read-byte in-port)
               s]
              [_
               (lp (set-add s (read-next)))]))]
         [_
          (error 'syrup-invalid-char "Unexpected character at position ~a: ~a"
                 (file-position in-port)
                 (_peek-char))]))
     (read-next))))

(define (syrup-decode bstr #:unmarshallers [unmarshallers '()])
  (syrup-read (open-input-bytes bstr)
              #:unmarshallers unmarshallers))

(module+ test
  (require rackunit
           racket/runtime-path
           racket/port)
  (define-runtime-path pwd
    ".")

  (test-equal?
   "eof anywhere in a syrup-read is an eof"
   (call-with-input-bytes
    #"[3:foo"
    (lambda (ip)
      (syrup-read ip)))
   eof)

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
    (call-with-input-file (build-path pwd "test-data" "zoo.bin")
      port->bytes))
  (test-equal?
   "Correctly encodes zoo structure"
   (syrup-encode zoo-structure)
   zoo-expected-bytes)
  
  (test-equal?
   "Correctly decodes zoo structure"
   (syrup-decode zoo-expected-bytes)
   zoo-structure)

  (test-equal?
   "Ignore whitespace"
   (syrup-decode #"
<3:zoo 19\"The Grand Menagerie
       [{3'age i12e
         4'eats #4:fish
                 4:mice
                 6:kibble$
         4'name 7\"Tabatha
         6'alive? t
         6'weight D@ ffffff
         7'species 3:cat}
        {3'age i6e
         4'eats #7:bananas
                 7:insects$
         4'name 6\"George
         6'alive? f
         6'weight D@1=p\243\327\n=
         7'species 6:monkey}]>")
   zoo-structure)

  (test-equal?
   "csexp backwards compat"
   (syrup-decode #"(3:zoo (3:cat 7:tabatha))")
   '(#"zoo" (#"cat" #"tabatha")))

  (test-equal?
   "bencode backwards compat"
   (syrup-decode #"l3:zood4:name3:cat7:species7:tabathaee")
   '(#"zoo" #hash((#"name" . #"cat") (#"species" . #"tabatha"))))

  (struct fizzbuzz (blorp blap)
    #:transparent)
  (define (fizzbuzz->record fb)
    (record* 'fizzbuzz (fizzbuzz-blorp fb) (fizzbuzz-blap fb)))
  
  (test-equal?
   "marshaller works"
   (syrup-encode (list 'meep 'moop (fizzbuzz 'fizzy 'water) 'bop)
                 #:marshallers (list (cons fizzbuzz? fizzbuzz->record)))
   #"[4'meep4'moop<8'fizzbuzz5'fizzy5'water>3'bop]")

  (test-equal?
   "unmarshaller works"
   (syrup-decode #"[4'meep4'moop<8'fizzbuzz5'fizzy5'water>3'bop]"
                 #:unmarshallers (list (cons 'fizzbuzz fizzbuzz)))
   (list 'meep 'moop (fizzbuzz 'fizzy 'water) 'bop)))
