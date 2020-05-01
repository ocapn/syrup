#lang racket/base

(provide record record*
         syrup-encode syrup-decode
         syrup-read)

(require racket/match
         racket/set)

(struct record (label args)
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

(define (syrup-encode obj)
  (match obj
    ;; Bytes are like <bytes-len>:<bytes>
    [(? bytes?)
     (netstring-encode obj)]
    ;; Integers are like i<maybe-signed-integer>e
    [(? integer?)
     (bytes-append #"i" (string->bytes/latin-1 (number->string obj)) #"e")]
    ;; Lists are like (<item1><item2><item3>)
    [(? list?)
     (bytes-append #"("
                   (apply bytes-append
                          (map syrup-encode obj))
                   #")")]
    ;; Dictionaries are like {<key1><val1><key2><val2>}
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
                   (syrup-encode (record-label obj))
                   (apply bytes-append
                          (map syrup-encode (record-args obj)))
                   #">")]
    ;; #t is t, #f is f
    [#t #"t"]
    [#f #"f"]
    ;; Sets are like #<item1><item2><item3>$
    [(? set?)
     (define encoded-items
       (for/list ([item obj])
         (syrup-encode item)))
     (define sorted-items
       (sort encoded-items
             bytes<?))
     (bytes-append #"#"
                   (apply bytes-append sorted-items)
                   #"$")]
    [_ (error 'syrup-unsupported-type obj)]))

(define digit-chars
  (seteq #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define whitespace-chars
  (seteq #\space #\tab #\newline))

(define (digit-char? char)
  (set-member? digit-chars char))

(define (syrup-read in-port)
  ;; consume whitespace
  (let lp ()
    (when (set-member? whitespace-chars (peek-char in-port))
      (read-char in-port)
      (lp)))

  (match (peek-char in-port)
    ;; it's either a bytestring, a symbol, or a string...
    ;; we tell via the divider
    [(? digit-char?)
     (define type #f)
     (define bytes-len
       (string->number
        (list->string
         (let lp ()
           (match (read-char in-port)
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
    [(or #\[ #\( #\l)
     (read-byte in-port)
     (let lp ()
       (match (peek-char in-port)
         ;; We've reached the end
         [(or #\] #\) #\e)
          (read-byte in-port)
          '()]
         ;; one more loop
         [_
          (cons (syrup-read in-port) (lp))]))]
    ;; it's a hashmap/dictionary
    [(or #\{ #\d)
     (read-byte in-port)
     (let lp ([ht #hash()])
       (match (peek-char in-port)
         [(or #\} #\e)
          (read-byte in-port)
          ht]
         [_
          (define key
            (syrup-read in-port))
          (define val
            (syrup-read in-port))
          (lp (hash-set ht key val))]))]
    ;; it's a record
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
    ;; it's a single float
    [#\F
     (read-byte in-port)
     (floating-point-bytes->real (read-bytes 4 in-port) #t)]
    ;; it's a double float
    [#\D
     (read-byte in-port)
     (floating-point-bytes->real (read-bytes 8 in-port) #t)]
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
       (match (peek-char in-port)
         [#\$
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
    #"<3:zoo19\"The Grand Menagerie({3'agei12e4'eats#4:fish4:mice6:kibble$4'name7\"Tabatha6'alive?t6'weightD@ ffffff7'species3:cat}{3'agei6e4'eats#7:bananas7:insects$4'name6\"George6'alive?f6'weightD@1=p\243\327\n=7'species6:monkey})>")
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
       ({3'age i12e
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
        7'species 6:monkey})>")
   zoo-structure)

  (test-equal?
   "csexp backwards compat"
   (syrup-decode #"(3:zoo (3:cat 7:tabatha))")
   '(#"zoo" (#"cat" #"tabatha")))

  (test-equal?
   "bencode backwards compat"
   (syrup-decode #"l3:zood4:name3:cat7:species7:tabathaee")
   '(#"zoo" #hash((#"name" . #"cat") (#"species" . #"tabatha")))))
