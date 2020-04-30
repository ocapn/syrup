#lang racket/base

(require racket/match
         racket/set)

(define (netstring-encode bstr)
  (bytes-append (string->bytes/latin-1 (number->string (bytes-length bstr)))
                #":"
                bstr))

;; Booleans: #"t" or #"f"
;; Float: ???
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
    [(? hash?)
     (define sorted-keys
       (sort (hash-keys obj) bytes<?))
     (define encoded-hash-pairs
       (for/list ([key sorted-keys])
         (define val
           (hash-ref obj key))
         (bytes-append (syrup-encode key)
                       (syrup-encode val))))
     (bytes-append #"{"
                   (apply bytes-append encoded-hash-pairs)
                   #"}")]
    [(? string?)
     (bytes-append #"\""
                   (netstring-encode (string->bytes/utf-8 obj)))]
    [(? symbol?)
     (bytes-append #"S")
     ]
    ;; wtf is this
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
                 (lp))])))))
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
                    (lp))])))))
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
          (unless (bytes? key)
            (error 'syrup-key-not-string key))
          (define val
            (syrup-read in-port))
          (lp (hash-set ht key val))]))]
    [#\"
     (read-byte in-port)
     (bytes->string/utf-8 (read-netstring in-port))]
    [_
     (error 'syrup-invalid-char "Unexpected character at position ~a: ~a"
            (file-position in-port)
            (peek-char in-port))]))

(define (syrup-decode bstr)
  (syrup-read (open-input-bytes bstr)))


(module+ test
  (require rackunit)

  (define zoo-structure
    '(#"zoo"
      #hash((#"species" . #"cat")
            (#"name" . "Tabatha")
            (#"age" . 12)
            (#"eats" . (#"mice" #"fish" #"kibble")))
      #hash((#"species" . #"monkey")
            (#"name" . "George")
            (#"age" . 6)
            (#"eats" . (#"bananas" #"insects")))))

  (define zoo-expected-bytes
    #"(3:zoo{3:agei12e4:eats(4:mice4:fish6:kibble)4:name\"7:Tabatha7:species3:cat}{3:agei6e4:eats(7:bananas7:insects)4:name\"6:George7:species6:monkey})")
  (test-equal?
   "Correctly encodes zoo structure"
   (syrup-encode zoo-structure)
   zoo-expected-bytes)
  
  (test-equal?
   "Correctly decodes zoo structure"
   (syrup-decode zoo-expected-bytes)
   zoo-structure))
