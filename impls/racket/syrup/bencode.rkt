#lang racket/base

(require racket/match
         racket/set)

(define (bencode-encode obj)
  (match obj
    ;; Bytes are like <bytes-len>:<bytes>
    [(? bytes?)
     (bytes-append (string->bytes/latin-1 (number->string (bytes-length obj)))
                   #":"
                   obj)]
    ;; Integers are like i<maybe-signed-integer>e
    [(? integer?)
     (bytes-append #"i" (string->bytes/latin-1 (number->string obj)) #"e")]
    ;; Lists are like l<item1><item2><item3>e
    [(? list?)
     (bytes-append #"l"
                   (apply bytes-append
                          (map bencode-encode obj))
                   #"e")]
    ;; Dictionaries are like d<key1><val1><key2><val2>e
    [(? hash?)
     (define sorted-keys
       (sort (hash-keys obj) bytes<?))
     (define encoded-hash-pairs
       (for/list ([key sorted-keys])
         (define val
           (hash-ref obj key))
         (bytes-append (bencode-encode key)
                       (bencode-encode val))))
     (bytes-append #"d"
                   (apply bytes-append encoded-hash-pairs)
                   #"e")]
    ;; wtf is this
    [_ (error 'bencode-unsupported-type obj)]))

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

(define (bencode-read in-port)
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
    [#\l
     (read-byte in-port)
     (let lp ()
       (match (peek-char in-port)
         ;; We've reached the end
         [#\e
          (read-byte in-port)
          '()]
         ;; one more loop
         [_
          (cons (bencode-read in-port) (lp))]))]
    [#\d
     (read-byte in-port)
     (let lp ([ht #hash()])
       (match (peek-char in-port)
         [#\e
          (read-byte in-port)
          ht]
         [_
          (define key
            (bencode-read in-port))
          (unless (bytes? key)
            (error 'bencode-key-not-string key))
          (define val
            (bencode-read in-port))
          (lp (hash-set ht key val))]))]
    [_
     (error 'bencode-invalid-char "Unexpected character at position ~a: ~a"
            (file-position in-port)
            (peek-char in-port))]))

(define (bencode-decode bstr)
  (bencode-read (open-input-bytes bstr)))


(module+ test
  (require rackunit)

  (define zoo-structure
    '(#"zoo"
      #hash((#"species" . #"cat")
            (#"name" . #"Tabatha")
            (#"age" . 12)
            (#"eats" . (#"mice" #"fish" #"kibble")))
      #hash((#"species" . #"monkey")
            (#"name" . #"George")
            (#"age" . 6)
            (#"eats" . (#"bananas" #"insects")))))

  (define zoo-expected-bytes
    #"l3:zood3:agei12e4:eatsl4:mice4:fish6:kibblee4:name7:Tabatha7:species3:cated3:agei6e4:eatsl7:bananas7:insectse4:name6:George7:species6:monkeyee")
  (test-equal?
   "Correctly encodes zoo structure"
   (bencode-encode zoo-structure)
   zoo-expected-bytes)
  
  (test-equal?
   "Correctly decodes zoo structure"
   (bencode-decode zoo-expected-bytes)
   zoo-structure))
