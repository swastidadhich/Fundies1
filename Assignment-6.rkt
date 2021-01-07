;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Homework 6.0|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; Exericse 1

; A Bit is one of
; - "0"
; - "1"
; repr. a bit.
(define B-0 "0")
(define B-1 "1")
#;
(define (bit-templ b)
  (cond [(string=? b "0") ...]
        [(string=? b "1") ...]))
 
; A BitString is a String over Bits
; repr. a bitvector (which is a technical term).
(define BS-0 "")
(define BS-1 "001")
(define BS-2 "101")
(define BS-3 "00")

#;
(define (bitstring-templ bs)
  (... bs ...))

; the template below is for my understanding 

; A LOB (List of Boolean) is one of
; - '()
; - (cons Boolean LOB)
; and represents a list of booleans

(define LOB-0 '())
(define LOB-1 (cons #f LOB-0))
(define LOB-2 (cons #f LOB-1))
(define LOB-3 (cons #t LOB-2))

#|
(define (lob-temp lob)
  (foldr
   (cond
     [(empty? lob)...]
     [(cons? lob)...(first lob)
                 (lob-temp (rest lob))...])))
|#

; a)
; bl2bs: [List-of Boolean] -> BitString
; returns a corresponding BitString in which #t becomes "1" AND #f becomes "0"
(check-expect (bl2bs LOB-0) BS-0)
(check-expect (bl2bs LOB-2) BS-3)
(check-expect (bl2bs LOB-3) BS-1)

(define (bl2bs lob)
  (foldr change-the-order "" (map b2bit lob)))

; change-the-order: Bit BitString -> BitString
; places the BitString before Bit and appends it together 
(check-expect (change-the-order B-0 BS-3) "000")
(check-expect (change-the-order B-1 BS-1) "0011")

(define (change-the-order b bs)
  (string-append bs b))

; b2bit: Boolean -> Bit
; returns if the inputed 
(check-expect (b2bit #f) "0")
(check-expect (b2bit #t) "1")

(define (b2bit b)
  (if b
      "1"
      "0"))
 
; b)
; bs2bl: BitString -> [List-of Boolean]
; returns a list of boolean given the corresponding inputed BitString
(check-expect (bs2bl BS-0) LOB-0)
(check-expect (bs2bl BS-3) LOB-2)

(define (bs2bl bs)
  (map bit->boolean (explode bs)))
  

; bit->boolean: Bit -> Boolean
; converts the inputed bit to a boolean
(check-expect (bit->boolean B-1) #t)
(check-expect (bit->boolean B-0) #f)

(define (bit->boolean b)
  (string=? b "1"))
 
; c)
; list=?: [List-of X] [List-of X] [X X -> Boolean] -> Boolean
; return #t if the two lists are equal
(check-expect (list=? '()'() string=?)  #t)
(check-expect (list=? (list 1 2 3) (list 1 2 3) =) #t)
(check-expect (list=? (list #t #f #t) (list #f #t) boolean=?) #f)

(define (list=? l1 l2 is=?)
  (cond [(empty? l1) (empty? l2)]
        [(cons?  l1) (and (cons? l2)
                          (is=? (first l1) (first l2))
                          (list=? (rest l1) (rest l2) is=?))]))

; d)
; test-double-conv: [List-of Boolean] -> [List-of Boolean]
; returns #t if the intial list of boolean is the same after it has been coverted to a BitString
(check-expect (test-double-conv LOB-0) #t)
(check-expect (test-double-conv LOB-1) #t)
(check-expect (test-double-conv LOB-0) #t)

(define (test-double-conv lob)
  (list=? (bs2bl (bl2bs lob)) lob boolean=?))


; Exercise 2

; A ListOfStrings (LoS) is one of:
; - empty
; - (cons string LoS)
; represents a list of strings

(define LOS0 empty)
(define LOS1 (cons "apple" LOS0))
(define LOS2 (cons "banana" LOS1))
(define LOS3 (cons "mango" LOS2))
(define LOS4 (cons "pear" LOS3))
(define LOS5 (cons "banana" LOS4))
(define LOS6 (cons "mango" LOS5))
(define LOS7 (cons "mango" LOS6))

(define (los-temp los)
  (...
   (cond
     [(empty? los) ...]
     [(cons? los)
      (first los)
      (los-temp (rest los))])))
;a)
; word-counts : [List-of String] -> [List-of Number]
; calculates for each word the frequency which it occurs in the list
(check-expect (word-counts (list "I" "really" "really" "don't" "know")) (list 1 2 2 1 1))
(check-expect (word-counts (list "I" "I" "really" "I" "know")) (list 3 3 1 3 1))
(check-expect (word-counts '()) '())

(define (word-counts los)
  (frequency-list los los))

; frequency-list : [List-of String] [List-of String] -> [List-of Number]
; returns for each string in the list how many times it occurs
(check-expect (frequency-list (list "I" "really" "really" "don't" "know")
                              (list "I" "really" "really" "don't" "know")) (list 1 2 2 1 1))
(check-expect (frequency-list (list "I" "I" "really" "I" "know")
                              (list "I" "I" "really" "I" "know")) (list 3 3 1 3 1))
(check-expect (frequency-list '()'()) '())
                         
(define (frequency-list los1 los2)
  (cond
    [(empty? los1) '()]
    [(cons? los1)
     (cons (frequency-string (first los1) los2)
           (frequency-list (rest los1) los2))]))

; frequency-string : String [List-of String] -> Number
; returns how many times a given string occurs in a list
(check-expect (frequency-string "I" (list "I" "really" "really" "don't" "know")) 1)
(check-expect (frequency-string "really" (list "I" "really" "really" "don't" "know")) 2)
(check-expect (frequency-string "really" '()) 0)

(define (frequency-string s los)
  (cond
    [(empty? los) 0]
    [(cons? los)
     (if (string=? s (first los))
         (add1 (frequency-string s (rest los)))
         (frequency-string s (rest los)))]))

; b)
; char-counts : String -> [List-of Number]
; returns a list of the number of times a char occurs in a string
(check-expect (char-counts "really") (list 1 1 1 2 2 1))
(check-expect (char-counts "drill") (list 1 1 1 2 2))

(define (char-counts s)
  (char-counts-help (explode s) (explode s)))

; char-counts-help : [List-of String] [List-of String]-> [List-of Number]
; returns a list of the number of times a char occurs in a list of strings
(check-expect (char-counts-help (list "r" "e" "a" "l" "l" "y")
                                (list "r" "e" "a" "l" "l" "y")) (list 1 1 1 2 2 1))
(check-expect (char-counts-help (list "d" "r" "i" "l" "l")
                                (list "d" "r" "i" "l" "l")) (list 1 1 1 2 2))

(define (char-counts-help los1 los2)
  (cond
    [(empty? los1) '()] 
    [(cons? los1)
     (cons (frequency-string (first los1) los2)
           (char-counts-help (rest los1) los2))]))

; c)

; counts : (X) [List-of X] is=? -> [List-of Number]
; calculates the frequency in which an element occurs in a list
(check-expect (counts (list 1 2 3 2 1) =) (list 2 2 1 2 2))
(check-expect (counts (list "d" "r" "i" "l" "l") string=?) (list 1 1 1 2 2))

(define (counts lox is=?)
  (frequency-lox lox lox is=?))

; frequency-lox : (X Y) [List-of X] [List-of X] [X -> Y] -> [List-of X]
; returns for each string in the list how many times it occurs
(check-expect (frequency-lox (list "I" "really" "really" "don't" "know")
                             (list "I" "really" "really" "don't" "know") string=?) (list 1 2 2 1 1))
(check-expect (frequency-lox (list 1 2 3 4)
                             (list 1 2 3 4) =) (list 1 1 1 1))
(check-expect (frequency-lox '()'() =) '())
(check-expect (frequency-lox (list true true false) (list true true) boolean=?) (list 2 2 0))

(define (frequency-lox lox1 lox2 is=?)
  (cond
    [(empty? lox1) '()]
    [(cons? lox1)
     (cons (frequency-x (first lox1) lox2 is=?)
           (frequency-lox (rest lox1) lox2 is=?))]))

; frequency-x : (X Y) X [List-of X] (X -> Y) -> Number
; returns how many times a given element occurs in a list
(check-expect (frequency-x "I" (list "I" "really" "really" "don't" "know") string=?) 1)
(check-expect (frequency-x 1 (list 1 2 3 1) =) 2)

(define (frequency-x x lox is=?)
  (cond
    [(empty? lox) 0]
    [(cons? lox)
     (if (is=? x (first lox))
         (add1 (frequency-x x (rest lox) is=?))
         (frequency-x x (rest lox) is=?))]))



  





