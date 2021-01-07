;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |homework_9 (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Swasti Dadhich and Nop Lertsumitkul
; Homework 9

; Exercise 1


; word-counts : [List-of String] -> [List-of Nat]
; returns a LoN with the frequency of each element within the given LoS
(check-expect (word-counts (list)) '())
(check-expect (word-counts (list "I" "really" "really" "don't" "know"))
              (list 1 2 2 1 1))
(check-expect (word-counts (list "a" "a" "a" "z" "zz" "a"))
              (list 4 4 4 1 1 4))
(check-expect (word-counts (list "Soccer" "is" "really" "really" "fun" "!" "!"))
              (list 1 1 2 2 1 2 2))

(define (word-counts los)
  (local [; frequency : String -> Nat
          ; returns the frequency of the particular element in los
          ; if s = "I", and los = (list "I") -> 1
          ; if s = "a", and los = (list "a" "a" "b") -> 2
          (define (frequency s)
            (local [; same? : String -> Boolean
                    ; determines whether the given String is equal to the
                    ; particular element in los
                    ; if s2 = "I", and s = "I" -> #t
                    ; if s2 = "z", and s = "a" -> #f
                    (define (same? s2)
                      (string=? s s2))]
              ; ([String -> Boolean] [List-of String]) -> Nat 
              (length (filter same? los))))]
    ; [String -> Nat][List-of String] -> [List-of Nat]
    (map frequency los)))
; (a)

; [TwoList X Y] is
; - (list X Y)
; a list of size 2

(define TL-0 '())
(define TL-1 (list 1 2))
(define TL-2 (list "one" "two"))
(define (tl-temp tl)
  (... (first tl)
       ... (second tl)))

; zip : [List-of X] [List-of X] -> [List-of TwoList]

(check-expect (zip (list 1 3 5 7) (list 2 4 6)) (list (list 1 2) (list 3 4) (list 5 6)))
(check-expect (zip (list "one" "three" "five" "seven") (list "two" "four" "six"))
              (list (list "one" "two") (list "three" "four") (list "five" "six")))
(check-expect (zip (list 1 3 5 7) '()) '())

(define (zip l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) '()]
    [(and (list? l1) (list? l2)) (append (list (list (first l1) (first l2)))
                                         (zip (rest l1) (rest l2)))]))

; (b)
; remove-dups : [List-of X] [X -> Boolean] -> [List-of X]
; returns a list without the duplicates

(check-expect (remove-dups (list 1 1 2 3) =) (list 1 2 3))
(check-expect (remove-dups (list 1 1 2 3 3) =) (list 1 2 3))
(check-expect (remove-dups (list "Quarantine" "is" "really" "really" "fun" "!" "!") string=?)
              (list "Quarantine" "is" "really" "fun" "!"))
(check-expect (remove-dups (list #t #t #f) boolean=?) (list #t #f))
(check-expect (remove-dups (list 10 7 2) =) (list 10 7 2))
(check-expect (remove-dups (list "Corona" "Check") string=?) (list "Corona" "Check"))
(check-expect (remove-dups '() =) '())

(define (remove-dups lox compar)
  (cond
    [(empty? lox) lox]
    [(cons? lox) (if
                  (repeat? (first lox) (rest lox) compar)
                  (remove-dups (rest lox) compar)
                  (cons (first lox) (remove-dups (rest lox) compar)))]))

; repeat? : (X) X [List-of X] [X X -> Boolean] -> Boolean
; returns the #t if x is repeated in the list 

(define (repeat? x lox compar)
  (ormap (λ (check) (compar check x)) lox))

;------------Unsimplified version of helper---------------   
;(define (repeat? x lox compar)
;  (cond
;    [(empty? lox) #f]
;    [(cons? lox) (or (compar (first lox) x)
;                     #t
;                     (repeat? x (rest lox) compar))]))

  
; (c)
; words-counts-neat : [List-of String] -> [TwoList String Nat]
; returns the result of zipping the input list and the output list

(check-expect (words-counts-neat '()) '())
(check-expect (words-counts-neat (list "hi" "hi" "bye")) (list (list "hi" 2) (list "bye" 1)))


(define (words-counts-neat los)
  (remove-dups (zip los (word-counts los)) twolist=?))


; twolist=? : [TwoList String Nat] [TwoList String Nat] -> Boolean
; returns #t if the 2 lists are the same

(check-expect (twolist=? (list "hi" 2) (list "bye" 1)) #f)
(check-expect (twolist=? (list "hi" 2) (list "hi" 2)) #t)

(define (twolist=? l1 l2)
  (and 
   (string=? (first l1) (first l2))
   (= (second l1) (second l2))))
  
;-----------------------------------------------------------------------------------------------------
; Exercise 2

; A [ZOM-List-of X] is one of:
; - '()
; - (cons X '())
; - (cons X (cons X [ZOM-List-of X]))
; repr. a list with zero, one, or more elements.

; Data defintion

(define ZOM-1 '())
(define ZOM-2 (cons 1 '()))
(define ZOM-3 (cons 1 (cons 2 '())))

(define (zomlox-templ loa)
  (...
   (cond
     [(empty? loa)...]
     [(empty? (rest loa))...(first loa)]
     [(cons? (rest loa))...(first loa)
                        ...(second loa)
                        ...(zomlox-templ (rest (rest loa)))])))


; An example of X could be number and a list that is not a [ZOM-List-of X]- could be
; [ZOM-List-of String]

; (b)

; len: [ZOM-List-of X] -> PosNumber
; returns the length of the inputed list

(check-expect (len ZOM-1) 0)
(check-expect (len ZOM-2) 1)
(check-expect (len ZOM-3) 2)
(check-expect (len (cons 3 (cons 1 (cons 2 '())))) 3)

(define COUNT (cons 0 '()))

(define (len loa)
  (cond
    [(empty? loa) 0]
    [(empty? (rest loa)) 1]
    [(cons? (rest loa)) (add1 (+ 1 (len (rest (rest loa)))))]))

; The simplified version can be written by removing (rest loa) after cons? and just using loa.
; (define (len loa)
;  (cond
;     [(empty? loa) 0]
;     [(cons? loa) (+ 1 (len (rest loa)))]))

; (c)

; flip-neighbor: [List-of Any] -> [List-of Any]
; returns the fliped version of the neighboring elements in the inputed list

(check-expect (flip-neighbor (list 1 "a" 2 "b")) (list "a" 1 "b" 2))
(check-expect (flip-neighbor (list 1 "a" 2 "b" 3)) (list "a" 1 "b" 2 3))
(check-expect (flip-neighbor (list 1 "a" 2 "b" 3 "c")) (list "a" 1 "b" 2 "c" 3))
(check-expect (flip-neighbor (list 1 "a" 2 "b" 3 "c" 4)) (list "a" 1 "b" 2 "c" 3 4))

(define (flip-neighbor loa)
  (cond
    [(empty? loa) loa]
    [(and (list? loa) (= (length loa) 1)) (append loa)]
    [(list? loa) (append (list (second loa) (first loa)) (flip-neighbor (rest (rest loa))))]))

;-----------------------------------------------------------------------------------------------------
; Exercise 3

; A [Checklist Z] is a [List-of [Z -> Boolean]]
; repr. a list of tests (functions that test some property of a Z)
(define CL-0 (list even? odd? zero?)) ; a [Checklist Nat]
; (we don't need to define a template since our DD
; is of the form [List-of ...])


; checklist->predicate : [Checklist Z] -> [Z -> Boolean]
; returns #t if all the predicates in the checklist return #t


(define CL-1 (list even? zero?))

(check-expect ((checklist->predicate CL-0) 3) #f)
(check-expect ((checklist->predicate CL-0) 0) #f)
(check-expect ((checklist->predicate CL-1) 0) #t)

; single-predicate: takes a fuction which takes in fuction)) -> Boolean

(define (checklist->predicate checklist)
  (λ (x) (andmap (λ (single-predicate?) (single-predicate? x)) checklist)))


                    








