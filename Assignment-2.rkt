;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
; Exercise 1
; subset-interval? : RealNum -> Boolean
; returns true if every element of the first interval is also an element of the second interval

(check-expect (subset-interval? 1 2 3 4) #f)
(check-expect (subset-interval? 1 10 3 4) #f)
(check-expect (subset-interval? 1 5 1 5) #t)
(check-expect (subset-interval? 4 5 1 6) #t)


(define (subset-interval? a b c d)
  (and (<= c a) (>= d b)))


; Exercise 2
; sort-three : Num Num Num -> String
; returns 3 numbers in a sorted manner

(check-expect (sort-three 1 2 3) "[1,2,3]")
(check-expect (sort-three 4 2 6) "[2,4,6]")
(check-expect (sort-three -1 0 -10) "[-10,-1,0]")
(check-expect (sort-three 3 2 1) "[1,2,3]")
(check-expect (sort-three 4 5 3) "[3,4,5]")
(check-expect (sort-three 2 4 3) "[2,3,4]")
(check-expect (sort-three 2 3 1) "[1,2,3]")


(define (sort-three n1 n2 n3)
  (cond
    [(and (< n1 n2) (< n2 n3)) (3nums->string n1 n2 n3)]
    [(and (< n2 n3) (< n3 n1)) (3nums->string n2 n3 n1)]
    [(and (< n3 n2) (< n2 n1)) (3nums->string n3 n2 n1)]
    [(and (< n1 n3) (< n3 n2)) (3nums->string n1 n3 n2)]
    [(and (< n2 n1) (< n1 n3)) (3nums->string n2 n1 n3)]
    [(and (< n3 n1) (< n1 n2)) (3nums->string n3 n1 n2)]))



; 3nums->string : Num Num Num -> String
; returns a convered, appended and bracketed string given a number


(check-expect (3nums->string 1 2 3) "[1,2,3]")
(check-expect (3nums->string 3 2 1) "[3,2,1]")
(check-expect (3nums->string 0 -1 3) "[0,-1,3]")

(define (3nums->string n1 n2 n3)
  (string-append "[" (number->string n1) "," (number->string n2) "," (number->string n3) "]"))


; Exercise 3
; => : Boolean Boolean -> Boolean
; returns the truth value of the implication

(check-expect (=> #f #f) #t)
(check-expect (=> #f #t) #t)
(check-expect (=> #t #f) #f)
(check-expect (=> #t #t) #t)

(define (=> b1 b2)
  (if b1 b2 #t))



; Exercise 4

;A.

; CombinedMajor is one of:
; - "CS and Linguistics"
; - "CS and Biology"
; - "CS and Cognitive Psychology"
; - "CS and Mathematics"
; - "DS and Biochemistry"
; - "DS and Biology"
; - "DS and Ecology and Evolutionary Biology"
; - "DS and Mathematics"

; represents somne of the currently offered combined major by Khoury and college of science

;B.

(define M-0 "CS and Linguistics")
(define M-1 "CS and Biology")
(define M-2 "CS and Cognitive Psychology")
(define M-3 "DS and Biology")

;C.

(define (combinedmajor-temp cm)
  (....
   (cond
     [(string=? cm "CS and Linguistics") ...]
     [(string=? cm "CS and Biology")...]
     [(string=? cm "CS and Cognitive Psychology")...]
     [(string=? cm "CS and Mathematics")...]
     [(string=? cm "DS and Biochemistry")...]
     [(string=? cm "DS and Biology")...]
     [(string=? cm "DS and Ecology and Evolutionary Biology")...]
     [(string=? cm "DS and Mathematics")...])))
;D.

; cs-or-ds : CombinedMajor -> String
; returns a whether an inputed CombinedMajor is CS or DS

(check-expect (cs-or-ds "CS and Linguistics") "CS")
(check-expect (cs-or-ds "CS and Cognitive Psychology") "CS")
(check-expect (cs-or-ds "DS and Biology") "DS")
(check-expect (cs-or-ds "DS and Mathematics") "DS")
(check-expect (cs-or-ds "CS and Mathematics") "CS")
(check-expect (cs-or-ds "DS and Biochemistry") "DS")
(check-expect (cs-or-ds "CS and Biology") "CS")
(check-expect (cs-or-ds "DS and Ecology and Evolutionary Biology") "DS")


(define (cs-or-ds cm)
  (cond
    [(string=? cm "CS and Linguistics") "CS"]
    [(string=? cm "CS and Biology") "CS"]
    [(string=? cm "CS and Cognitive Psychology") "CS"]
    [(string=? cm "CS and Mathematics") "CS"]
    [(string=? cm "DS and Biochemistry") "DS"]
    [(string=? cm "DS and Biology") "DS"]
    [(string=? cm "DS and Ecology and Evolutionary Biology") "DS"]
    [(string=? cm "DS and Mathematics") "DS"]))

; cs-or-ds-v2 : CombinedMajor -> String
; returns a whether an inputed CombinedMajor is CS or DS

(check-expect (cs-or-ds-v2 "CS and Linguistics") "CS")
(check-expect (cs-or-ds-v2 "CS and Cognitive Psychology") "CS")
(check-expect (cs-or-ds-v2 "DS and Biology") "DS")
(check-expect (cs-or-ds-v2 "DS and Mathematics") "DS")
(check-expect (cs-or-ds-v2 "CS and Mathematics") "CS")
(check-expect (cs-or-ds-v2 "DS and Biochemistry") "DS")
(check-expect (cs-or-ds-v2 "CS and Biology") "CS")
(check-expect (cs-or-ds-v2 "DS and Ecology and Evolutionary Biology") "DS")

(define (cs-or-ds-v2 cm)
  (substring cm 0 2))


;E;

; alternative : CombinedMajor -> CombinedMajor
; returns combined major that combines a Khoury program (CS or DS) with the same non-Khoury program

(define ERROR "No alternative!")

(check-expect (alternative "CS and Mathematics") "DS and Mathematics")
(check-expect (alternative "CS and Biology") "DS and Biology")
(check-expect (alternative "DS and Biology") "CS and Biology")
(check-error (alternative "CS and Cognitive Psychology") ERROR)
(check-error (alternative "DS and Ecology and Evolutionary Biology") ERROR)

(define (alternative cm)
  (cond
    [(or (string=? cm  "CS and Mathematics")
         (string=? cm  "DS and Mathematics")
         (string=? cm  "CS and Biology")
         (string=? cm  "DS and Biology"))
     (string-append (if (string=? (cs-or-ds-v2 cm) "DS")
                        "CS"
                        "DS")
                    (substring cm 2))]
    [else (error ERROR)]))





              
; Exercise 5

(define BASE 2)
(define LIMIT 10000)

(define FONT-SIZE 12)
(define FONT-COLOR "black")
(define SCENE (rectangle 300 20 "solid" "yellow"))

; power-of-base : i -> Image
; displays the bas &power, upon keypress increases exponent by 1,stops when number exceeds limit

(define (powers-of-base i)
  (big-bang i
    [to-draw draw-screen]
    [on-key next-base]
    [stop-when stop-screen final-image]))

; draw-screen : i -> Image
; produces an image of the current world state

(check-expect (draw-screen 0)
              (overlay (text (number->string (expt BASE 0))
                             FONT-SIZE
                             FONT-COLOR)
                       SCENE))

(check-expect (draw-screen 2)
              (overlay (text (number->string (expt BASE 2))
                             FONT-SIZE
                             FONT-COLOR)
                       SCENE))

(check-expect (draw-screen 3)
              (overlay (text (number->string (expt BASE 3))
                             FONT-SIZE
                             FONT-COLOR)
                       SCENE))


(define (draw-screen i)
  (overlay
   (text (number->string (expt BASE i))
         FONT-SIZE
         FONT-COLOR)
   SCENE))


; next-base : i KeyEvent -> i
; returns the next power of the given base upon a keypress

(check-expect (next-base 0 "right") 1)
(check-expect (next-base 2 "right") 3)
(check-expect (next-base 2 "left") 2)

(define (next-base i key)
  (cond [(key=? key "right")
         (add1 i)]
        [else i]))

; stop-screen: i -> Image
; stops when the current number to be displayed exceeds LIMIT

(check-expect (stop-screen 0) #f)
(check-expect (stop-screen 10000000) #t)
               
(define (stop-screen i)
  (< LIMIT (expt BASE i)))

; final-image: i -> Image
; displays the final image

(check-expect (final-image 0)
              (overlay
               (text (string-append "Value exceeds " (number->string LIMIT) " !")
                     FONT-SIZE
                     FONT-COLOR)
               SCENE))

(check-expect (final-image 2)
              (overlay
               (text (string-append "Value exceeds " (number->string LIMIT) " !")
                     FONT-SIZE
                     FONT-COLOR)
               SCENE))
(check-expect (final-image 100)
              (overlay
               (text (string-append "Value exceeds " (number->string LIMIT) " !")
                     FONT-SIZE
                     FONT-COLOR)
               SCENE))

(define (final-image i)
  (overlay
   (text (string-append "Value exceeds " (number->string LIMIT) " !")
         FONT-SIZE
         FONT-COLOR)
   SCENE))




