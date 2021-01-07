;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/universe)
(require 2htdp/image)


; Exercise 1

; stirling : NatNum -> NatNum
; approximates the value of n factorial

;(check-expect (stirling 0) 0)
;(check-expect (stirling 3) 6)
;(check-expect (stirling 5) 120)
;(check-expect (stirling 10) 3628800)

(define (stirling n)
  (* (sqrt (* 2 pi n)) (expt (/ n e) n)))

;Exercise 2

; area-from-sides : NatNum NatNum NatNum -> NatNum
; computes the area of a triangle given 3 sides

(check-expect (area-from-sides 3 4 5) 6)
(check-expect (area-from-sides 20 21 29) 210)
(check-expect (area-from-sides 8 15 17) 60)
             

(define (area-from-sides n1 n2 n3)
  (sqrt (* (- (s-value n1 n2 n3) n1) (- (s-value n1 n2 n3) n2) (- (s-value n1 n2 n3) n3)
           (s-value n1 n2 n3))))


; s-value: NatNum NatNum NatNum -> NatNum
; computes the value of s which is needed to compute area

(check-expect (s-value 2 3 4) 4.5)
(check-expect (s-value 3 3 4) 5)
(check-expect (s-value 0 4 4) 4)

(define (s-value n1 n2 n3)
  (/ (+ n1 n2 n3) 2))


;Exercise 3

; duplicated?: String -> Boolean
; returns true if the string is duplicated

(check-expect (duplicated? "ss") true)
(check-expect (duplicated? "abc") false)
(check-expect (duplicated? "abcabc") true)
(check-expect (duplicated? "12345") false)

(define (duplicated? s)
  (cond
    [(odd? (string-length s)) false]
    [(equal?
      (substring s 0 (/ (string-length s) 2))
      (substring s (/ (string-length s) 2) (string-length s))) true]))
  
;Exercise 4

(define ANGLE 120)

; pine-tree: Number Number -> Image
; draws an image given 2 numbers

(define (pine-tree a d)
  (above
   (isosceles-triangle a ANGLE  "solid" "forestgreen")
   (isosceles-triangle (+ a d) ANGLE  "solid" "forestgreen")
   (isosceles-triangle (+ a (* 2 d)) ANGLE  "solid" "forestgreen")
   (isosceles-triangle (+ a (* 3 d)) ANGLE  "solid" "forestgreen")
   (isosceles-triangle (+ a (* 4 d)) ANGLE  "solid" "forestgreen")
   (rectangle a (* 1.5 a) "solid" "brown")))


(pine-tree 40 10)

  
;Exercise 5

; Bouncy Ball animation 

(define BG (square 300 "solid" "white"))
(define BALL-BLACK (circle 5 "solid" "black"))
(define BALL-RED (circle 5 "solid" "red"))
(define BG-HEIGHT 300)
(define BG-WIDTH 300)

; draw-ball: Number -> Image
; draws the ball at a given (x,y), given an x - coordinate 

(define (draw-ball x)
  (cond
    [(<= x 150)(place-image BALL-BLACK
                            x (+ x 150) BG)]
    [(and  (> x 150) (< x 300))  (place-image BALL-BLACK
                                              x (- 300 (- x 150)) BG)]
    [(>= x 300) (place-image BALL-RED
                             300 150
                             BG)]))                    
  
(animate draw-ball)

 

 






