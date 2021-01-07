;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)

; Exercise 1

(define-struct moon [x v])
     
; A Moon is a (make-moon Number Number)
; the position and velocity of a moon.
;  - x is the x-position of the moon
;  - v is the x-velocity of the moon

(define MOON-1 (make-moon 10 1))
(define MOON-2 (make-moon 300 -1))

(define (moon-templ m)
  (... (moon-x m) ... (moon-v m) ...))

(define-struct system [moon system])
; A System is one of:
; - 0
; - (make-system Moon System)
; Representing the moons we are simulating:
; - zero moons, OR
; - the first moon and the rest of the moons

(define SYSTEM-0 0)
(define SYSTEM-1 (make-system MOON-1 SYSTEM-0))
(define SYSTEM-2 (make-system MOON-2 SYSTEM-1))

#;
(define (system-templ s)
  ...
  (cond [(number? s) ...]
        [(system? s) ... (moon-templ   (system-moon   s)) ...
                     (system-templ (system-system s)) ...])
  ...)
          
(define SIZE 400)
(define MIDDLE (/ SIZE 2))
     
(define SUN   (circle (/ SIZE 10) "solid" "yellow"))
(define MOON  (circle (/ SIZE 10) "solid" "gray"))
(define SKY   (square SIZE "solid" "pink"))
(define SCENE (place-image SUN MIDDLE MIDDLE SKY))

(define NEW-MOON (make-moon 1 5))
(define NEW-MOON-1 (make-moon 4 1))
(define NEW-MOON-2 (make-moon 10 -1))
(define NEW-MOON-3 (make-moon 15 1))

; eclipse : System -> System
; Runs an eclipse of moons

(define (eclipse initial-s)
  (big-bang initial-s
    [to-draw draw-eclipse]
    [on-tick move-eclipse]
    [on-key change-moon-count]))

; draw-eclipse : System -> Image ; SIGNATURE CHANGE!
; Draw the moons into the SCENE

(check-expect (draw-eclipse SYSTEM-0) SCENE)
(check-expect (draw-eclipse SYSTEM-2)
              (draw-moon MOON-1 (draw-moon MOON-2 SCENE)))

(define (draw-eclipse s)
  (cond [(number? s) SCENE]
        [(system? s) (draw-moon (system-moon s)
                                (draw-eclipse (system-system s)))]))

; draw-moon : Moon Image -> Image
; Draws a moon onto a background

(check-expect (draw-moon MOON-1 SKY)
              (place-image MOON (moon-x MOON-1) MIDDLE SKY))

(define (draw-moon moon background)
  (place-image MOON (moon-x moon) MIDDLE background))

; move-eclipse : System -> System
; Moves ALL moons for one tick

(check-expect (move-eclipse SYSTEM-0) SYSTEM-0)
(check-expect (move-eclipse SYSTEM-2)
              (make-system (move-moon MOON-2) 
                           (make-system (move-moon MOON-1) 0)))

(define (move-eclipse s)
  (cond [(number? s) 0]
        [(system? s) (make-system (move-moon (system-moon s))
                                  (move-eclipse (system-system s)))]))

; move-moon : Moon -> Moon
; Moves a single moon

(check-expect (move-moon MOON-1) (make-moon 11 1))
(check-expect (move-moon MOON-2) (make-moon 299 -1))

(define (move-moon m)
  (make-moon (+ (moon-x m) (moon-v m))
             (moon-v m)))
     


; change-moon-count: System KeyEvent -> System
; Adds a moon to the system

(check-expect (change-moon-count SYSTEM-0 " ") (make-system NEW-MOON-1 SYSTEM-0))
(check-expect (change-moon-count SYSTEM-1 "2") (make-system NEW-MOON-1
                                                            (make-system NEW-MOON-2 SYSTEM-1)))
(check-expect (change-moon-count SYSTEM-2 "3")
              (make-system NEW-MOON-1 (make-system NEW-MOON-2 (make-system NEW-MOON-3 SYSTEM-2))))

(check-expect (change-moon-count SYSTEM-0 "\b") SYSTEM-0)


(define (change-moon-count s ke)
  (cond
    [(key=? ke "2") (make-system NEW-MOON-1 (make-system NEW-MOON-2 s))]
    [(key=? ke "3") (make-system NEW-MOON-1 (make-system NEW-MOON-2 (make-system  NEW-MOON-3 s)))]
    [(or
      (key=? ke "\b") (key=? ke "\u007F"))
     (remove-1-moon s)]
    [else (make-system NEW-MOON-1 s)]))

; remove-1-moon: System -> System
; returns a system with one less moon

(define (remove-1-moon s)
  (cond [(number? s) 0]
        [(system? s) (system-system s)]))


; Exercise 2
  
; A ListOfStrings (LoS) is one of:
; - '()
; - (cons string LoS)
; represents a list of strings

(define LOS-0 '())
(define LOS-1 (cons "apple" LOS-0))
(define LOS-2 (cons "banana" LOS-1))
(define LOS-3 (cons "mango" LOS-2))
(define LOS-4 (cons "pear" LOS-3))
(define LOS-5 (cons "banana" LOS-4))
(define LOS-6 (cons "mango" LOS-5))
(define LOS-7 (cons "mango" LOS-6))

#|
(define (los-templ los)
  ... (cond [(empty? los) ...]
            [(cons?  los) ... (first los) ... (los-templ (rest los)) ...]))
|#

; are-there-two? : LoS String -> Boolean
; returns true whenever the given list contains at least two copies of the given string

(check-expect (are-there-two? LOS-1 "banana") #f)
(check-expect (are-there-two? LOS-6 "mango") #t)

(define (are-there-two? los s)
  (cond
    [(empty? los) #f]
    [(cons? los)
     (if
      (string=? s (first los))
      (copies? (rest los) s)
      (are-there-two? (rest los) s))]))

; copies? : LoS String -> Boolean
; returns true whenever the rest of the given list contains at least two copies of the given string

(check-expect (copies? LOS-1 "banana") #f)
(check-expect (copies? LOS-6 "mango") #t)

(define (copies? los s)
  (cond
    [(empty? los) #f]
    [(cons? los)
     (if
      (string=? s (first los))
      #t
      (copies? (rest los) s))]))

; Exercise 3

(define-struct city [name country pop])
 
; A City is a (make-city String String Nat)
; repr. the name, country, and population of the city

(define CITY-BOS (make-city "Boston" "USA" 700000))
(define CITY-GEN (make-city "Geneva" "Switzerland" 450000))
(define CITY-ZUR (make-city "Zurich" "Switzerland" 450000))
(define CITY-TOK (make-city "Tokyo" "Japan" 10000000))
(define CITY-LUC (make-city "Luckenbach" "USA" 12))
(define CITY-0   (make-city "Zero" "Zero"  0))

#|
(define (city-temp city)
  (...
   (make-name city)
   (make-country city)
   (make-pop city)))
|#

; city-max-2: City City -> City
; returns the city with a bigger popluation

(check-expect (city-max-2 CITY-BOS CITY-GEN) CITY-BOS)
(check-expect (city-max-2  CITY-GEN CITY-ZUR) CITY-ZUR)
(check-expect (city-max-2 CITY-TOK  CITY-GEN) CITY-TOK)
(check-expect (city-max-2 CITY-0 CITY-LUC) CITY-LUC)


(define (city-max-2 c1 c2)
  (if
   (> (city-pop c1) (city-pop c2))
   c1
   c2))

; ListOfCity (LoC) is one of
; - '()
; - (cons City LoC)

(define LOC-0 (cons CITY-0 '()))
(define LOC-1 (cons CITY-BOS LOC-0))
(define LOC-2 (cons CITY-GEN LOC-1))
(define LOC-3 (cons CITY-ZUR LOC-2))
(define LOC-4 (cons CITY-TOK LOC-3))
(define LOC-5 (cons CITY-LUC LOC-4))


#|
(define (loc-temp loc)
  ...(cond [(empty? loc)...]
           [(cons? loc)... (first loc)
                       ...(los-temp (rest loc))...]))
|#

; city-max: LoC -> City
; returns the largest city in terms of population

(check-expect (city-max LOC-0) CITY-0)
(check-expect (city-max LOC-1) CITY-BOS)
(check-expect (city-max LOC-2) CITY-BOS)
(check-expect (city-max LOC-3) CITY-BOS)
(check-expect (city-max LOC-4) CITY-TOK)
(check-expect (city-max LOC-5) CITY-TOK)

(define (city-max loc)
  (cond
    [(empty? loc) CITY-0]
    [(cons? loc) (city-max-2 (first loc) (city-max (rest loc)))]))

;(check-expect (city-max (list CITY-LUC CITY-GEN CITY-ZUR)) CITY-GEN)-

; this means you are only comparing the first and second in the list for the expected output.
; we would have to change our function city-max-2, if I inputed 2 cities with the same value it would
; return the second one. I would change so it if given 2 same values it should output the first value

;(define (city-max-2 c1 c2)
; (if
; (< (city-pop c1) (city-pop c2))
;  c2
;  c1))

; this is how i would change the original function

  
