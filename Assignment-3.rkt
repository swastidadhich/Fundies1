;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)


; Exercise 1

(define BG (square 1000 "solid" "white"))
(define HEIGHT 20)

; monitor-account: PostNum -> Image
; displays account balance

(define (monitor-account n)
  (big-bang n
    [to-draw draw-balance]
    [on-tick balance-change 1]
    [stop-when stop-screen broke-screen]))


; draw-balance: PosNum-> Image
; draws the image of of the current bank balance

(check-expect (draw-balance 49)(overlay (rectangle (* 10 49) HEIGHT "solid" "gold") BG))
(check-expect (draw-balance 20)(overlay (rectangle (* 10 20) HEIGHT "solid" "gold") BG))

(define (draw-balance n)
  (overlay
   (rectangle (* 10 n) HEIGHT "solid" "gold") BG))

; balance-change: PosNum -> PosNum
; returns the balance after each tick (one day)

(check-expect (balance-change 2) 9)
(check-expect (balance-change 6) 21)
(check-expect (balance-change 3) 1)

(define (balance-change n)
  (cond
    [(odd? n) (sub1 (/ (add1 n) 2))]
    [(even? n) (* (add1 n) 3)]))

; stop-screen: PosNum -> Boolean
; stops when the current balance is zero

(check-expect (stop-screen 10) #f)
(check-expect (stop-screen 0) #t)

(define (stop-screen n)
  (<= n 0))

; broke-screen: PosNum -> Image
; displays a messages when there is no balance in the bank and stops

(check-expect (broke-screen 0) (overlay (text "You are broke!" 10 "black") BG))

(define (broke-screen n)
  (overlay
   (text
    "You are broke!" 10 "black") BG))

; Exercise 2

; Traffic Light Simulator

; A TrafficLight is one of:
; - "red"
; - "yellow"
; - "green"
; Represents the colors of a traffic light

(define TL-RED    "red")
(define TL-YELLOW "yellow")
(define TL-GREEN  "green")
#;
(define (trafficlight-templ tl)
  (...
   (cond
     [(string=? tl TL-RED)    ...]
     [(string=? tl TL-YELLOW) ...]
     [(string=? tl TL-GREEN)  ...])))

; ClockedLight is (make-CL TrafficLight PostNum)
; represents the color of a traffic light and its cyclic time [0,6).

(define-struct cl [trafficlight time])

(define CL-RED (make-cl TL-RED 1))
(define CL-GREEN (make-cl TL-GREEN 4))
(define CL-YELLOW (make-cl TL-YELLOW 5))

(define (cl-temp cl)
  (...
   (trafficlight-temp (cl-trafficlight cl))
   (cl-time cl)))
     

; loop-light : ClockedLight -> ClockedLight
; Visualizes a looping traffic light

(define (loop-light cl)
  (big-bang cl
    [to-draw draw-cl]  
    [on-tick next-cl 1]))

; draw-cl : ClockedLight -> Image
; Visualizes a traffic light

(define TL-RADIUS 50)
(check-expect (draw-cl CL-RED)    (circle TL-RADIUS "solid" "red"))
(check-expect (draw-cl CL-YELLOW) (circle TL-RADIUS "solid" "yellow"))
(check-expect (draw-cl CL-GREEN)  (circle TL-RADIUS "solid" "green"))

(define (draw-cl cl)
  (circle TL-RADIUS "solid" (cl-trafficlight cl)))

; next-cl : ClockedLight -> ClockedLight
; Loops the light

(check-expect (next-cl CL-RED)(make-cl TL-RED 2))
(check-expect (next-cl CL-GREEN)(make-cl TL-YELLOW 5))
(check-expect (next-cl CL-YELLOW)(make-cl TL-RED 0))


(define (next-cl cl)
  (make-cl (if 
            (change-color cl)
            (next-tl (cl-trafficlight cl))
            (cl-trafficlight cl)) (mod-6 cl)))
    

; next-tl : TrafficLight -> TrafficLight
; Loops the light

(check-expect (next-tl TL-RED)    TL-GREEN)
(check-expect (next-tl TL-YELLOW) TL-RED)
(check-expect (next-tl TL-GREEN)  TL-YELLOW)

(define (next-tl tl)
  (cond
    [(string=? tl TL-RED)    TL-GREEN]
    [(string=? tl TL-YELLOW) TL-RED]
    [(string=? tl TL-GREEN)  TL-YELLOW]))


; change-color : ClockedLight -> Boolean
; tells when to change the color

(define (change-color cl)
  (or (= (cl-time cl) 2)
      (= (cl-time cl) 4)
      (= (cl-time cl) 5)))
  


; mod-6: ClockedLight -> ClockedLight
; Goes to the next tick

(define (mod-6 cl)
  (modulo (add1 (cl-time cl)) 6))





; Exercise 3

; A Event is a (make-event String NatNum NatNum String Boolean String)
(define-struct event [title daytime-start daytime-end location public? name])
;represents an event's title, start time & end time, location, if the event is
;public& name/postion of event creator

; selectors

; event-title
; returns the 'title' out of the make-title

; event-daytime-start
; returns the 'daytime-start' out of the make-daytime-start

; event-daytime-end
; returns the 'daytime-end' out of the make-daytime-end

; event-location
; returns the 'location' out of the make-location

; event-public?
; returns if event is 'public?' out of the make-public?

; event-name
; returns the 'name' out of the make-name

(define EVENT-1 (make-event "Business Meeting" 8.0 10.0 "office" #f "CEO"))
(define EVENT-1.2 (make-event "Business Meeting" 14.0 16.0 "office" #f "CEO"))
(define EVENT-2 (make-event "Lunch Meeting" 13.0 14.0  "cafe" #t "Anna Flemmings"))
(define EVENT-2.2 (make-event "Lunch Meeting" 10.0 11.0  "cafe" #t "Anna Flemmings"))
(define EVENT-3 (make-event "Conference Call" 15.0 17.0 "office" #t "Product Manager"))
(define EVENT-3.3 (make-event "Conference Call" 13.0 14.0 "office" #t "Product Manager"))
(define EVENT-X (make-event "Video Conference Call" 18.0 17.0 "office" #t "Group Manager"))

(define (event-temp e)
  (...
   (event-title e)
   (event-daytime-end e)
   (event-daytime-end e)
   (event-location e)
   (event-public? e)
   (event-name e)))

; duration: Event -> NatNum
; returns the length of an event in mintues


(check-expect (duration EVENT-1) 120)
(check-expect (duration EVENT-2) 60)
(check-expect (duration EVENT-X)
              "event 'Video Conference Call': incorrect time specification: start>end")

(define (duration e)
  (if
   (< (event-daytime-start e) (event-daytime-end e))
   (* (- (event-daytime-end e) (event-daytime-start e)) 60)
   (string-append "event " "'"(event-title e)"'" ": incorrect time specification: start>end")))

; reschedule: Event NatNum -> Event
; returns an event at a new time but with the same duration

(check-expect (reschedule EVENT-1 14.0) EVENT-1.2)
(check-expect (reschedule EVENT-2 10.0) EVENT-2.2)
(check-error (reschedule EVENT-3 24.0)
             "event ’Conference Call’: rescheduling extends event to midnight")

(define (reschedule e n)
  (if
   (>=  (event-daytime-end e) 24)
   (make-event (event-title e) n (duration (/ (- (event-daytime-end n) (event-daytime-start n)) 60))
               (event-location e) (event-public? e) (event-name e))
   (error (string-append "event " "'"(event-title e)"'" ": rescheduling extends event to midnight"))))

; overlap?: Event Event -> Boolean
; returns true if the two events overlap

(check-expect (overlap? EVENT-1 EVENT-2) #f)
(check-expect (overlap? EVENT-2 EVENT-3.3) #t)

(define (overlap? e1 e2)
  (or
   (> (event-daytime-end e1) (event-daytime-start e2)) #f))
  


   


