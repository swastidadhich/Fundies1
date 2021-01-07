;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 8.0|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; Exercise 1
; expand : NatNum -> [List-of [List-of Nat]]
; returns a list of length n
(check-expect (expand 5) (list
                          (list 0)
                          (list 0 1)
                          (list 0 1 2)
                          (list 0 1 2 3)
                          (list 0 1 2 3 4)))
(check-expect (expand 2) (list
                          (list 0)
                          (list 0 1)
                          (list 0 1 2)))

(define (expand n)
  (local [; create-range : NatNum -> [List-of Nat]
          ; returns a list of numbers from 0 to the given number
          ; 0 -> (list 0)
          ; 3 -> (list 0 1 2)
          (define (create-range n)
            (range 0 (add1 n) 1))]
    (build-list n create-range)))


; Exercise 2
(define (word-counts los)
  (local [; frequency-string : String [List-of String] -> Number
          ; returns how many times a given string occurs in a list
          ; "really" (list "I" "really" "really" "don't" "know") -> 2
          ; really" '() -> 0
          (define (frequency-string s)
            (local [; match? : String -> Boolean
                    ; determines if two strings are the same
                    ; "yes" : "yes" -> true
                    ; "yes" : "no" -> false
                    (define (match? string)
                      (string=? s string))]
              (length (filter match? los))))]
    (map frequency-string los)))

; Exercise 3
(define-struct country [name capital population landlocked neighbors])
 
; A Country is a (make-country String String Nat Boolean [List-of String])
; repr. data on a country, where
; - name is the country's name
; - capital is the name of the country's capital
; - population is the (approx.) population of the country IN MILLIONS
; - landlocked is true iff the country does not have a coastline
; - neighbors is a list of names of all countries with which the country has a common border

(define CO-0 (make-country "United States" "Washington"   330 #f (list "Canada" "Mexico")))
(define CO-1 (make-country "Japan"         "Tokyo"        130 #f '()))
(define CO-2 (make-country "Switzerland"   "Bern"           9 #t
                           (list "Germany" "Austria" "Lichtenstein" "Italy" "France")))
(define CO-3 (make-country "France"        "Paris"         70 #f
                           (list "Belgium" "Germany" "Switzerland" "Italy" "Spain")))
(define CO-4 (make-country "Argentina"     "Buenos Aires"  50 #f
                           (list "Paraguay" "Brazil" "Uruguay" "Chile" "Bolivia")))
 
(define (country-templ c)
  (... (country-name       c) ... (country-capital    c) ... (country-population c)
       ... (country-landlocked c) ... (country-neighbors  c) ...))
 
(define-struct city [name country population megacity languages])
 
; A City is a (make-city String String Nat Boolean [List-of String])
; repr. data on a city, where
; - name is the name of the city
; - country is the name of the country to which the city belongs
; - population is the (approx.) population of the city IN THOUSANDS
; - megacity is true iff the city has a population of 1 million people or more
; - languages is a list of names of /natural/ languages (not ISL) officially spoken in that city.

(define CI-0 (make-city "Washington" "United States" 5000 #t (list "English" "Spanish")))
(define CI-1 (make-city "Bern"       "Switzerland"    500 #f (list "German" "French" "Rhaetian")))
(define CI-2 (make-city "Boston"     "United States"  700 #f (list "English" "Spanish" "Chinese")))
(define CI-3 (make-city "Nairobi"    "Kenya"         5000 #t (list "Swahili" "English")))

(define (city-templ c)
  (... (city-name     c) ... (city-country   c) ... (city-population c)
       ... (city-megacity c) ... (city-languages c) ...))
 
; A Location is one of
; - Country
; - City
; repr. a geographic location.
(define LOC-0 CO-0)
(define LOC-1 CI-0)



;# 
(define (location-templ l)
  (cond [(country? l) (... (country-templ l) ...)]
        [(city?    l) (... (city-templ    l) ...)]))
 
; An Atlas is a [List-of Location]
; A list of Country-ies and City-ies.
; (We are not providing a template, since this data is of the form, [List-of ...].)


(define A-0 (list CO-0 CO-1 CO-2 CO-3 CO-4 CI-0 CI-1 CI-2 CI-3))
(define A-1 (list CO-0 CO-1 CO-2 CO-3 CI-0 CI-1 CI-2))
(define A-2 (list CO-0 CO-1 CO-2 CI-0 CI-1))
(define A-3 '())
(define A-4 (list CO-2 CI-1 CI-2))
(define A-5 (list CO-0 CO-2 CI-1))
(define A-6 (list CO-2 CI-1))

; Exercise 4

; missing-locations: Atlas -> Atlas
; returns countries who's capital arent in the atlas and the cities who's countries arent in the altas

(check-expect (missing-locations A-3) '())
(check-expect (missing-locations A-5) (list CO-0))
(check-expect (missing-locations A-6) '())
(check-expect (missing-locations A-4) (list CI-2))

(define (missing-locations a)
  (local [(define CITIES (filter city? a))
          (define COUNTRIES (filter country? a))
          ; missing-location?: location->boolean
          ; returns #t if the inputed location(city/country) has no
          ; corresponding capital country/city 
          (define (missing-location? l)
            (cond
              [(country? l) (missing-country l)]
              [(city?    l) (missing-city l)]))
            
          ; missing-country: Country-> Boolean
          ; returns #t if the countrys capital is not in the list of cities
          ; using A-0: "United States" -> #f
          ; using A-2: "Japan" -> #t
          (define (missing-country c) 
            (check-cities? (country-capital c))) 

          ; check-cities? : String [List-of City] -> Boolean
          ; returns #t if the country's capital is not in the list of cities
          ; using A-0 "Washington"  -> #f
          ; using A-7 "Nairobi" -> #t
          (define (check-cities? c)
            (local [; is-city?: City -> Boolean
                    ; returns #t if string is same as the citys name
                    (define (is-city? city)
                      (string=? c (city-name city)))]
              (not (ormap is-city? CITIES)))) 

          ; missing-city: City-> Boolean
          ; returns #t if the citys country isnt in the altas
          ; using A-0: "Washington" -> #f
          ; using A-7: "Nairobi" -> #t
          (define (missing-city c)
            (check-countries? (city-country c)))

          ; check-countries?: String [List-of Country] -> Boolean
          ; returns #t if the citys country does not have a country in the list of countries
          ; using A-0: "Washington" -> #f
          ; using A-7: "Nairobi" -> #t
          (define (check-countries? c)
            (local [; is-country?: Country -> Boolean
                    ; returns #f if string is same as the countrys name
                    (define (is-country? country)
                      (string=? c (country-name country)))]
              (not (ormap is-country? COUNTRIES))))]
    (filter missing-location? a)))


  




  
  


            
