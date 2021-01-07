;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; HW 7

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

(define CI-0.v2 (make-city "Washington" "United States" 5000 #t '()))
(define CI-1.v2 (make-city "Bern"       "Switzerland"    500 #f '()))
(define CI-2.v2 (make-city "Boston"     "United States"  700 #f '()))
(define CI-3.v2 (make-city "Nairobi"    "Kenya"         5000 #t '()))
(define (city-templ c)
  (... (city-name     c) ... (city-country   c) ... (city-population c)
       ... (city-megacity c) ... (city-languages c) ...))
 
; A Location is one of
; - Country
; - City
; repr. a geographic location.
(define LOC-0 CO-0)
(define LOC-1 CI-0)
(define LOC-2 CI-0.v2)


;# 
(define (location-templ l)
  (cond [(country? l) (... (country-templ l) ...)]
        [(city?    l) (... (city-templ    l) ...)]))
 
; An Atlas is a [List-of Location]
; A list of Country-ies and City-ies.
; (We are not providing a template, since this data is of the form, [List-of ...].)

(define A-0.v2 (list CO-0 CO-1 CO-2 CO-3 CO-4 CI-0.v2 CI-1.v2 CI-2.v2 CI-3.v2))
(define A-1.v2 (list CO-0 CO-1 CO-2 CO-3 CI-0.v2 CI-1.v2 CI-2.v2))
(define A-2.v2 (list CO-0 CO-1 CO-2 CI-0.v2 CI-1.v2))
(define A-3.v2 '())

; EX1
(define A-0 (list CO-0 CO-1 CO-2 CO-3 CO-4 CI-0 CI-1 CI-2 CI-3))
(define A-1 (list CO-0 CO-1 CO-2 CO-3 CI-0 CI-1 CI-2))
(define A-2 (list CO-0 CO-1 CO-2 CI-0 CI-1))
(define A-3 '())
(define A-4 (list CO-2 CI-0 CI-2 CI-3))
(define A-5 (list CO-0 CO-2 CI-1))


; EX2
; countries: Atlas -> [List-of Countries]
; returns only a list of countries when given an Atlas which is list of location
(check-expect (countries A-0) (list CO-0 CO-1 CO-2 CO-3 CO-4))
(check-expect (countries A-1) (list CO-0 CO-1 CO-2 CO-3))
(check-expect (countries A-2) (list CO-0 CO-1 CO-2))
(check-expect (countries A-3) '())

(define (countries a)
  (filter country? a))

; cities: Atlas -> [List-of Cities]
; returns only a list of cities when given an Atlas which is list of location
(check-expect (cities A-0) (list CI-0 CI-1 CI-2 CI-3))
(check-expect (cities A-1) (list CI-0 CI-1 CI-2))
(check-expect (cities A-2) (list CI-0 CI-1))
(check-expect (cities A-3) '())

(define (cities a)
  (filter city? a))

; EX3
; cities-us: Atlas -> [List-of City]
; returns a list of cities in the US
(check-expect (cities-us A-0) (list CI-0 CI-2))
(check-expect (cities-us A-1) (list CI-0 CI-2))
(check-expect (cities-us A-2) (list CI-0))
(check-expect (cities-us A-3) '())

(define (cities-us a)
  (filter us-city? (cities a)))

; us-city?: City -> Boolean
; returns a #t if the city is in the united states
(check-expect (us-city? CI-0) #t)
(check-expect (us-city? CI-1) #f)
(check-expect (us-city? CI-2) #t)
(check-expect (us-city? CI-3) #f)

(define (us-city? c)
  (if
   (string=? (city-country c) "United States")
   #t
   #f))

; EX4
; neighbors-germany: Atlas -> [List-of Country]
; returns a list of countries that are neighbours with germany
(check-expect (neighbors-germany A-0) (list CO-2 CO-3))
(check-expect (neighbors-germany A-1) (list CO-2 CO-3))
(check-expect (neighbors-germany A-2) (list CO-2))
(check-expect (neighbors-germany A-3) '())

(define (neighbors-germany a)
  (filter neighbor? (countries a)))

; neighbor?: [list-of Country] -> Boolean
; returns true if the country is neighboring germany
(check-expect (neighbor? CO-0) #f)
(check-expect (neighbor? CO-1) #f)
(check-expect (neighbor? CO-2) #t)
(check-expect (neighbor? CO-3) #t)

(define (neighbor? loc)
  (ormap check-elements? (country-neighbors loc)))

; check-elements? : String-> Boolean
; returns #t if the country is Germany
(check-expect (check-elements? "United States") #f)
(check-expect (check-elements? "Japan") #f)
(check-expect (check-elements? "Switzerland") #f)
(check-expect (check-elements? "France") #f)
(check-expect (check-elements? "Germany") #t)

(define (check-elements? c)
  (if
   (string=? "Germany" c)
   #t
   #f))

; EX5
; atlas-w/o-lang-data: Atlas -> Atlas
; returns Atlas with the updated version in which langauge data of a city is returned by an emptylist
(check-expect (atlas-w/o-lang-data A-0) A-0.v2)
(check-expect (atlas-w/o-lang-data A-1) A-1.v2)
(check-expect (atlas-w/o-lang-data A-2) A-2.v2)
(check-expect (atlas-w/o-lang-data A-3) A-3.v2)

(define (atlas-w/o-lang-data a)
  (map updated-city a))

; updated-city: Location -> Location
; returns the updated location

(check-expect (updated-city LOC-0) CO-0)
(check-expect (updated-city LOC-2) CI-0.v2)


(define (updated-city l)
  (cond [(country? l) l]
        [(city? l) (make-city
                    (city-name l) (city-country l) (city-population l) (city-megacity l) '())]))


; EX6
; countries-population: Atlas -> Number
; returns the sum of the the population (in millions) from the list of countries

(check-expect (countries-population A-0) 589)
(check-expect (countries-population A-1) 539)
(check-expect (countries-population A-2) 469)
(check-expect (countries-population A-3) 0)

(define (countries-population a)
  (foldr + 0 (map country-population (countries a))))



; EX7
; check-population: Atlas -> Boolean
; returns #t if the total number of City people is
; at most the total number of Country people in the Atlas

(define ERROR-A4 (string-append "check-population: error: atlas has 10700 thousand people "
                                "in all cities but only 9 million people in all countries"))

(check-expect (check-population A-5) #t)
(check-expect(check-population A-4) ERROR-A4)
(check-expect (check-population A-3) #t)

(define (check-population a)
  (if
   (>= (* (countries-population a) 1000000) (* (cities-population a) 1000))
   #t
   (string-append "check-population: error: atlas has "
                  (number->string (cities-population a)) " thousand people in all cities but only "
                  (number->string (countries-population a)) " million people in all countries")))

; cities-population: Atlas -> Number
; returns the sum of the the population (in thousands) from the list of cities

(check-expect (cities-population A-0) 11200)
(check-expect (cities-population A-1) 6200)
(check-expect (cities-population A-2) 5500)
(check-expect (cities-population A-3) 0)

(define (cities-population a)
  (foldr + 0 (map city-population (cities a))))

; EX8
; I filled out the halftime class survey.
