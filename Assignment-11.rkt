;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; HW 11

; Exercise 1
; (a)
; set=?: [List-of X] [List-of X][X X-> Boolean] -> Boolean
; returns iff the two inputed lists are equal up to duplicity and order
(check-expect (set=? (list 1 2 3) (list 1 2 3) =) #t)
(check-expect (set=? (list 1 2 3 3) (list 3 2 1) =) #t)
(check-expect (set=? (list 1 2 3 3 4) (list 3 2 1) =) #f)
(check-expect (set=? (list "one" "two" "three" "two") (list "one" "two" "three") string=?) #t)
(check-expect (set=? (list #f #t #f) (list #f #t #f #f #f) boolean=?) #t)
(check-expect (set=? '() '() =) #t)


(define (set=? lox1 lox2 compar?)
  (if (and (present-in? lox1 lox2 compar?) (present-in? lox2 lox1 compar?)) #t #f))

; present-in? : [List-of X] [List-of X][X -> Boolean] -> Boolean
; checks if all the items in the first list are present at least once in the second
(check-expect (present-in?  (list 1 2 3) (list 1 2 3) =) #t)
(check-expect (present-in?  (list 1 2 3 3 4) (list 3 2 1) =) #f)
(check-expect (present-in? (list "one" "two" "three" "two") (list "one" "two" "three") string=?) #t)
(check-expect (present-in? '() '() =) #t)

(define (present-in? lox1 lox2 compar?)
  (cond
    [(empty? lox1) #t]
    [(cons? lox1) (if (present-in-item? (first lox1) lox2 compar?)
                      (present-in? (rest lox1) lox2 compar?)
                      #f)]))


; present-in-item? : X [List-of X] -> Booelan
; checks if an item is present at least once in a list

(check-expect (present-in-item?  1 (list 1 2 3) =) #t)
(check-expect (present-in-item?  2 (list 2 3 2) =) #t)
(check-expect (present-in-item?  4 (list 1 2 3) =) #f)

(define (present-in-item? x lox compar?)
  (cond
    [(empty? lox) #f]
    [(cons? lox) (if (compar? x (first lox))
                     #t
                     (present-in-item? x (rest lox) compar?))]))

;(b)

;(define TEST-WIKI   (list PAGE-0 PAGE-1 PAGE-2 PAGE-4)) ; Wiki is a [List-of Page]
;(define TEST-RESULT (list "Shillman Hall" "Mardi Gras")) ; Page is a [List-of String]
;(check-expect (broken-links TEST-WIKI) TEST-RESULT)
;(check-expect (set=? (broken-links TEST-WIKI) TEST-RESULT string=?) #t)
           

; Exercise 2

(define-struct page [title links])
     
; A Page is a (make-page String [List-of String])
; repr. a web page's title and links to other pages.
(define PAGE-0 (make-page "Khoury"        (list "NEU")))
(define PAGE-1 (make-page "NEU"           (list "Khoury" "Boston")))
(define PAGE-2 (make-page "Boston"        (list "NEU" "Shillman Hall")))
(define PAGE-3 (make-page "Shillman Hall" (list "NEU")))
(define PAGE-4 (make-page "New Orleans"   (list "Mardi Gras")))
(define (page-templ p)
  (... (page-title p) ...
       (los-templ (page-links p)) ...))
(define (los-templ los)
  (... (cond [(empty? los) ...]
             [(cons?  los) ... (first los)
                           ... (los-templ (rest los))])))
     
; A Wiki is a [List-of WebPage]
; Interpretation: A list of webpages (like the Internet)
(define WIKI-0 (list PAGE-0 PAGE-1 PAGE-2 PAGE-3 PAGE-4))

; find-path: String String Wiki-> [List-of String]
; returns a file path, representing the pages visited along the path or empty if it is not found

(check-expect (find-path "Boston" "NEU" WIKI-0) (list "Boston" "NEU"))
(check-expect (find-path "NEU" "Shillman Hall" WIKI-0) (list "NEU" "Boston" "Shillman Hall"))
(check-expect (find-path "Boston" "Mardi Gras" WIKI-0) '())
(check-expect (find-path "Boston" "Boston" WIKI-0) (list "Boston"))


(define (find-path t1 t2 w)
  (local [(define w-wo-t1 (eliminate t1 w))]
    (if (string=? t1 t2)
        (list t1)
        (foldr (λ (los1 los2) (if (empty? los1)
                                  los2
                                  (cons t1 los1))) '()
                                                   (map (λ (l) (find-path l t2 w-wo-t1))
                                                        (page-links-by-title t1 w))))))

; eliminate: String Wiki -> Wiki
; the wiki without page with the given name
(check-expect (eliminate "NEU" (list PAGE-0 PAGE-1))
              (list (make-page "Khoury" '())))

(define (eliminate t w)
  (cond
    [(empty? w) '()]
    [(string=? (page-title (first w)) t) (eliminate t (rest w))]
    [else (cons (eliminate-links-to t (first w))
                (eliminate t (rest w)))]))

; eliminate-links-to: String Page -> Page
; the page without any links to the page with the given name
(check-expect (eliminate-links-to "NEU" PAGE-3) (make-page "Shillman Hall" '()))

(define (eliminate-links-to t p)
  (make-page (page-title p)
             (filter (λ (l) (not (string=? l t))) (page-links p))))

; page-links-by-title: String Wiki -> [List-of String]
; the list of page names linked to from the given page, or '() if page not found

(check-expect (page-links-by-title "Boston" WIKI-0) (list "NEU" "Shillman Hall"))
(check-expect (page-links-by-title "NOPAGE" WIKI-0) '())

(define (page-links-by-title t w)
  (cond
    [(empty? w) '()]
    [(string=? (page-title (first w)) t) (page-links (first w))]
    [else (page-links-by-title t (rest w))]))




;-----------------------------------------------------------
; BACK to Mine Sweeper Project
; Exercise 3

(require 2htdp/universe)

; A Content is one of:
; - Nat
; - #t
; and represents either a count of the number of mines surrounding
; a cell or a mine itself

(define CONTENT-BLANK 0)
(define CONTENT-MINE #t)

; content-template: Content -> ???
(define (content-template c)
  (cond [(number? c) (... c ...)]
        [(boolean? c) ...]))

(define-struct hidden [con])
(define-struct visible [con])
(define-struct flagged [con])

; A Cell is one of:
; - (make-hidden Content)
; - (make-visible Content)
; - (make-flagged Content)
; and represents either a hidden cell, a visible cell, or a flagged cell

(define CELL-H0 (make-hidden 0))
(define CELL-V0 (make-visible 0))
(define CELL-F0 (make-flagged 0))

(define CELL-H1 (make-hidden 1))
(define CELL-V1 (make-visible 1))
(define CELL-F1 (make-flagged 1))

(define CELL-HM (make-hidden #t))
(define CELL-VM (make-visible #t))
(define CELL-FM (make-flagged #t))

; cell-template : Cell -> ???
(define (cell-template cell)
  (cond [(hidden? cell) (... (hidden-con cell) ...)]
        [(visible? cell) (... (visible-con cell) ...)]
        [(flagged? cell) (... (flagged-con cell) ...)]))

; A Board is a [List-of [List-of Cell]
; and represents a grid of cells that make up a game board

(define BOARD-EMPTY '())
(define BOARD-SEMI-EMPTY '(() () () ()))
(define BOARD-2X2-BLANK (make-list 2 (make-list 2 CELL-H0)))
(define BOARD-3X3-MID (list (make-list 3 CELL-H1)
                            (list CELL-H1 CELL-HM CELL-H1)
                            (make-list 3 CELL-H1)))
(define BOARD-LOSE (list (list CELL-VM)))
(define BOARD-3X3-EMPTY (make-list 3 (make-list 3 CELL-H0)))

; board-template : Board -> ???
(define (board-template b)
  (cond [(empty? b) ...]
        [(cons? b) (... (row-template (first b))
                        (board-template (rest b)) ...)]))

; row-template : [List-of Cell] -> ???
(define (row-template loc)
  (cond [(empty? loc) ...]
        [(cons? loc) (... (cell-template (first loc))
                          (row-template (rest loc)) ...)]))

(define-struct game [board rev?])
; A Game is a (make-game Board Boolean)
; and represents a game of Minesweeper with a board of cells and a flag that is
; #t if the mouse is revealing cells and #f if it is flagging them

(define GAME-EMPTY (make-game BOARD-EMPTY #t))
(define GAME-2X2-T (make-game BOARD-2X2-BLANK #t))
(define GAME-2X2-F (make-game BOARD-2X2-BLANK #f))
(define GAME-3X3-T (make-game BOARD-3X3-MID #t))
(define GAME-3X3-F (make-game BOARD-3X3-MID #f))
(define GAME-LOSE (make-game BOARD-LOSE #t))

; game-template : Game -> ???
(define (game-template g)
  (... (board-template (game-board g))
       (game-rev? g) ...))

; mine-sweeper : Nat Nat -> Game
; Play the minesweeper game with a square board of the given size and the
; given number of mines
(define (mine-sweeper size mines)
  (mine-sweeper-from (make-game (generate-mine-board size mines) #t)))

; mine-sweeper-from : Game -> Game
; Play the minesweeper game with the given initial game state
(define (mine-sweeper-from g)
  (big-bang g
    [to-draw draw-game]
    [on-mouse change-if-click]
    [on-key change-mouse-state]
    [stop-when game-over? draw-game]))

#|--- STUBS AS REQUESTED BY THE ASSIGNMENT ---|#
; draw-game : Game -> Image
; Draw the current state of the game (WARNING: THIS IS CURRENTLY A STUB)
(define (draw-game g)
  ...)

#|--- END STUBS ---|#
#|
; generate-mine-board : Nat Nat -> Board
; Generates a fixed board of the given size with the given number of mines
; (all the mines are at the beginning)
(check-expect (generate-mine-board 0 0) '())
(check-error (generate-mine-board 2 7) "Cannot place 7 mines on a 2x2 game board.")
(check-expect
 (generate-mine-board 2 3)
 (list (make-list 2 CELL-HM) (list CELL-HM (make-hidden 3))))
(check-expect (generate-mine-board 2 4) (make-list 2 (make-list 2 CELL-HM)))
(check-expect
 (generate-mine-board 3 4)
 (list (make-list 3 CELL-HM)
       (list CELL-HM (make-hidden 4) (make-hidden 2))
       (list CELL-H1 CELL-H1 CELL-H0)))

(define (generate-mine-board size mines)
  (if (> mines (* size size))
      (error (string-append "Cannot place " (number->string mines)
                            " mines on a " (number->string size) "x" (number->string size)
                            " game board."))
      (local [(define full-mine-rows (if (zero? size) 0 (floor (/ mines size))))
              (define partial-row?
                (and (not (zero? size)) (not (zero? (modulo mines size)))))
              (define mines-in-partial (- mines (* full-mine-rows size))) 
              (define full-zero-rows
                (max 0 (- size (+ full-mine-rows (if partial-row? 1 0)))))]
        (add-counts (append (make-list full-mine-rows (make-list size CELL-HM))
                            (if partial-row?
                                (list (append (make-list mines-in-partial CELL-HM)
                                              (make-list (- size mines-in-partial) CELL-H0)))
                                '())
                           (make-list full-zero-rows (make-list size CELL-H0)))))))

|#

; add-counts : Board -> Board
; Add the correct count for each item on the given board
(check-expect (add-counts '()) '())
(check-expect
 (add-counts (list (make-list 3 CELL-H0)
                   (list CELL-H0 CELL-HM CELL-H0)
                   (make-list 3 CELL-H0)))
 BOARD-3X3-MID)

(define (add-counts b)
  (build-list (length b)
              (λ (row) (build-list (length b)
                                   (λ (col) (add-count-to-cell b row col))))))

; add-count-to-cell : Board Nat Nat -> Cell
; If the cell at the given location is a mine, leave it alone
; If it is not a mine, count the mines around it and make that the count for the cell
(check-expect (add-count-to-cell BOARD-3X3-MID 0 0) CELL-H1)
(check-expect
 (add-count-to-cell
  (list (list CELL-HM CELL-HM) (list CELL-HM CELL-H0)) 1 1)
 (make-hidden 3))

(define (add-count-to-cell board row col)
  (local [(define cell (list-ref (list-ref board row) col))
          (define neighbor-posns (get-neighbor-indices row col (length board)))
          (define neighbor-cells
            (map (λ (p) (list-ref (list-ref board (posn-x p)) (posn-y p))) neighbor-posns))]
    (update-cell-count cell (length (filter mine-cell? neighbor-cells)))))

; get-neighbor-indices : Nat Nat Nat -> [List-of Posn]
; Get a list of the row/column indices of the neighbors of the cell with the given indices
(check-expect (get-neighbor-indices 0 0 0) '())
(check-expect
 (get-neighbor-indices 0 0 2)
 (list (make-posn 0 1) (make-posn 1 0) (make-posn 1 1)))
(check-expect
 (get-neighbor-indices 2 2 3)
 (list (make-posn 1 1) (make-posn 1 2) (make-posn 2 1)))
(check-expect
 (get-neighbor-indices 1 2 4)
 (list (make-posn 0 1) (make-posn 0 2) (make-posn 0 3)
       (make-posn 1 1) (make-posn 1 3)
       (make-posn 2 1) (make-posn 2 2) (make-posn 2 3)))

(define (get-neighbor-indices row col size)
  (local [(define can-add-left? (> size col 0))
          (define can-add-right? (< col (sub1 size)))
          (define can-add-up? (> size row 0))
          (define can-add-down? (< row (sub1 size)))]
    (append (if (and can-add-left? can-add-up?) (list (make-posn (sub1 row) (sub1 col))) '())
            (if can-add-up? (list (make-posn (sub1 row) col)) '())
            (if (and can-add-right? can-add-up?) (list (make-posn (sub1 row) (add1 col))) '())
            (if can-add-left? (list (make-posn row (sub1 col))) '())
            (if can-add-right? (list (make-posn row (add1 col))) '())
            (if (and can-add-left? can-add-down?) (list (make-posn (add1 row) (sub1 col))) '())
            (if can-add-down? (list (make-posn (add1 row) col)) '())
            (if (and can-add-right? can-add-down?) (list (make-posn (add1 row) (add1 col))) '()))))

; update-cell-count : Cell Nat -> Cell
; If this cell is a mine, leave it, otherwise update to the given count
(check-expect (update-cell-count CELL-HM 5) CELL-HM)
(check-expect (update-cell-count (make-visible 7) 2) (make-visible 2))
(check-expect (update-cell-count (make-flagged 0) 3) (make-flagged 3))

(define (update-cell-count cell new-count)
  (local [; update-contents : Content -> Content
          ; Update the contents if it is not a mine to be new-count
          (define (update-contents c)
            (if (boolean? c) c new-count))]
    (cond [(hidden? cell)
           (make-hidden (update-contents (hidden-con cell)))]
          [(visible? cell)
           (make-visible (update-contents (visible-con cell)))]
          [(flagged? cell)
           (make-flagged (update-contents (flagged-con cell)))])))

; mine-cell? : Cell -> Boolean
; Is this a mine cell?
(check-expect (mine-cell? CELL-HM) #t)
(check-expect (mine-cell? (make-visible 3)) #f)
(check-expect (mine-cell? (make-flagged 0)) #f)

(define (mine-cell? cell)
  (cond [(hidden? cell) (boolean? (hidden-con cell))]
        [(visible? cell) (boolean? (visible-con cell))]
        [(flagged? cell) (boolean? (flagged-con cell))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 4

; change-if-click : Game Number Number MouseEvent -> Game
; Change the game if the user clicked on a cell

(define (change-if-click g x y me)
  (cond
    [(mouse=? me "button-down") (changer g x y)]
    [else g]))

; changer : Game Number Number -> Game
; changes the cell status based on the current status and reveal status of the game

(define (changer g x y)
  (cond
    [(game-rev? g) (make-game (new-visible (game-board g) (divider x) (divider y)) #t)]
    [else (make-game (new-flagged (game-board g) (divider x) (divider y)) #f)]))

; nth : [List-of X] Nat -> X
; returns the nth item in a list
(check-expect (nth (list 1 2 3 4 5) 4) 4)
(check-expect (nth (list "one" "two" "three" "four") 4) "four")

(define (nth lox n)
  (cond ((empty? lox) '())
        ((= n 1) (first lox))
        (else (nth (rest lox) (- n 1)))))

; divider: Number -> Number
; divides inputed number by 10
(check-expect (divider 200) 20)
(check-expect (divider 30) 3)

(define (divider n)
  (/ n 10))


; new-visible: Board Number Number -> Board
; finds the cell and determines if it needs to reveal the cell or the surrounding

(check-expect (new-visible BOARD-3X3-MID 1 1) (list (make-list 3 CELL-H1)
                                                    (list CELL-H1 CELL-VM CELL-H1)
                                                    (make-list 3 CELL-H1)))
(check-expect (new-visible BOARD-3X3-EMPTY 1 1) (make-list 3 (make-list 3 CELL-V0)))
              
(define (new-visible b x y)
  (if (= 0 (hidden-con (nth (nth b y) x)))
      (reveal-surrounding (reveal-single b x y) x y)
      (reveal-single b x y)))

; reveal-surrounding: Board Number Number -> Board
; reveals all the cells around it
;(define BOARD-2X2-BLANK (make-list 2 (make-list 2 CELL-H0)))

(check-expect (reveal-surrounding BOARD-3X3-EMPTY 1 1) (list (make-list 3 CELL-V0)
                                                             (list CELL-V0 CELL-H0 CELL-V0)
                                                             (make-list 3 CELL-V0)))
                                  
(define (reveal-surrounding b x y)
  (foldr update-board b (get-neighbor-indices x y (length b))))

; update-board : Posn Board -> Board
; returns updated board based on the inputed  coordinates
(check-expect (update-board (make-posn 1 1) BOARD-3X3-MID) (list (make-list 3 CELL-H1)
                                                                 (list CELL-H1 CELL-VM CELL-H1)
                                                                 (make-list 3 CELL-H1)))
(define (update-board p b)
  (reveal-single b (posn-x p) (posn-y p)))


; reveal-single : Board Number Number -> Board
; reveals one cell
(check-expect (reveal-single BOARD-3X3-MID 1 1) (list (make-list 3 CELL-H1)
                                                      (list CELL-H1 CELL-VM CELL-H1)
                                                      (make-list 3 CELL-H1)))
(define (reveal-single b x y)
  (cond
    [(> y 0) (cons (first b) (reveal-single (rest b) x (- y 1)))]
    [(= y 0) (cons (reveal-cell (first b) x) (rest b))]))


; reveal-cell : [List-of Cells] Number -> [List-of Cells]
; outputs an updated row with the desired cell changed
(check-expect (reveal-cell (list CELL-H1 CELL-VM CELL-H1) 1) (list CELL-H1 CELL-VM CELL-H1))
(check-expect (reveal-cell (list CELL-H1 CELL-H1 CELL-H1) 1) (list CELL-H1 CELL-V1 CELL-H1))

(define (reveal-cell r x)
  (cond
    [(> x 0) (cons (first r) (reveal-cell (rest r) (- x 1)))]
    [(= x 0) (cond
               [(hidden? (first r)) (cons (make-visible (hidden-con (first r))) (rest r))]
               [else (cons (first r) (rest r))])]))


; new-flagged: Board Number Number -> Board
; flags or unflags a cell
(check-expect (new-flagged BOARD-3X3-MID 1 1) (list (make-list 3 CELL-H1)
                                                    (list CELL-H1 CELL-FM CELL-H1)
                                                    (make-list 3 CELL-H1)))
(define (new-flagged b x y)
  (cond
    [(> y 0) (cons (first b) (new-flagged (rest b) x (- y 1)))]
    [(= y 0) (cons (flag-cell (first b) x) (rest b))]))


; flag-cell : [List-of Cells] Number -> [List-of Cells]
; flags a hidden cell on the board or unflags a flagged cell
(check-expect (flag-cell (list CELL-H0 CELL-HM CELL-H0) 1) (list CELL-H0 CELL-FM CELL-H0))
                          
(define (flag-cell r x)
  (cond
    [(> x 0) (cons (first r) (flag-cell (rest r) (- x 1)))]
    [(= x 0) (cons (modify-cell (first r)) (rest r))]))

; modify-cell: Cell -> Cell
; outputs a the updated cell
(check-expect (modify-cell CELL-HM) (make-flagged #t))

(define (modify-cell cell)
  (cond
    [(hidden? cell) (make-flagged (hidden-con cell))]
    [(flagged? cell) (make-hidden (flagged-con cell))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change-mouse-state : Game KeyEvent -> Game
; Change the state of the mouse if the user pressed the space bar
(check-expect (change-mouse-state GAME-EMPTY "x") GAME-EMPTY)
(check-expect (change-mouse-state GAME-2X2-F " ") GAME-2X2-T)

(define (change-mouse-state g key)
  (if (key=? key " ") (make-game (game-board g) (not (game-rev? g))) g))

; game-over? : Game -> Boolean
; Did the user either win or lose?
(check-expect (game-over? GAME-LOSE) #t)
(check-expect (game-over? GAME-EMPTY) #t)
(check-expect (game-over? GAME-3X3-T) #f)

(define (game-over? g)
  (or (board-win? (game-board g))
      (board-lose? (game-board g))))

; board-win? : Board -> Boolean
; Did the user reveal all the non-mine squares?
(check-expect (board-win? BOARD-EMPTY) #t)
(check-expect (board-win? BOARD-3X3-MID) #f)
(check-expect (board-win? (make-list 2 (make-list 2 CELL-FM))) #t)

(define (board-win? board)
  (local [; mine-or-visible? : Cell -> Boolean
          ; Is the given cell either a mine or visible?
          (define (mine-or-visible? cell)
            (cond [(visible? cell) #t]
                  [(hidden?  cell) (boolean? (hidden-con cell))]
                  [(flagged? cell) (boolean? (flagged-con cell))]))]
    (andmap (λ (row) (andmap mine-or-visible? row)) board)))

; board-lose? : Board -> Boolean
; Is there any visible mine square on the board?
(check-expect (board-lose? BOARD-3X3-MID) #f)
(check-expect
 (board-lose?
  (list (list (make-hidden #t) (make-visible 0))
        (list (make-flagged #t) (make-visible #t))))
 #t)

(define (board-lose? board)
  (local [; visible-mine? : Cell -> Boolean
          ; Is the given cell a mine that is visible?
          (define (visible-mine? cell)
            (and (visible? cell) (boolean? (visible-con cell))))]
    (ormap (λ (row) (ormap visible-mine? row)) board)))


; Exercise 5

; generate-mine-board : Nat Nat -> Board
; Generates a random board of the given size with the given number of mines

(check-expect (= (length (generate-mine-board 3 3)) 3) #t)
(check-error (generate-mine-board 2 7) "Cannot place 7 mines on a 2x2 game board.")

(define (generate-mine-board size mines)
  (if (> mines (* size size))
      (error (string-append "Cannot place " (number->string mines)
                            " mines on a " (number->string size) "x" (number->string size)
                            " game board."))
      (add-counts (change-all-mines BOARD-3X3-EMPTY '()))))

; change-all-mines : Board [List-of Posns] -> Board
; changes all cells indicated by the posns into mines
(check-expect (change-all-mines BOARD-3X3-EMPTY (list (make-posn 1 1) (make-posn 0 1)))
              (list (make-list 3 CELL-H0)
                    (list CELL-HM CELL-HM CELL-H0)
                    (make-list 3 CELL-H0)))
(define (change-all-mines b lop)
  (cond
    [(empty? lop) b]
    [(cons? lop) (change-all-mines (change-mine b (first lop)) (rest lop))]))

; change-mine : Board Posn -> Board
; changes the cell at the posn into a mine
(check-expect (change-mine BOARD-3X3-EMPTY (make-posn 1 1)) (list (make-list 3 CELL-H0)
                                                                  (list CELL-H0 CELL-HM CELL-H0)
                                                                  (make-list 3 CELL-H0)))
(define (change-mine b p)
  (cond
    [(> (posn-y p) 0) (cons (first b) (change-mine (rest b) (make-posn (posn-x p) (- 1 (posn-y p)))))]
    [(= (posn-y p) 0) (cons (mine-cell (first b) (posn-x p)) (rest b))]))

; mine-cell : [List-of Cells] Number -> [List-of Cells]
; changes the xth cell in the row into a mine
(check-expect (mine-cell (list CELL-H0 CELL-H0 CELL-H0) 1) (list CELL-H0 CELL-HM CELL-H0))
(define (mine-cell r x)
  (cond
    [(> x 0) (cons (first r) (mine-cell (rest r) (- x 1)))]
    [(= x 0) (cons (make-hidden #t) (rest r))]))

; posn-gen-nodups : [List-of Posns] Number Nat -> [List-of Posns]
; generates a list of length (mines) of random posns without duplicates
(define (posn-gen-nodups lop size mines)
  (if
   (or (has-duplicates? lop) (empty? lop))
   (posn-gen-nodups (posn-gen size mines) size mines)
   lop))

; posn-gen : Nat Nat -> [List-of Posns]
; generates a list of length (mines) of random posns
(define (posn-gen size mines)
  (if
   (= mines 0)
   '()
   (cons (make-posn (random size) (random size)) (posn-gen size (sub1 mines)))))

; has-duplicates? : [List-of Posns] -> Boolean
; checks if a list of posns has a duplicate set of posns
(check-expect (has-duplicates? (list (make-posn 1 1) (make-posn 1 1))) #t)
(check-expect (has-duplicates? (list (make-posn 1 1) (make-posn 1 0))) #f)
(define (has-duplicates? lop) 
  (cond
    [(empty? lop) #f] 
    [(member (first lop) (rest lop)) #t]
    [else (has-duplicates? (rest lop))]))