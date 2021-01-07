;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Nop and Swasti HW|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Homework 10 


; Exercise 1
(define-struct file [name size write? exec?])

; A File is a (make-file String Nat Boolean Boolean)
; represents a file in a directory structure, with:
; - a name
; - a size in Bytes
; - 2 permission bits: the file is writeable/executable

(define F-T (make-file "thomas.jpg"  1200 #f #f))
(define F-J (make-file "john.jpg"    1400 #f #f))
(define F-L (make-file "leena.jpg"   1300 #f #f))
(define F-R (make-file "drracket"    1000000 #f #t))
(define F-C (make-file "2500-grades" 10000 #t #f))


(define (file-temp file)
  (...
   (file-name file)...
   (file-size file)...
   (file-write? file)...
   (file-exec? file)...))


(define-struct dir [name entries])

; A Dir is a (make-dir String [List-of Entry])
; represents a directory

(define (dir-temp dir)
  (...
   (dir-name dir)...
   (loe-temp (dir-entries dir))...))


; An Entry is one of
; - File
; - Dir
; represents either a file or a (sub-) directory in a directory structure
; Every File and every Dir can serve as an Entry example


(define (entry-temp entry)
  (...
   (cond
     [(file? entry) (...(file-temp entry)...)]
     [(dir? entry) (...(dir-temp entry)...)])))

(define (loe-temp loe)
  (...
   (cond
     [(empty? loe)...]
     [(cons? loe)...(entry-temp (first loe))
                 (loe-temp (rest loe))])))


(define D-P (make-dir "Personal" (list F-T)))
(define D-R (make-dir "Racket"   (list F-R)))
(define D-C (make-dir "Course"   (list F-C)))
(define D-T (make-dir "Wahl"     (list D-P D-R D-C)))
(define D-J (make-dir "Park"     (list F-J)))
(define D-L (make-dir "Razzaq"   (list F-L)))
(define D-H (make-dir "Home"     (list D-T D-J D-L)))

; count-entries/d: Dir -> Nat
; counts the number of entries in the given directory

(check-expect (count-entries/d D-P) 2)
(check-expect (count-entries/d D-H) 12)


(define (count-entries/d dir)
  (+ 1 (count-entries/loe (dir-entries dir))))

; count-entries/loe: [List-of Entry] -> Nat
; counts the number of entries in the given list

(check-expect (count-entries/loe '()) 0)
(check-expect (count-entries/loe (list F-L)) 1)
(check-expect (count-entries/loe (list D-H)) 12)

(define (count-entries/loe loe)
  (foldr + 0 (map count-entries/e loe)))


; count-entries/e: Entry -> Nat
; counts the number of entries in the given entry

(check-expect (count-entries/e F-T) 1)
(check-expect (count-entries/e D-H) 12)

(define (count-entries/e entry)
  (cond
    [(file? entry) 1]
    [(dir? entry) (count-entries/d entry)]))


; for counting files we wouldnt have the +1 in the entries/dir

; compute the storage needed for a directory
; - (compute the storage needed for a )file : lenght of file name+ file size
; - length of dir name + sum of storage entries


; du/d: Dir -> Nat
; the storage required for d

(check-expect (du/d D-P) (+ 8 10 1200))
(check-expect (du/d D-R) (+ 6 8 1000000))
(check-expect (du/d D-C) (+ 6 11 10000))
(check-expect (du/d D-T) (+ 4 (du/d D-P) (du/d D-R) (du/d D-C)))
(check-expect (du/d D-J) (+ 4 8 1400))
(check-expect (du/d D-L) (+ 6 9 1300))
(check-expect (du/d D-H) (+ 4 (du/d D-T) (du/d D-J) (du/d D-L)))


(define (du/d dir)
  (+ (string-length (dir-name dir))
     (du/loe (dir-entries dir))))

; du/loe: [List-of Entry] -> Nat
; compute the storage require for the list of entries

(check-expect (du/loe '()) 0)
(check-expect (du/loe (list D-H)) (du/d D-H))
(check-expect (du/loe (list F-R)) (+ 8 1000000))

(define (du/loe loe)
  (foldr + 0 (map du/e loe)))


; du/e: Entry -> Nat
; compute the storage require for an entry

(check-expect (du/e D-H) (du/d D-H))
(check-expect (du/e F-R) (+ 8 1000000))

(define (du/e entry)
  (cond
    [(file? entry) (+ (string-length (file-name entry))
                      (file-size entry))]
    [(dir? entry) (du/d entry)]))


; Policy: require all executable files to be named in all lower-case letters

; large-caps-executable/d: Dir -> [List-of String]
; lists the names of the files in the given directory that violates the above policy

(check-expect (large-caps-executable/d D-H) '())
(check-expect (large-caps-executable/d (make-dir "Temp" (list (make-file "DrRacket" 1000 #f #t))))
              (list "DrRacket"))

(define (large-caps-executable/d dir)
  (foldr append '() (map large-caps-executable/e (dir-entries dir))))


; large-caps-executable/e : Entry -> [List-of String]
; gets the list of names of the files in the given entry that violates the above policy

(check-expect (large-caps-executable/e D-H) '())
(check-expect (large-caps-executable/e (make-dir "Temp" (list (make-file "DrRacket" 1000 #f #t))))
              (list "DrRacket"))
(check-expect (large-caps-executable/e F-R) '())

(define (large-caps-executable/e entry)
  (cond
    [(file? entry) (large-caps-executable/f entry)]
    [(dir? entry) (large-caps-executable/d entry)]))



; large-caps-executable/f : File -> [List-of String]
; if f violates the policy, its name is put in a list, otherwise return '()

(check-expect (large-caps-executable/f F-R) '())
(check-expect (large-caps-executable/f (make-file "DrRacket" 1000 #f #t)) (list "DrRacket"))

(define (large-caps-executable/f file)
  (local [(define fname (file-name file))
          (define loc (explode fname))]  
    (if (and (file-exec? file)
             (ormap (λ (s) (not (string-lower-case? s))) loc))
        (list fname)
        '())))

;---------------------------------------------------------------------------------------------
; (a)

; find: Dir [File  -> Boolean] -> [List-of Strings]
; returns a list of strings, of files that satisty the predicates in the directory
(check-expect (find D-H (tester "2500-grades")) (list "Home/Wahl/Course/2500-grades"))
(check-expect (find D-P (tester "thomas.jpg")) (list "Personal/thomas.jpg"))
(check-expect (find D-L (tester "leena.jpg")) (list "Razzaq/leena.jpg"))
(check-expect (find D-J file-write?) '())
(check-expect (find D-J file-exec?) '())


(define (find dir test?)
  (map (λ (str) (string-append (dir-name dir) "/"  str)) (find/loe (dir-entries dir) test?)))

; find/loe : [List-of Entry][File  -> Boolean] -> [List-of String]
; returns a list of strings from the list of entry that satisy the predicate
(check-expect (find/loe (dir-entries D-H) (tester "2500-grades")) (list "Wahl/Course/2500-grades"))
(check-expect (find/loe (dir-entries D-P) (tester "thomas.jpg")) (list "thomas.jpg"))


(define (find/loe loe test?)
  (foldr (λ (entry los) (append (find/e entry test?) los))'() loe))

; find/e: Entry [File  -> Boolean] -> [List-of String]
; gets the list of names of the files in the given entry that satisfy the predicate
(check-expect (find/e D-H (tester "2500-grades")) (list "Home/Wahl/Course/2500-grades"))
(check-expect (find/e D-P (tester "thomas.jpg")) (list "Personal/thomas.jpg"))

(define (find/e entry test?)
  (cond
    [(dir? entry) (find entry test?)]
    [(file? entry) (if (test? entry)
                       (list (file-name entry))
                       '())]))
                    
; (b)
; find-by-name: Dir String -> [List-of String]
; returns quickly the file name from a directory and an inputed list

(check-expect (find-by-name D-H "2500-grades") (list "Home/Wahl/Course/2500-grades"))
(check-expect (find-by-name D-L "leena.jpg") (list "Razzaq/leena.jpg"))

(define (find-by-name dir str)
  (find dir (λ (file) (string=? (file-name file) str))))

; tester: String -> [File -> Boolean]
; returns #t the string inputed is same as the file name

(check-expect ((tester "leena.jpg") F-L) #t)
(check-expect ((tester "leena.jpg") F-T) #f)

(define (tester str)
  (λ (file) (string=? (file-name file) str)))

;---------------------------------------------------------------------
; MINE SWEEPER GAME PROJECT

; Exercise 2

; Content is one of:
; - Nat [0 8]
; - "Mine"
; where Nat-Mines represents number of surrounding mines and Mine is a string

(define CONTENT-0 8)
(define CONTENT-MINE "Mine")
(define CONTENT-3 0)
(define CONTENT-4 4)

(define (content-temp c)
  (cond
    [(string=? c) ...]
    [(number? c) ...]
    [else ...]))

(define-struct cell [content status])
; A cell is a (make-cell NatNum string)
; it represents the state of a cell
; - content is a nat which represents number of surrounding mines and number of mines
; - status represents if the cell is hidden, visible or flagged

(define CELL-0 (make-cell 0 "Hidden"))
(define CELL-1 (make-cell 2 "Visible"))
(define CELL-2 (make-cell 1 "Flagged"))
(define CELL-3 (make-cell "Mine" "Hidden"))

(define CELL-MH (make-cell "Mine" "Hidden"))
(define CELL-1H (make-cell 1 "Hidden"))
(define CELL-2H (make-cell 2 "Hidden"))
(define CELL-3H (make-cell 3 "Hidden"))

(define (cell-temp c)
  (...
   (content-temp (cell-content c)...
                 (cell-status c)...)))
               
; Exercise 3

; A Row is one of:
; - empty
; - [List-of Cell]
; represents the rows in a board

(define ROW-0 '())
(define ROW-1 (list CELL-0 CELL-1 CELL-2))
(define ROW-2 (list CELL-1 CELL-2 CELL-3))
(define ROW-3 (list CELL-3 CELL-2 CELL-1))


(define ROW-M21 (list CELL-MH CELL-2H CELL-1H))
(define ROW-23M (list CELL-2H CELL-3H CELL-MH))
(define ROW-1M2 (list CELL-1H CELL-MH CELL-2H))


(define (row-temp r)
  (...
   (cond
     [(empty? r)...]
     [(cons? r)...(first r)
               (row-temp (rest r))...])))

; A Board is one of:
; - empty
; - [List-of row]
; represents the minesweeper board

(define BOARD-0 '())
(define BOARD-1 (list ROW-1 ROW-2 ROW-3))
(define BOARD-3X3 (list ROW-M21 ROW-23M ROW-1M2))

(define (board-temp b)
  (...
   (cond
     [(empty? b)...]
     [(cons? b)...(first b)
               (board-temp (rest b))...])))

; Exercise 4
(define-struct game [board reveal?])
; A Game is a (make-game Board Boolean)
; represents the whole minesweeper game
; - board represents the minesweeper board
; - reveal? simulates the mouse click where
;   #t is interpreted to mean that mouse clicks unhide
;   #f is interpreted to mean that mouse clicks flag

(define GAME-3X3 (make-game BOARD-3X3 #t))
; represents the initial state of the game where everything is hidden

(define (game-temp g)
  (...
   (board-temp (game-board g)...
               (game-reveal? g))...))
  
; Exercise 5

(require 2htdp/image)
(require 2htdp/universe)



; random-game-generation: Nat Nat -> Game
; creates a game with a new board based on the inputed n and m which is the number of mine

(define (random-game-generation n m)
  GAME-3X3)

; mine-sweeper: Nat Nat -> Game
; runs the minesweeper game with a randomly generated board

(define (mine-sweeper n m)
  (if (> m (sqr n))
      (error "Too many mines relative to board size")
      (mine-sweeper-from (random-game-generation n m))))


(define (mine-sweeper-from game)
  (big-bang game
    [to-draw draw-game]
    [on-mouse mouse-click]
    [on-key process-key]
    [stop-when game-over?]))

; draw-game: Game -> Image
; creates an image of the game from the inputed values

(define (draw-game game)
  empty-image)

; mouse-click: Game Integers Integers MouseEvent -> Game
; responds to the unhiding and flagging

(define (mouse-click game)
  game)


; Exercise 6
; process-key: Game KeyEvent -> Game
; toggles the mouse button between
; "left" (used to unhide cells) and "right"(used to flag suspected mines) using the spacebar

(check-expect (process-key (make-game BOARD-3X3 #t) " ") (make-game BOARD-3X3 #f))
(check-expect (process-key (make-game BOARD-3X3 #f) " ") (make-game BOARD-3X3 #t))

(define (process-key game key)
  (cond
    [(and (key=? key " ") (game-reveal? game)) (make-game (game-board game) #f)]
    [(and (key=? key " ") (not(game-reveal? game))) (make-game (game-board game) #t)]
    [else game]))

; Exercise 7
; game-over?: Game -> String
; the game is over when the winning condition holds, or the losing condition holds.

(define CELL-MINE (make-cell "Mine" "Visible"))
(define ROW-MINE (list CELL-0 CELL-MINE CELL-2))
(define BOARD-MINE (list ROW-1 ROW-MINE ROW-3))
(define GAME-MINE (make-game BOARD-MINE #t))

; game-over? : Game -> Boolean
; returns true if the winning or losing condition is true
(check-expect (game-over? GAME-MINE) #t)
(check-expect (game-over? GAME-3X3) #f)

(define (game-over? game)
  (or (did-you-lose? game) (did-you-win? game)))

; did-you-lose? : Game -> Boolean
; checks the game for any visible mines and returns true once is found.
(check-expect (did-you-lose? GAME-MINE) #t)
(check-expect (did-you-lose? GAME-3X3) #f)

(define (did-you-lose? game)
  (check-board-mine? (game-board game)))

; check-board-mine? : [List of Rows] -> Boolean
; checks every row in the board for any visible mines and returns true once one is found
(check-expect (check-board-mine? BOARD-MINE) #t)
(check-expect (check-board-mine? BOARD-3X3) #f)

(define (check-board-mine? board)
  (cond
    [(empty? board) #f]
    [(list? board) (if (check-row-mine? (first board)) #t (check-board-mine? (rest board)))]))


; check-row-mine? : [List of Cells] -> Boolean
; checks every cell in the row for any visible mines and returns true once one is found
(check-expect (check-row-mine? ROW-MINE) #t)
(check-expect (check-row-mine? ROW-M21) #f)

(define (check-row-mine? row)
  (cond
    [(empty? row) #f]
    [(number? (cell-content (first row)))
     (if (and (string=? (number->string (cell-content (first row))) "Mine")
              (string=? (cell-status (first row)) "Visible"))
         #t
         (check-row-mine? (rest row)))]
    [(string? (cell-content (first row)))
     (if (and (string=? (cell-content (first row)) "Mine")
              (string=? (cell-status (first row)) "Visible"))
         #t
         (check-row-mine? (rest row)))]))

; did-you-win? : Game -> Boolean
; checks the game for any visible mines and returns
; #t if none are found and every non-mine cell is visible
(check-expect (did-you-win? GAME-MINE) #f)
(check-expect (did-you-win? GAME-3X3) #f)

(define (did-you-win? game)
  (check-board-safe? (game-board game)))

; check-board-safe? : [List of Rows] -> Boolean
; checks every row in the board for visible mines and returns
; #t if none are found and every non-mine cell is visible
(check-expect (check-board-safe? BOARD-MINE) #f)
(check-expect (check-board-safe? BOARD-3X3) #f)

(define (check-board-safe? board)
  (cond
    [(empty? board) #f]
    [(list? board) (if
                    (check-row-safe? (first board))
                    #t
                    (check-board-safe? (rest board)))]))

; check-row-safe? : [List of Cells] -> Boolean
; checks every cell in the row for a visible mine and
; returns #t if none are found and every non-mine cell is visible
(check-expect (check-row-safe? ROW-MINE) #f)
(check-expect (check-row-safe? ROW-M21) #f)

(define (check-row-safe? row)
  (cond
    [(= (length row) 0) #t]
    [(and (number? (cell-content (first row)))
          (not (string=? (cell-status (first row)) "Visible"))) #f]
    [(and (string? (cell-content (first row)))
          (string=? (cell-status (first row)) "Visible"))
     #f]
    [else (check-row-safe? (rest row))]))







  


         




  


  






