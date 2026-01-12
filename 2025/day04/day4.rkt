#lang racket
(require math/base)

;; --- Input Reading and Parsing ---
(define (read-input filename)
  (file->lines filename))

(define (parse-input raw-input)
  ;; Example: Convert lines of numbers to a list of numbers
  (map string->number raw-input))

;; --- Part 1 ---
(define adjacent-points
  '((-1 . -1) (-1 .  0) (-1 .  1)
    ( 0 . -1)           ( 0 .  1)
    ( 1 . -1) ( 1 .  0) ( 1 .  1)))
    
(define (contains-roll? vector-grid row column)
  (eq? #\@ (vector-ref (vector-ref vector-grid row) column)))

(define (look-up-adjacent-point vector-grid row column num-rows num-columns)
  (if (or (< row 0) (< column 0) (>= row num-rows) (>= column num-columns))
    0
    (if (contains-roll? vector-grid row column)
      1
      0)))
      
(define (surrounding-rolls vector-grid row column num-rows num-columns)
  (sum (map (lambda (x) (look-up-adjacent-point vector-grid (+ row (car x)) (+ column (cdr x)) num-rows num-columns)) adjacent-points)))

(define (point-value vector-grid row column num-rows num-columns)
  (if (and (contains-roll? vector-grid row column)
           (< (surrounding-rolls vector-grid row column num-rows num-columns) 4))
    1
    0))
    
(define (tally-row-loop vector-grid row column tally num-rows num-columns)
  (if (< column num-columns)
    (let ([point (point-value vector-grid row column num-rows num-columns)])
      (tally-row-loop vector-grid row (+ 1 column) (+ tally point) num-rows num-columns))
    tally))
    
(define (part1-loop vector-grid row tally num-rows num-columns)
  (if (< row num-rows)
    (let ([row-tally (tally-row-loop vector-grid row 0 0 num-rows num-columns)])
      (part1-loop vector-grid (+ 1 row) (+ tally row-tally) num-rows num-columns))
    tally))

(define (solve-part1 input)
  ;; Implement logic for Part 1
  (displayln "Solving Part 1...")
  (let ([numRows (length input)]
        [numColumns (string-length (car input))]
        [vector-grid (list->vector (map (lambda (x) (list->vector (string->list x))) input))])
    (part1-loop vector-grid 0 0 numRows numColumns)))

;; --- Part 2 ---
(define (find-removable-rolls-in-row vector-grid row column roll-list num-rows num-columns)
  (if (< column num-columns)
    (if (is-accessible-roll? vector-grid row column num-rows num-columns)
      (find-removable-rolls-in-row vector-grid row (+ 1 column) (cons (cons row column) roll-list) num-rows num-columns)
      (find-removable-rolls-in-row vector-grid row (+ 1 column) roll-list num-rows num-columns))
    roll-list))

(define (find-removable-rolls vector-grid row roll-list num-rows num-columns)
  (if (< row num-rows)
    (let ([row-rolls(find-removable-rolls-in-row  vector-grid row 0 roll-list num-rows num-columns)])
      (find-removable-rolls vector-grid (+ 1 row) row-rolls num-rows num-columns))
    roll-list))

(define (is-accessible-roll? vector-grid row column num-rows num-columns)
  (and (contains-roll? vector-grid row column)
       (< (surrounding-rolls vector-grid row column num-rows num-columns) 4)))

(define (remove-roll! vector-grid row column)
  (vector-set! (vector-ref vector-grid row) column #\R))
  
(define (remove-rolls vector-grid roll-list)
  (if (empty? roll-list)
    vector-grid
    (let ([roll (car roll-list)])
      (remove-roll! vector-grid (car roll) (cdr roll))
      (remove-rolls vector-grid (cdr roll-list)))))

(define (part2-loop vector-grid tally num-rows num-columns)
  (let ([removable-rolls (find-removable-rolls vector-grid 0 '() num-rows num-columns)])
    (if (empty? removable-rolls)
      tally
      (let ([new-grid (remove-rolls vector-grid removable-rolls)])
        (part2-loop new-grid (+ tally (length removable-rolls)) num-rows num-columns)))))

(define (solve-part2 input)
  ;; Implement logic for Part 2
  (displayln "Solving Part 2...")
  (let ([numRows (length input)]
        [numColumns (string-length (car input))]
        [vector-grid (list->vector (map (lambda (x) (list->vector (string->list x))) input))])
    (part2-loop vector-grid 0 numRows numColumns)))

;; --- Main Execution ---
(define input-filename "input")

(define string-input (read-input input-filename))
(define number-input (parse-input string-input))

(displayln (format "Part 1 Solution: ~a" (solve-part1 string-input)))
(displayln (format "Part 2 Solution: ~a" (solve-part2 string-input)))
