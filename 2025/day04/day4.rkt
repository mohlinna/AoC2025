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
    
(define (tally-row-loop input-row vector-grid row column tally num-rows num-columns)
  (if (empty? input-row)
    tally
    (let ([point (point-value vector-grid row column num-rows num-columns)])
    (tally-row-loop (cdr input-row) vector-grid row (+ 1 column) (+ tally point) num-rows num-columns))))
    
    
(define (part1-loop input vector-grid row tally num-rows num-columns)
  (if (empty? input)
    tally
    (let ([row-tally (tally-row-loop (string->list (car input)) vector-grid row 0 0 num-rows num-columns)])
      (part1-loop (cdr input) vector-grid (+ 1 row) (+ tally row-tally) num-rows num-columns))))

(define (solve-part1 input)
  ;; Implement logic for Part 1
  (displayln "Solving Part 1...")
  (let ([numRows (length input)]
        [numColumns (string-length (car input))]
        [vector-grid (list->vector (map (lambda (x) (list->vector (string->list x))) input))])
    (displayln (format "  numRows: ~a  numColumns: ~a" numRows numColumns))
    (part1-loop input vector-grid 0 0 numRows numColumns)))

;; --- Part 2 ---
(define (solve-part2 parsed-input)
  ;; Implement logic for Part 2
  (displayln "Solving Part 2...")
  2)
  ;;(print read))

;; --- Main Execution ---
(define input-filename "input")

(define string-input (read-input input-filename))
(define number-input (parse-input string-input))

(displayln (format "Part 1 Solution: ~a" (solve-part1 string-input)))
(displayln (format "Part 2 Solution: ~a" (solve-part2 string-input)))
