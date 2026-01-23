#lang racket
(require math/base)

;; --- Input Reading and Parsing ---
(define (read-input filename)
  (file->lines filename))

(define (parse-input raw-input)
  ;; Example: Convert lines of numbers to a list of numbers
  (map string->number raw-input))

;; --- Part 1 ---
(define (button-to-binary number-list result)
  (if (empty? number-list)
    result
    (button-to-binary (cdr number-list) (bitwise-ior result (arithmetic-shift 1 (car number-list))))))

(define (buttons-to-binary input output)
  (if (equal? #\{ (string-ref (car input) 0))
    output
    (let* ([number-list (map string->number (string-split (string-replace (string-replace (car input) "(" "") ")" "" ) ","))]
           [button (button-to-binary number-list 0)])
      (buttons-to-binary (cdr input) (cons button output)))))

(define (indicator-to-binary indicator result)
  (if (empty? indicator)
    result
    (if (equal? (car indicator) #\#)
      (indicator-to-binary (cdr indicator) (bitwise-ior (arithmetic-shift result 1) 1))
      (if (equal? (car indicator) #\.)
        (indicator-to-binary (cdr indicator) (arithmetic-shift result 1))
        (indicator-to-binary (cdr indicator) result)))))

(define (choose-combo-loop x y result)
  ;(displayln (format "x: ~a.  y: ~a.  result: ~a." x y result))
  (cond 
    [(= y 0) (list (list))]
    [(< x y) result]
    [(= x y) (cons (range x) result)]
    [else
      (let* ([under-result (choose-combo-loop (- x 1) (- y 1) '())]
             [current-result (map (lambda (l) (cons (- x 1) l)) under-result)])
        (choose-combo-loop (- x 1) y (append current-result result)))]))
      
(define (x-choose-y-combos x y)
  (choose-combo-loop x y '()))
  
(define (check-combos combos indicator buttons)
  (if (empty? combos)
    #f
    (let* ([buttons-pressed (map (lambda (x) (vector-ref buttons x)) (car combos))]
           [button-press-result (apply bitwise-xor buttons-pressed)])
      (if (= button-press-result indicator)
        #t
        (check-combos (cdr combos) indicator buttons)))))
  
(define (find-presses-for-machine-loop indicator buttons num-buttons presses)
  (let ([combos (x-choose-y-combos num-buttons presses)])
    (if (check-combos combos indicator buttons)
      presses
      (find-presses-for-machine-loop indicator buttons num-buttons (+ 1 presses)))))

(define (find-presses-for-machine input)
  (let* ([splits (string-split input)]
         [indicator (indicator-to-binary (reverse (string->list (car splits))) 0)]
         [buttons (list->vector (buttons-to-binary (cdr splits) '()))]
         [num-buttons (vector-length buttons)])
    (find-presses-for-machine-loop indicator buttons num-buttons 1)))

(define (solve-part1 input)
  ;; Implement logic for Part 1
  (displayln "Solving Part 1...")
  (sum (map find-presses-for-machine input)))

;; --- Part 2 ---
(define (joltages-to-vector input)
  (list->vector (string-split (string-replace (string-replace input "{" "") "}" "" ) ",")))

(define (buttons-and-joltages-to-lists input output)
  (if (equal? #\{ (string-ref (car input) 0))
    (cons (joltages-to-vector (car input)) output)
    (let* ([number-list (map string->number (string-split (string-replace (string-replace (car input) "(" "") ")" "" ) ","))])
      (buttons-and-joltages-to-lists (cdr input) (cons number-list output)))))

(define (find-presses-for-joltage input)
  (let* ([splits (string-split input)]
         [buttons-and-joltages (buttons-and-joltages-to-lists (cdr splits) '())]
         [joltages (car buttons-and-joltages)]
         [buttons (cdr buttons-and-joltages)])
    buttons-and-joltages))
    ;(find-presses-for-joltage-loop joltages buttons)))

(define (solve-part2 input)
  ;; Implement logic for Part 2
  (displayln "Solving Part 2...")
  (map find-presses-for-joltage input))

;; --- Main Execution ---
(define input-filename "test_input")

(define string-input (read-input input-filename))
(define number-input (parse-input string-input))

(displayln (format "Part 1 Solution: ~a" (solve-part1 string-input)))
(displayln (format "Part 2 Solution: ~a" (solve-part2 string-input)))
