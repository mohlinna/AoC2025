#lang racket
(require math/base)

;; --- Input Reading and Parsing ---
(define (read-input filename)
  (file->lines filename))

(define (parse-input raw-input)
  ;; Example: Convert lines of numbers to a list of numbers
  (map string->number raw-input))

;; --- Part 1 ---
(define char->digit-map 
  '#hash((#\0 . 0)
         (#\1 . 1)
         (#\2 . 2)
         (#\3 . 3)
         (#\4 . 4)
         (#\5 . 5)
         (#\6 . 6)
         (#\7 . 7)
         (#\8 . 8)
         (#\9 . 9)))

(define (find-joltage-from-bank-loop bank first second)
  (let ([current (car bank)]
        [upcoming (cdr bank)])
    (if (empty? upcoming)
      (+ (* 10 first) (max second current))
      (if (> current first)
        (find-joltage-from-bank-loop upcoming current 0)
        (find-joltage-from-bank-loop upcoming first (max second current))))))
        
(define (convert-bank-string-to-list bank-str)
  (map (lambda (x) (dict-ref char->digit-map x)) (string->list bank-str)))

(define (solve-part1 input)
  ;; Implement logic for Part 1
  (displayln "Solving Part 1...")
  (sum (map (lambda (x) (find-joltage-from-bank-loop x 0 0)) 
         (map convert-bank-string-to-list input))))

;; --- Part 2 ---
(define (find-max-and-position bank max max-pos counter)
  (if (empty? bank)
    (list max max-pos)
    (let ([current (car bank)])
      (if (> current max)
        (find-max-and-position (cdr bank) current counter (+ counter 1))
        (find-max-and-position (cdr bank) max max-pos (+ counter 1))))))

(define (find-batteries bank n accum)
  (if (= n 0)
    accum
    (let* ([len (string-length bank)]
           [end-search-point (- len n -1)]
           [portion-to-search (substring bank 0 end-search-point)]
           [max-and-position (find-max-and-position (convert-bank-string-to-list portion-to-search) 0 0 0)]
           [max (car max-and-position)]
           [pos (cadr max-and-position)])
    (find-batteries (substring bank (+ pos 1)) (- n 1) (+ (* 10 accum) max)))))
        
(define (solve-part2 input)
  ;; Implement logic for Part 2
  (displayln "Solving Part 2...")
  (sum (map (lambda (x) (find-batteries x 12 0)) input)))

;; --- Main Execution ---
(define input-filename "input")

(define string-input (read-input input-filename))
(define number-input (parse-input string-input))

(displayln (format "Part 1 Solution: ~a" (solve-part1 string-input)))
(displayln (format "Part 2 Solution: ~a" (solve-part2 string-input)))
