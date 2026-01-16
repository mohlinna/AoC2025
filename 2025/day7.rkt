#lang racket
(require math/base)

;; --- Input Reading and Parsing ---
(define (read-input filename)
  (file->lines filename))

(define (parse-input raw-input)
  ;; Example: Convert lines of numbers to a list of numbers
  (map string->number raw-input))

;; --- Part 1 ---
(define (find-tachyon-splits-loop input current-beams next-beams tally)
  (if (empty? input)
    tally
    (if (empty? current-beams)
      (find-tachyon-splits-loop (cddr input) next-beams '() tally)
      (let ([current (car current-beams)])
        (if (equal? #\^ (vector-ref (car input) current))
          (find-tachyon-splits-loop input (cdr current-beams) (set-add (set-add next-beams (+ current 1)) (- current 1)) (+ 1 tally))
          (find-tachyon-splits-loop input (cdr current-beams) (set-add next-beams current) tally))))))

(define (solve-part1 input)
  ;; Implement logic for Part 1
  (displayln "Solving Part 1...")
  (find-tachyon-splits-loop
    (map list->vector (map string->list (cddr input)))
    (list (string-find (car input) "S"))
    '()
    0))

;; --- Part 2 ---
(define (solve-part2 input)
  ;; Implement logic for Part 2
  (displayln "Solving Part 2...")
  2)

;; --- Main Execution ---
(define input-filename "input")

(define string-input (read-input input-filename))
(define number-input (parse-input string-input))

(displayln (format "Part 1 Solution: ~a" (solve-part1 string-input)))
(displayln (format "Part 2 Solution: ~a" (solve-part2 string-input)))
