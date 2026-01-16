#lang racket
(require math/base)

;; --- Input Reading and Parsing ---
(define (read-input filename)
  (file->lines filename))

(define (parse-input raw-input)
  ;; Example: Convert lines of numbers to a list of numbers
  (map string->number raw-input))

;; --- Part 1 ---

(define (one-if-fresh fresh-list item)
  (if (empty? fresh-list)
    0
    (let ([range-to-check (car fresh-list)])
      (if (or (< item (car range-to-check)) (> item (cadr range-to-check)))
        (one-if-fresh (cdr fresh-list) item)
        1))))

(define (tally-fresh-loop input fresh-list tally)
  (if (empty? input)
    tally
    (tally-fresh-loop (cdr input) fresh-list (+ tally (one-if-fresh fresh-list (car input))))))

(define (generate-fresh-list input fresh-list)
  (if (non-empty-string? (car input))
    (generate-fresh-list (cdr input) (cons (map string->number (string-split (car input) "-")) fresh-list))
    (tally-fresh-loop (map string->number (cdr input)) fresh-list 0)))

(define (solve-part1 input)
  ;; Implement logic for Part 1
  (displayln "Solving Part 1...")
  (generate-fresh-list input '()))

;; --- Part 2 ---
(define (find-fresh-quantity sorted-fresh-list previous-range tally)
  (if (empty? sorted-fresh-list)
    (+ tally 1 (- (cadr previous-range) (car previous-range)))
    (let ([new-range (car sorted-fresh-list)])
      (if (> (car new-range) (cadr previous-range))
        (find-fresh-quantity (cdr sorted-fresh-list) new-range (+ tally 1 (- (cadr previous-range) (car previous-range))))
        (if (> (cadr new-range) (cadr previous-range))
          (find-fresh-quantity (cdr sorted-fresh-list) (list (car previous-range) (cadr new-range)) tally)
          (find-fresh-quantity (cdr sorted-fresh-list) previous-range tally))))))

(define (generate-sorted-fresh-list input fresh-list)
  (if (non-empty-string? (car input))
    (generate-sorted-fresh-list (cdr input) (cons (map string->number (string-split (car input) "-")) fresh-list))
    (let ([sorted-list (sort fresh-list (lambda (x y) (< (car x) (car y))))])
      (find-fresh-quantity (cdr sorted-list) (car sorted-list) 0))))

(define (solve-part2 input)
  ;; Implement logic for Part 2
  (displayln "Solving Part 2...")
  (generate-sorted-fresh-list input '()))

;; --- Main Execution ---
(define input-filename "input")

(define string-input (read-input input-filename))
(define number-input (parse-input string-input))

(displayln (format "Part 1 Solution: ~a" (solve-part1 string-input)))
(displayln (format "Part 2 Solution: ~a" (solve-part2 string-input)))
