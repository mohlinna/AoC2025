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
(define (find-quantum-tachyon-splits-loop input current-beams next-beams)
  (if (empty? input)
    (sum (map (lambda (x) (cdr x)) current-beams))
    (if (empty? current-beams)
      (find-quantum-tachyon-splits-loop (cddr input) (hash->list next-beams) (hash))
      (let ([current (caar current-beams)]
            [current-quantity (cdar current-beams)])
        (if (equal? #\^ (vector-ref (car input) current))
          (let* ([one-above (+ current 1)]
                 [one-above-quantity (hash-ref next-beams one-above 0)]
                 [new-one-above-quantity (+ current-quantity one-above-quantity)]
                 [one-below (- current 1)]
                 [one-below-quantity (hash-ref next-beams one-below 0)]
                 [new-one-below-quantity (+ current-quantity one-below-quantity)])
            (find-quantum-tachyon-splits-loop input (cdr current-beams) (hash-set (hash-set next-beams one-above new-one-above-quantity) one-below new-one-below-quantity)))
          (let ([new-current-quantity (+ current-quantity (hash-ref next-beams current 0))])
            (find-quantum-tachyon-splits-loop input (cdr current-beams) (hash-set next-beams current new-current-quantity))))))))

(define (solve-part2 input)
  ;; Implement logic for Part 2
  (displayln "Solving Part 2...")
  (find-quantum-tachyon-splits-loop
    (map list->vector (map string->list (cddr input)))
    (list (cons (string-find (car input) "S") 1))
    (hash)))

;; --- Main Execution ---
(define input-filename "input")

(define string-input (read-input input-filename))
(define number-input (parse-input string-input))

(displayln (format "Part 1 Solution: ~a" (solve-part1 string-input)))
(displayln (format "Part 2 Solution: ~a" (solve-part2 string-input)))
