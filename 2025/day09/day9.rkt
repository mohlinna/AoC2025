#lang racket
(require math/base)

;; --- Input Reading and Parsing ---
(define (read-input filename)
  (file->lines filename))

(define (parse-input raw-input)
  ;; Example: Convert lines of numbers to a list of numbers
  (map string->number raw-input))

;; --- Part 1 ---
(define (find-sizes-from point other-points sizes-list)
  (if (empty? other-points)
    sizes-list
    (let* ([next-point (car other-points)]
           [size (* (- (car point) (car next-point) -1)
                    (- (cadr point) (cadr next-point) -1))])
      (find-sizes-from point (cdr other-points) (cons (list size point next-point) sizes-list)))))

(define (find-all-sizes input sizes-list)
  (if (empty? input)
    (sort sizes-list (lambda (x y) (> (car x) (car y))))
    (let ([new-size-list (find-sizes-from (car input) (cdr input) sizes-list)])
      (find-all-sizes (cdr input) new-size-list))))

(define (solve-part1 input)
  ;; Implement logic for Part 1
  (displayln "Solving Part 1...")
  (let* ([all-sizes 
           (find-all-sizes
             (map (lambda (x) (map string->number (string-split x ","))) input)
             '())])
  (caar all-sizes)))

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
