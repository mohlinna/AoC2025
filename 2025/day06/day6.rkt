#lang racket
(require math/base)

;; --- Input Reading and Parsing ---
(define (read-input filename)
  (file->lines filename))

(define (parse-input raw-input)
  ;; Example: Convert lines of numbers to a list of numbers
  (map string->number raw-input))

;; --- Part 1 ---
(define (do-ceph-math equation)
  (if (equal? (car equation) "+")
    (apply + (map string->number (cdr equation)))
    (apply * (map string->number (cdr equation)))))

(define (solve-part1 input)
  ;; Implement logic for Part 1
  (displayln "Solving Part 1...")
  (sum (map do-ceph-math (apply map list (map string-split input)))))

;; --- Part 2 ---
(define (new-ceph-math-loop num-str-list op-list current-list total)
  (if (empty? num-str-list)
    (+ total (apply (car op-list) current-list))
    (if (non-empty-string? (car num-str-list))
      (new-ceph-math-loop (cdr num-str-list) op-list (cons (string->number (car num-str-list)) current-list) total)
      (begin (displayln (format "current-list: ~a.  op:  ~a" current-list (car op-list)))
      (new-ceph-math-loop (cdr num-str-list) (cdr op-list) '() (+ total (apply (car op-list) current-list)))))))

(define (solve-part2 input)
  ;; Implement logic for Part 2
  (displayln "Solving Part 2...")
  (let* ([reversed-input (reverse input)]
         [op-str-list (string-split (car reversed-input))]
         [op-list (map (lambda (x) (if (equal? x "+") + *)) op-str-list)]
         [num-str-list (reverse (cdr reversed-input))]
         [flipped-char-matrix (apply map list (map string->list num-str-list))]
         [new-num-str-list (map (lambda (x) (string-trim (list->string x) #:repeat? #t)) flipped-char-matrix)])
    (new-ceph-math-loop new-num-str-list op-list '() 0)))

;; --- Main Execution ---
(define input-filename "input")

(define string-input (read-input input-filename))
(define number-input (parse-input string-input))

(displayln (format "Part 1 Solution: ~a" (solve-part1 (reverse string-input))))
(displayln (format "Part 2 Solution: ~a" (solve-part2 string-input)))
