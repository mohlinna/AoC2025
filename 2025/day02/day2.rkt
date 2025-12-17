#lang racket
(require math/base)

;; --- Input Reading and Parsing ---
(define (read-to-string filename)
  (file->string filename))

(define (read-input filename)
  (file->lines filename))

(define (parse-input raw-input)
  ;; Example: Convert lines of numbers to a list of numbers
  (map string->number raw-input))

;; --- Part 1 ---
(define (find-first-prefix num-str)
  (let* ([prefix-len (/ (string-length num-str) 2)]
         [first-half (substring num-str 0 prefix-len)]
         [second-half (substring num-str prefix-len)]
         [first-half-num (string->number first-half)]
         [second-half-num (string->number second-half)])
    (if (> first-half-num second-half-num)
      first-half
      (number->string (+ 1 first-half-num)))))

(define (find-last-prefix num-str)
  (let* ([prefix-len (/ (string-length num-str) 2)]
         [first-half (substring num-str 0 prefix-len)]
         [second-half (substring num-str prefix-len)]
         [first-half-num (string->number first-half)]
         [second-half-num (string->number second-half)])
    (if (> second-half-num first-half-num)
      first-half
      (number->string (- first-half-num 1)))))

(define (calc-sum-from-invalid-prefixes first-prefix last-prefix)
  (let* ([set-size (+ 1 (- (string->number last-prefix) (string->number first-prefix)))]
         [first-num (string->number (string-append first-prefix first-prefix))]
         [last-num (string->number (string-append last-prefix last-prefix))])
    (* set-size (/ (+ first-num last-num) 2))))

(define (find-invalid-values-sum range)
  (let* ([range-list (string-split range "-")]
         [first (car range-list)]
         [last (cadr range-list)]
         [first-len (string-length first)]
         [last-len (string-length last)]
         [first-odd (odd? first-len)]
         [last-odd (odd? last-len)])
    (if (and first-odd last-odd)
      0
      (if first-odd
        (calc-sum-from-invalid-prefixes
          (string-append "1" (make-string (- (/ last-len 2) 1) #\0))
          (find-last-prefix last))
        (if last-odd
          (calc-sum-from-invalid-prefixes
            (find-first-prefix first)
            (make-string (/ first-len 2) #\9))
          (calc-sum-from-invalid-prefixes
            (find-first-prefix first)
            (find-last-prefix last)))))))

(define (part1-loop range-list running-sum)
  (if (empty? range-list)
    running-sum
    (let ([range-sum (find-invalid-values-sum (car range-list))])
      (part1-loop (cdr range-list) (+ range-sum running-sum)))))

(define (solve-part1 input)
  ;; Implement logic for Part 1
  (displayln "Solving Part 1...")
  (part1-loop (string-split input ",") 0))

;; --- Part 2 ---
(define (string-repeat n str)
  (string-append* (make-list n str)))

(define (find-first-prefix2 num-str prefix-size)
  (let* ([prefix (substring num-str 0 prefix-size)]
         [prefix-num (string->number prefix)]
         [potential-invalid-num (string->number (string-repeat (/ (string-length num-str) prefix-size) prefix))])
    (if (>= potential-invalid-num (string->number num-str))
      prefix-num
      (+ 1 prefix-num))))

(define (find-last-prefix2 num-str prefix-size)
  (let* ([prefix (substring num-str 0 prefix-size)]
         [prefix-num (string->number prefix)]
         [potential-invalid-num (string->number (string-repeat (/ (string-length num-str) prefix-size) prefix))])
    (if (>= (string->number num-str) potential-invalid-num)
      prefix-num
      (- prefix-num 1))))

(define (find-invalid-values2 first-prefix-num last-prefix-num multiplier invalid-set)
  (if (> first-prefix-num last-prefix-num)
    invalid-set
    (let ([new-invalid (string->number (string-repeat multiplier (number->string first-prefix-num)))])
      (find-invalid-values2 (+ 1 first-prefix-num) last-prefix-num multiplier (set-add invalid-set new-invalid)))))

(define (find-invalid-values-sum2 range-list prefix-size-list invalid-set)
  (if (empty? prefix-size-list)
    (sum (set->list invalid-set))
    (let* ([first (car range-list)]
           [last (cadr range-list)]
           [len (string-length first)]
           [prefix-size (car prefix-size-list)]
           [first-prefix (find-first-prefix2 first prefix-size)]
           [last-prefix (find-last-prefix2 last prefix-size)]
           [new-invalid-set (find-invalid-values2 first-prefix last-prefix (/ len prefix-size) (set))])
       (find-invalid-values-sum2 range-list (cdr prefix-size-list) (set-union new-invalid-set invalid-set)))))

(define (find-prefix-sizes-to-check num)
  (case (string-length num)
    [(1) '()]
    [(4) '(2)]
    [(6) '(2 3)]
    [(8) '(4)]
    [(9) '(3)]
    [(10) '(2 5)]
    [else '(1)]))

(define (part2-loop range-list running-sum)
  (if (empty? range-list)
    running-sum
    (let ([range-sum (find-invalid-values-sum2 
                       (car range-list) 
                       (find-prefix-sizes-to-check (caar range-list))
                       (set))])
      (part2-loop (cdr range-list) (+ range-sum running-sum)))))

(define (create-ranges-list raw-list output-list)
  (if (empty? raw-list)
    output-list
    (let* ([range-list (string-split (car raw-list) "-")]
           [first (car range-list)]
           [last (cadr range-list)]
           [first-len (string-length first)]
           [last-len (string-length last)])
      (create-ranges-list (cdr raw-list)
        (if (= first-len last-len)
          (cons range-list output-list)
          (cons (list first (make-string first-len #\9))
                (cons (list (string-append "1" (make-string first-len #\0)) last)
                      output-list)))))))

(define (solve-part2 input)
  ;; Implement logic for Part 2
  (displayln "Solving Part 2...")
  (part2-loop (create-ranges-list (string-split input ",") '()) 0))

;; --- Main Execution ---
(define input-filename "input")

(define single-string-input (read-to-string input-filename))
(define string-input (read-input input-filename))
(define number-input (parse-input string-input))

(displayln (format "Part 1 Solution: ~a" (solve-part1 (car string-input))))
(displayln (format "Part 2 Solution: ~a" (solve-part2 (car string-input))))
