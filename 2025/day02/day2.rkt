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
    ;;(displayln (format "  num-str: ~a.  prefix-len: ~a.  first-half: ~a.  second-half: ~a" num-str prefix-len first-half second-half))
    (if (> second-half-num first-half-num)
      first-half
      (number->string (- first-half-num 1)))))

(define (calc-sum-from-invalid-prefixes first-prefix last-prefix)
  (let* ([set-size (+ 1 (- (string->number last-prefix) (string->number first-prefix)))]
         [first-num (string->number (string-append first-prefix first-prefix))]
         [last-num (string->number (string-append last-prefix last-prefix))])
    (displayln (format "  set-size ~a.  first-num ~a.  last-num ~a." set-size first-num last-num))
    (* set-size (/ (+ first-num last-num) 2))))

(define (find-invalid-values-sum range)
  (let* ([range-list (string-split range "-")]
         [first (car range-list)]
         [last (cadr range-list)]
         [first-len (string-length first)]
         [last-len (string-length last)]
         [first-odd (odd? first-len)]
         [last-odd (odd? last-len)])
    (displayln (format "first ~a.  last ~a.  first-len ~a.  last-len ~a" first last first-len last-len))
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
(define (solve-part2 input)
  ;; Implement logic for Part 2
  (displayln "Solving Part 2...")
  2)

;; --- Main Execution ---
(define input-filename "input")

(define single-string-input (read-to-string input-filename))
(define string-input (read-input input-filename))
(define number-input (parse-input string-input))

(displayln (format "Part 1 Solution: ~a" (solve-part1 (car string-input))))
(displayln (format "Part 2 Solution: ~a" (solve-part2 (car string-input))))
