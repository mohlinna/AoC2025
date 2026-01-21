#lang racket
(require math/base)

;; --- Input Reading and Parsing ---
(define (read-input filename)
  (file->lines filename))

(define (parse-input raw-input)
  ;; Example: Convert lines of numbers to a list of numbers
  (map string->number raw-input))

;; --- Part 1 ---
(define (find-circuit-with circuits point)
  (if (empty? circuits)
    (set point)
    (if (set-member? (car circuits) point)
      (car circuits)
      (find-circuit-with (cdr circuits) point))))

(define (connect-circuits distances circuits to-go)
  (if (= 0 to-go)
    (sort (map set-count circuits) >)
    (let* ([first-circuit (find-circuit-with circuits (cadar distances))]
           [second-circuit (find-circuit-with circuits (caddar distances))]
           [combined-circuit (set-union first-circuit second-circuit)])
      (connect-circuits (cdr distances) (cons combined-circuit (remove first-circuit (remove second-circuit circuits))) (- to-go 1)))))

(define (find-distances-from point other-points distances-list)
  (if (empty? other-points)
    distances-list
    (let* ([next-point (car other-points)]
           [distance (+ (expt (- (car point) (car next-point)) 2)
                        (expt (- (cadr point) (cadr next-point)) 2)
                        (expt (- (caddr point) (caddr next-point)) 2))])
      (find-distances-from point (cdr other-points) (cons (list distance point next-point) distances-list)))))

(define (find-all-distances input distances-list)
  (if (empty? input)
    (sort distances-list (lambda (x y) (< (car x) (car y))))
    (let ([new-distance-list (find-distances-from (car input) (cdr input) distances-list)])
      (find-all-distances (cdr input) new-distance-list))))

(define (solve-part1 input)
  ;; Implement logic for Part 1
  (displayln "Solving Part 1...")
  (let* ([all-distances 
           (find-all-distances
             (map (lambda (x) (map string->number (string-split x ","))) input)
             '())]
         [circuit-sizes (connect-circuits all-distances '() 1000)])
    (* (car circuit-sizes) (cadr circuit-sizes) (caddr circuit-sizes))))

;; --- Part 2 ---
(define (find-last-connection distances circuits size)
  (let* ([first-point (cadar distances)]
         [second-point (caddar distances)]
         [first-circuit (find-circuit-with circuits first-point)]
         [second-circuit (find-circuit-with circuits second-point)]
         [combined-circuit (set-union first-circuit second-circuit)])
    (if (= size (set-count combined-circuit))
      (* (car first-point) (car second-point))
      (find-last-connection (cdr distances) (cons combined-circuit (remove first-circuit (remove second-circuit circuits))) size))))

(define (solve-part2 input)
  ;; Implement logic for Part 2
  (displayln "Solving Part 2...")
  (let* ([all-distances 
           (find-all-distances
             (map (lambda (x) (map string->number (string-split x ","))) input)
             '())])
    (find-last-connection all-distances '() (length input))))

;; --- Main Execution ---
(define input-filename "input")

(define string-input (read-input input-filename))
(define number-input (parse-input string-input))

(displayln (format "Part 1 Solution: ~a" (solve-part1 string-input)))
(displayln (format "Part 2 Solution: ~a" (solve-part2 string-input)))
