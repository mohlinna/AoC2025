#lang racket
(require math/base)

;; --- Input Reading and Parsing ---
(define (read-input filename)
  (file->lines filename))

(define (parse-input raw-input)
  ;; Example: Convert lines of numbers to a list of numbers
  (map string->number raw-input))

;; --- Part 1 ---
(define (spin-dial current-pos spin)
  (let* ([direction (substring spin 0 1)]
         [operation (if (equal? "L" direction) - +)]
         [magnitude (string->number (substring spin 1))])
    (modulo (operation current-pos magnitude) 100)))

(define (solve-part1-loop input position zero-count)
  (if (empty? input)
    zero-count
    (let ([new-position (spin-dial position (car input))])
      (solve-part1-loop (cdr input) new-position (if (equal? 0 new-position) (+ 1 zero-count) zero-count)))))

(define (solve-part1 parsed-input)
  ;; Implement logic for Part 1
  (displayln "Solving Part 1...")
  (solve-part1-loop parsed-input 50 0))

;; --- Part 2 ---
(define (spin-dial-left-loop position count)
  ;;(displayln (format "      spin-dial-left-loop: position: ~a, count: ~a"
  ;;                   position count))
  (if (> position 0)
    (list position count)
    (if (equal? position 0)
      (list position (+ 1 count))
      (spin-dial-left-loop (+ position 100) (+ 1 count)))))

(define (spin-dial-left position magnitude)
  (if (equal? position 0)
    (spin-dial-left-loop (- position magnitude) -1)
    (spin-dial-left-loop (- position magnitude) 0)))
  
(define (spin-dial-right-loop position count)
  ;;(displayln (format "      spin-dial-right-loop: position: ~a, count: ~a"
  ;;                   position count))
  (if (< position 100)
    (list position count)
    (spin-dial-right-loop (- position 100) (+ 1 count))))

(define (spin-dial-right position magnitude)
  (spin-dial-right-loop (+ position magnitude) 0))

(define (spin-dial2 current-pos spin)
  ;;(displayln (format "    spin-dial2: current-pos: ~a, spin: ~a"
  ;;                   current-pos spin))
  (let* ([direction (substring spin 0 1)]
         [operation (if (equal? "L" direction) spin-dial-left spin-dial-right)]
         [magnitude (string->number (substring spin 1))])
    (operation current-pos magnitude)))

(define (solve-part2-loop input position zero-count)
  (if (empty? input)
    zero-count
    (let* ([results (spin-dial2 position (car input))]
           [new-position (car results)]
           [zeros-passed (cadr results)])
      (solve-part2-loop (cdr input) new-position (+ zeros-passed zero-count)))))
      
(define (solve-part2 parsed-input)
  ;; Implement logic for Part 2
  (displayln "Solving Part 2...")
  (solve-part2-loop parsed-input 50 0))

;; --- Main Execution ---
(define input-filename "input")

(define raw-data (read-input input-filename))
(define parsed-data (parse-input raw-data))

(displayln (format "Part 1 Solution: ~a" (solve-part1 raw-data)))
(displayln (format "Part 2 Solution: ~a" (solve-part2 raw-data)))
