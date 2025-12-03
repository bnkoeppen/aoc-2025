#lang racket

;; INPUT PROCESSING
;; ----------------------

(define input_string
  (string-trim (file->string "../inputs/input_day2")))

(define input-id-ranges
   (map (lambda (id_range)
          (map string->number (string-split id_range "-")))
        (string-split input_string ",")))

;; ----------------------

(define range-begin car)
(define range-end cadr)

; calculates digits in an integer
(define digits
  (lambda (n)
    (string-length (number->string n))))

; The minimum integer with d digits
(define min-digits
  (lambda (d)
    (expt 10 (- d 1))))

; The maximum integer with d digits
(define max-digits
  (lambda (d)
    (- (expt 10 d) 1)))

; generates silly number from pattern
; 10 -> 1010
(define generate-silly
  (lambda (n)
    (+ n (* n (expt 10 (digits n))))))

; splits integers into two parts, big endian
(define split-integer
  (lambda (n)
    (let ((digits-n (digits n)))
      (if (odd? digits-n)
          (error "split-integer: n has odd number of digits " n)
          (list (modulo n (expt 10 (/ digits-n 2)))
                (quotient n (expt 10 (/ digits-n 2))))))))

(define silly?
  (lambda (n)
    (if (odd? (digits n))
        #f
        (let ((split-n (split-integer n)))
          (equal? (car split-n) (cadr split-n))))))

; from an integer n, calculate the next silly number
; cond in an if because the let eager evaluates and would
; rather not split n a bunch
(define next-silly
  (lambda (n)
    (if (odd? (digits n))
        (next-silly (min-digits (+ 1 (digits n))))
        (let ((split-n (split-integer n)))
          (cond
            ((< (car split-n)
                (cadr split-n))   (generate-silly (cadr split-n)))
            ((>= (car split-n)
                 (cadr split-n))  (generate-silly (+ 1 (cadr split-n))))))))
)
    
(define sillies-in-range-pair
  (case-lambda
    ((range-pair) (sillies-in-range-pair range-pair (range-begin range-pair) (lambda (v) (list v))))
    ((range-pair curr return)
     (cond
       ((not (silly? curr))             (sillies-in-range-pair range-pair (next-silly curr) return))
       ((> curr (range-end range-pair)) (cdr (return 'dummy))) ; Add dummy final item and discard
       (else                            (sillies-in-range-pair range-pair (next-silly curr) (lambda (v) (cons v (return curr)))))))))

(define sillies-in-ranges
  (lambda (range-pairs)
    (map sillies-in-range-pair range-pairs)))

(define sum-of-sillies-in-ranges
  (lambda (range-pairs)
    (apply + (flatten (sillies-in-ranges range-pairs)))))

(sum-of-sillies-in-ranges input-id-ranges)
