#lang racket

(define instruction-sequence
  (file->lines "../inputs/input_day1"))


;; Part 1

(define default-stack-pt1 (list 50))

; Takes string of instruction, returns if left-instruction
(define left-instruction?
  (lambda (inst)
    (equal?
     (car (string->list inst))
     #\L)))

; Takes string of instruction, returns if right-instruction
(define right-instruction?
  (lambda (inst)
    (equal?
     (car (string->list inst))
     #\R)))

; Takes string of instruction, returns corresponding integer where counterclockwise motion is negative
; L20 -> -20
; R2 -> 2
(define instruction->number
  (lambda (inst)
    (cond
      ((left-instruction? inst) (* -1 (string->number (substring inst 1))))
      ((right-instruction? inst) (string->number (substring inst 1)))
      (else (error "instruction->integer: Non-instruction passed as inst" inst)))))

(define turn-safe
  (lambda (pos inst)
    (cond
      ((not (number? pos))         (error "turn-safe: non-number passed as pos"))
      (else                        (modulo (+ pos (instruction->number inst)) 100)))))

; Given stack of positions and instruction, adds new position of safe to the stack
(define turn-safe-stack
  (lambda (stack inst)
    (cond
      ((not (list? stack))         (error "turn-safe-stack: stack is not a list" stack))
      ((not (number? (car stack))) (error "turn-safe-stack: non-number at top of stack" (car stack)))
      (else                        (cons (turn-safe (car stack) inst) stack)))))


; Calculates reversed sequence of positions obtained by performing a sequence of instructions
(define calculate-positions
  (case-lambda
    ((seq) (calculate-positions seq (lambda (inst) (turn-safe-stack default-stack-pt1 inst))))
    ((seq return)
     (if (null? (cdr seq))
         (return (car seq))
         (calculate-positions (cdr seq)
                              (lambda (inst) (turn-safe-stack (return (car seq)) inst)))))))

; Takes sequence of instructions and calculates passcode
(define calculate-passcode-pt1
  (lambda (seq)
    (length (filter (lambda (v) (equal? 0 v)) (calculate-positions seq)))))

(calculate-passcode-pt1 instruction-sequence)


;; Part 2
;; Instead of stack of positions use stack of pos-pairs
;; pos-pair is list of form '(position zero-clicks-so-far)

; Absolute delta turn from zero
; Adds compensatory vector from zero in the direction of rotation to the rotation vector and returns the magnitude
(define default-stack-pt2 (list (list 50 0)))

(define abs-dtfz
  (lambda (pos-pair inst)
    (cond
      ((left-instruction? inst)  (+ (modulo (- 100 (car pos-pair)) 100)
                                    (abs (instruction->number inst))))
      ((right-instruction? inst) (+ (car pos-pair)
                                    (instruction->number inst)))
      (else (error "dtfz: Non-instruction passed as inst" inst)))))

(define zero-clicks-from-turn
  (lambda (pos-pair inst)
    (quotient (abs-dtfz pos-pair inst) 100)))

(define turn-safe-stack-pt2
  (lambda (stack inst)
    (cond
      ((not (list? stack)) (error "turn-safe-stack-pt2: stack is not a list" stack))
      (else                (cons (list (turn-safe (caar stack) inst)
                                       (+ (cadar stack)
                                          (zero-clicks-from-turn (car stack) inst)))
                                 stack)))))

(define process-turns
  (case-lambda
    ((seq) (process-turns seq (lambda (inst) (turn-safe-stack-pt2 default-stack-pt2 inst))))
    ((seq return)
     (if (null? (cdr seq))
         (return (car seq))
         (process-turns (cdr seq) (lambda (inst) (turn-safe-stack-pt2 (return (car seq)) inst)))))))

(define calculate-passcode-pt2
  (lambda (seq)
    (cadar (process-turns seq))))

(calculate-passcode-pt2 instruction-sequence)






