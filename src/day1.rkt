#lang racket

(define instruction-sequence
  (file->lines "../inputs/input_day1"))

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

; Given stack of positions and instruction, adds new position of safe to the stack
(define turn-safe
  (lambda (stack inst)
    (cond
      ((not (list? stack))         (error "turn-safe: stack is not a list" stack))
      ((not (number? (car stack))) (error "turn-safe: non-number at top of stack" (car stack)))
      (else                        (cons (modulo (+ (car stack) (instruction->number inst)) 100) stack)))))


; Calculates reversed sequence of positions obtained by performing a sequence of instructions
(define calculate-positions
  (case-lambda
    ((seq) (calculate-positions seq (lambda (inst) (turn-safe (list 50) inst))))
    ((seq return)
     (if (null? (cdr seq))
         (return (car seq))
         (calculate-positions (cdr seq)
                              (lambda (inst) (turn-safe (return (car seq)) inst)))))))

; Takes sequence of instructions and calculates passcode
(define calculate-passcode
  (lambda (seq)
    (length (filter (lambda (v) (equal? 0 v)) (calculate-positions seq)))))

(calculate-passcode instruction-sequence)
