#lang racket


(define (member w ls)
  (cond [(null? ls)     #f]
        [(= w (car ls)) #t]
        [#t             (member w (cdr ls))]))

(define (elem n xs)
  (cond [(null? xs) (error 'elem "Element not found")]
        [(<= n 0)   (car xs)]
        [#t         (elem (- n 1) (cdr xs))]))

(define (lookup-index e xs)
  (letrec ([go (lambda (n xs)
                 (cond [(null? xs)    (error 'lookup-index "Element not found on the alphabet")]
                       [(= (car xs) e) n]
                       [#t             (go (+ n 1) (cdr xs))]))])
    (go 0 xs)))
                       

(define (trans-gen alphabet matrix)
  (lambda (s x)
    (elem (lookup-index x alphabet) (elem s matrix))))

(define (automata alphabet init-state finals matrix)
  (lambda (str)
    (letrec ([loop (lambda (s xs)
                      (cond [(null? xs) (member s finals)]
                            [#t         (loop (trans s (car xs)) (cdr xs))]))]
             [trans (trans-gen alphabet matrix)])
      (loop init-state str))))
            


(define even-zeroes
  (automata '(0 1) 0 '(0)
                   '((1 0)
                     (0 1))))