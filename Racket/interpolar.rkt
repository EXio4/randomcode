#lang racket

(struct point (x y) #:constructor-name p #:transparent)

(define l (list
           (p -5 0.03125)
           (p -4 0.0625)
           (p -3 0.125)
           (p -2 0.25)
           (p -1 0.5) 
           (p  0 1)
           (p  1 2)
           (p  2 4)
           (p  3 8)
           (p  4 16)
           (p  5 32)
           ))

; asume un list de n puntos
(define (divdif pts n)
  (if (= n 0) (point-y (first pts))
      (/ (-
          (divdif (drop pts 1) (- n 1))
          (divdif (take pts (sub1 (length pts))) (- n 1)))
         (-
          (point-x (last pts))
          (point-x (first pts)))
         )
      ))

(define (df pts n)
  (divdif (take pts (add1 n)) n))

(define (ddiv pts)
  (divdif pts (sub1 (length pts))))

;; obtener y resolver a determinado punto

(define (newh pts x)
  (if (null? (cdr pts)) 1
  (*
   (- x (point-x (car pts)))
   (newh (cdr pts) x))
  ))

(define (sum proc n)
  (if (= n 0) 0
      (+ (proc n) (sum proc (- n 1)))))

(define (newton points x)
  (let
      ([newton_helper (lambda (z)
             (let ([lst (take points z)])
               (* (ddiv lst) (newh lst x))))])
    (sum newton_helper (length points))))
