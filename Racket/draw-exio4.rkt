#lang racket

(require pict)
(require racket/stream)
(require racket/dict)

(define char '((#\e . ((1.0 1.0 1.0)
                       (1.0 0.0 0.0)
                       (1.0 1.0 1.0)
                       (1.0 0.0 0.0)
                       (1.0 1.0 1.0)))
               
               (#\x . ((0.0 0.0 0.0)
                       (1.0 0.0 1.0)
                       (1.0 1.0 0.0)
                       (0.0 1.0 1.0)
                       (1.0 0.0 1.0)))
               
               (#\i . ((0.0 0.0 0.0)
                       (1.0 1.0 1.0)
                       (0.0 1.0 0.0)
                       (0.0 1.0 0.0)
                       (1.0 1.0 1.0)))
               
               (#\o . ((0.0 0.0 0.0)
                       (1.0 1.0 1.0)
                       (1.0 0.0 1.0)
                       (1.0 0.0 1.0)
                       (1.0 1.0 1.0)))

               (#\4 . ((1.0 0.0 1.0)
                       (1.0 0.0 1.0)
                       (1.0 1.0 1.0)
                       (0.0 0.0 1.0)
                       (0.0 0.0 1.0)
                       ))))
                  
(define (get-char x)
  (dict-ref char x))

(define (fold+map c z xs f)
  (foldl (lambda (e es) (c es (f e))) z xs))

(define (draw x c)
  (define bk (blank (pict-width x)))
  (fold+map vl-append bk c
            (lambda (ys) (fold+map hb-append bk ys
                                   (lambda (e) (cellophane x e))))))
  

(define (str-i s) (lambda (rect)
                (foldr (lambda (curr next)
                          (hb-append (draw rect (get-char (char-downcase curr))) next)) (blank (pict-width rect)) (string->list s))))


(define (fractal i b func)
  (sequence-fold (lambda (v n)
                   (func (bitmap (pict->bitmap v)))) b (in-range 0 i)))


(define (rend s file x) (begin (send (pict->bitmap x) save-file file 'png)
                             (cond [(eq? s 'yes) x]
                                   [else         (blank)])))

(define ibr
       '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0)
         (0 0 1 0 0 0 0 1 0 0 0 1 0 1 1 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 1 0)
         (0 1 0 1 0 0 0 1 0 0 0 1 0 1 0 1 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 0)
         (1 1 1 1 1 1 1 0 1 1 1 1 1 0 0 1 0 0 1 0 0 0 0 0 1 1 1 1 0 0 1 1)
         (1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 0)
         (1 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 1 0 0 1 1)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        ))

(define s (str-i "exio4"))
(rend 'no  "fractal-exio4.png" (fractal 2 (filled-rectangle 17 15) s))
(rend 'yes "exio4-rect.png"    (s (filled-rectangle 16 15)))
(rend 'yes "exio4-rrect.png"   (s (filled-rectangle 48 47)))
(rend 'yes "exio4-circle.png"  (s (pin-over (colorize (disk 32) "red") 2 2 (disk 28))))
(rend 'yes "exio4-rects.png"   (s (rectangle 32 32)))
(rend 'yes "ibr.png"           (draw (filled-rectangle 9 9) ibr))