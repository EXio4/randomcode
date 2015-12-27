#lang racket

(require pict)
(require racket/stream)
(require racket/dict)

(define char '((#\e . ((#t #t #t)
                       (#t #f #f)
                       (#t #t #t)
                       (#t #f #f)
                       (#t #t #t)))
               
               (#\x . ((#f #f #f)
                       (#t #f #t)
                       (#t #t #f)
                       (#f #t #t)
                       (#t #f #t)))
               
               (#\i . ((#f #f #f)
                       (#t #t #t)
                       (#f #t #f)
                       (#f #t #f)
                       (#t #t #t)))
               
               (#\o . ((#f #f #f)
                       (#t #t #t)
                       (#t #f #t)
                       (#t #f #t)
                       (#t #t #t)))
               (#\4 . ((#t #f #t)
                       (#t #f #t)
                       (#t #t #t)
                       (#f #f #t)
                       (#f #f #t)
                       ))))
                  
(define (get-char x)
  (dict-ref char x))

(define (draw x c)
  (define bk (blank (pict-width x)))
  (define (go-line xs)
    (cond [(null? xs) bk]
          [(car xs)   (hb-append x  (go-line (cdr xs)))]
          [else       (hb-append bk (go-line (cdr xs)))]))
  (define (go xs)
    (cond [(null? xs) bk]
          [else       (vl-append (go-line (car xs)) (go (cdr xs)))]))
  (go (get-char c)))
  


(define (str-i s) (lambda (rect)
                (foldr (lambda (curr next)
                          (hb-append (draw rect (char-downcase curr)) next)) (blank (pict-width rect)) (string->list s))))


(define (fractal i b func)
  (sequence-fold (lambda (v n)
                   (func (bitmap (pict->bitmap v)))) b (in-range 0 i)))


(define (rend file x) (begin (send (pict->bitmap x) save-file file 'png)
                              x))

(define s (str-i "exio4"))
(rend "fractal-exio4.png" (fractal 2 (filled-rectangle 17 15) s))
(rend "exio4-rect.png"    (s (filled-rectangle 16 15)))
(rend "exio4-rrect.png"   (s (filled-rectangle 48 47)))
(rend "exio4-circle.png"  (s (pin-over (colorize (disk 32) "red") 2 2 (disk 28))))
(rend "exio4-rects.png"   (s (rectangle 32 32)))
