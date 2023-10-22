;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Fractals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

;;midpoint: pons pons ->; pons
(define (midpoint p1 p2)
  (make-posn (/ (+ (posn-x p1)(posn-x p2))2)(/ (+ (posn-y p1)(posn-y p2)) 2)))
(midpoint (make-posn -1 2)(make-posn 3 -6))

;;distance: pons pons ->; pons
(define (distance p1 p2)
  (sqrt(+ (sqr(- (posn-x p2)(posn-x p1)))(sqr(-(posn-y p2)(posn-y p1))))))
(distance (make-posn -2 -3)(make-posn -4 4))

;;too-small: pons pons pons ->; bool
(define (too-small? a b c)
  (cond
[(< (distance a b) 6)true]
[(< (distance a c) 6)true]
[(< (distance b c) 6)true]
[else false]))

;;draw-triangle: a b c ->;
(define (draw-triangle a b c)
  (add-line (add-line (add-line empty-image (posn-x c)(posn-y c)(posn-x b)(posn-y
b) 'black)(posn-x b)(posn-y b)(posn-x a)(posn-y a) 'black)(posn-x a)(posn-y
a)(posn-x c)(posn-y c)'black))
(draw-triangle (make-posn 100 200)(make-posn 200 200)(make-posn 150 150))
;;Sierpinski: a b c ->; bool
(define (sierpinski a b c)
(cond
[(too-small? a b c) empty-image]
[else (local ((define a-b (midpoint a b))
(define b-c (midpoint b c))
(define c-a (midpoint a c)))
(overlay/align "left" "top"
(draw-triangle a b c)
(sierpinski a a-b c-a)
(sierpinski b a-b b-c)
(sierpinski c c-a b-c)))]))

(sierpinski (make-posn 100 200)(make-posn 200 200)(make-posn 150 100))