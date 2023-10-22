#lang racket
(require pict3d)
(define-struct world (aloo alopl rf bf))
(define-struct object (pos1 pos2 texture))
(define-struct flag (pos carried?))
(define-struct player (at-pos v-pos la-pos team))

(define tl_firingrange
(combine (with-color(rgba "beige" 1)(rectangle origin (dir 1 1 0.01)))
         (with-color(rgba "orange" 1)(rectangle (pos 1 0 0.5)(dir 0.01 1 0.5)))
         (with-color(rgba "orange" 1)(rectangle (pos -1 0 0.5)(dir 0.01 1 0.5)))
         (with-color(rgba "orange" 1)(rectangle (pos 0 1 0.5)(dir 1 0.0 0.5)))
         (with-color(rgba "orange" 1)(rectangle (pos 0 -1 0.5)(dir 1 0.0 0.5)))
         (with-color(rgba "white" 1)(rectangle (pos 0 -0.5 0)(dir -0.5 0.01 0.11)))
         (with-color(rgba "white" 1)(rectangle (pos  0 0.5 0.5)(dir 0.5 0.01 0.5)))
         (basis 'camera (point-at (pos 0 0 0.1) (pos 0 1 0)))))

(define (redp pos1)
  (combine (with-color (rgba "beige" 1)(sphere pos1 0.2))
           (with-color (rgba "red" 1)(rectangle (pos (pos-x pos1)(pos-y pos1)(-(pos-z pos1) 0.21))(dir 0.1 0.1 0.3)))))
(define (bluep pos1)
  (combine (with-color (rgba "beige" 1)(sphere pos1 0.2))
           (with-color (rgba "blue" 1)(rectangle (pos (pos-x pos1)(pos-y pos1)(-(pos-z pos1) 0.21))(dir 0.1 0.1 0.3)))))


;;draw-people: alopl -> 3dscene
(define (draw-people alopl)
  (cond
    [(empty? alopl)(rectangle origin (dir 0 0 0))]
    [(symbol=? (player-team(first alopl))'red)(combine(redp (player-at-pos(first alopl)))(draw-people(rest alopl)))]
    [(symbol=? (player-team(first alopl))'blue)(combine(bluep (player-at-pos(first alopl)))(draw-people(rest alopl)))]))
      
(combine
 (draw-people (list (make-player (pos 1 1 1)(pos 1 1 1)(pos 1 1 1) 'blue)
                   (make-player (pos 1/2 1/2 1)(pos 1 1 1)(pos 1 1 1) 'red)
                   (make-player (pos 1/4 1 1/2)(pos 1 1 1)(pos 1 1 1) 'red)))
 tl_firingrange)
    
