#lang racket
;matrix-multiply: matrix vector -> vector
(define (matrix-multiply m v)
  (transpose (map (lambda (column) (map (lambda (row) (dot row column)) m)) (transpose v))))



(define-struct world (aloo alopl yplayer rf bf shots portal))
(define-struct object (pos1 pos2 texture))
(define-struct flag (pos state color))
(define-struct player (team at-pos v-pos la-pos gun))
(define-struct shot (team at-pos la-dir))
(define-struct portal (team at-pos to-portal))
(define (shotimage shot1) (with-color (rgba (shot-team shot1) 0.5) (rectangle (shot-at-pos shot1) (shot-la-dir shot1))))
(define (portalimage portal1) (with-color (rgba (portal-team portal1) 0.7) (cylinder (portal-at-pos portal1) 1)))

;shoot: world -> world     
(define (shoot world)
 (cond
    [(and (eq? (player-gun (world-yplayer world)) true) (mouse-event? "button-down"))
     (cond
       [(eq? "true" (first (check-color (player-team (world-yplayer world)) (world-portals world)))) (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (world-rf world) (world-bf world) (world-shots world) (cons (make-portal (player-team (world-yplayer world)) (player-la-dir (world-yplayer world)) (second (check-color (player-team (world-yplayer world)) (world-portals world)))) (world-portals world)))]
       [else (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (world-rf world) (world-bf world) (world-shots world) (cons (make-portal (player-team (world-yplayer world)) (player-la-dir (world-yplayer world)) empty) (world-portals world)))])]                                                                                      
    [(and (eq? (player-gun (world-yplayer world)) false) (mouse-event? "button-down")) (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (world-rf world) (world-bf world) (cons (make-shot (player-team (world-yplayer world)) (player-at-pos (world-yplayer world)) (player-la-dir (world-yplayer world))) (world-shots world)) (world-portals world))]
    [else world]))


;shoot: s world -> world     
(define (shoot s world)
 (cond
    [(and (eq? (player-gun (world-yplayer world)) true) (string=? s "button-down"))
     (cond
       [(eq? "true" (first (check-color (player-team (world-yplayer world)) (world-portal world)))) (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (world-rf world) (world-bf world) (world-shots world) (cons (make-portal (player-team (world-yplayer world)) (player-la-pos (world-yplayer world)) (second (check-color (player-team (world-yplayer world)) (world-portal world)))) (world-portal world)))]
       [else (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (world-rf world) (world-bf world) (world-shots world) (cons (make-portal (player-team (world-yplayer world)) (player-la-pos (world-yplayer world)) empty) (world-portal world)))])]                                                                                      
    [(and (eq? (player-gun (world-yplayer world)) false) (string=? s "button-down")) (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (world-rf world) (world-bf world) (cons (make-shot (player-team (world-yplayer world)) (player-at-pos (world-yplayer world)) (player-la-pos (world-yplayer world))) (world-shots world)) (world-portal world))]
    [else world]))

Shoot references check color-->
;check-color: color list -> symbol pos
(define (check-color color list)
  (cond
    [(empty? list) false]
    [(eq? (portal-team (first list)) color) (cons "true" (portal-at-pos (first list)))]
    [else (check-color (rest list))]))


;draw-shots: world -> world
(define (draw-shots world)
  (cond
    [(empty? (world-shots world)) (rectangle origin (dir 0 0 0))]
    [else (combine (shotimage (first (world-shots world)))
                   (draw-shots (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (world-rf world) (world-bf world) (rest (world-shots world)) (world-portals world))))]))



;draw-flags: flag flag -> pict3d
(define (draw-flags redflag blueflag)
   (combine (with-color (rgba (flag-color redflag) 1) (cube (flag-pos redflag) .3))
                   (with-color (rgba (flag-color blueflag) 1) (cube (flag-pos blueflag) .3))))

;follow-player: player flag -> flag
(define (follow-player player flag)
   (cond
	[(false? (flag-state flag)) flag]
	[else (make-flag (player-at-pos player) true (flag-color flag))]))

(define (redp pos1)
  (combine (with-color (rgba "beige" 1)(sphere pos1 0.2))
           (with-color (rgba "red" 1)(rectangle (pos (pos-x pos1)(pos-y pos1)(-(pos-z pos1) 0.21))(dir 0.1 0.1 0.3)))))
(define (bluep pos1)
  (combine (with-color (rgba "beige" 1)(sphere pos1 0.2))
           (with-color (rgba "blue" 1)(rectangle (pos (pos-x pos1)(pos-y pos1)(-(pos-z pos1) 0.21))(dir 0.1 0.1 0.3)))))

;draw-shots: world -> world
(define (draw-shots world)
  (cond
    [(empty? (world-shots world)) (rectangle origin (dir 0 0 0))]
    [else (combine (shotimage (first (world-shots world)))
                   (draw-shots (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (world-rf world) (world-bf world) (rest (world-shots world)) (world-portals world))))]))

;draw-portals: world-> world
(define (draw-portals world)
  (cond
    [(empty? (world-portals world)) (rectangle origin (dir 0 0 0))]
    [else (combine (portalimage (first (world-portals world)))
                   (draw-portals (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (world-rf world) (world-bf world) (world-shots world) (rest (world-portals world)))))]))


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

;in-object?: player object -> b
(define (in-object? a-player a-object)
  (and (and (and (> (pos-x (player-at-pos a-player)) (pos-x (object-pos1 a-object)))
                 (< (pos-x (player-at-pos a-player)) (pos-x (object-pos2 a-object))))
            (and (> (pos-y (player-at-pos a-player)) (pos-y (object-pos1 a-object)))
                 (< (pos-y (player-at-pos a-player)) (pos-y (object-pos2 a-object)))))
       (and (> (pos-z (player-at-pos a-player)) (pos-z (object-pos1 a-object)))
            (< (pos-z (player-at-pos a-player)) (pos-z (object-pos2 a-object))))))

;hit-object?: player object -> b
(define (hit-object? a-player a-object)
  (ormap in-object? (list (make-player (player-team a-player)
                                       (make-pos (+ (pos-x (player-at-pos a-player)) 0.5)
                                                 (pos-y (player-at-pos a-player))
                                                 (pos-z (player-at-pos a-player)))
                                       (player-v-pos a-player)
                                       (player-la-pos a-player))
                          (make-player (player-team a-player)
                                       (make-pos (- (pos-x (player-at-pos a-player)) 0.5)
                                                 (pos-y (player-at-pos a-player))
                                                 (pos-z (player-at-pos a-player)))
                                       (player-v-pos a-player)
                                       (player-la-pos a-player))
                          (make-player (player-team a-player)
                                       (make-pos (pos-x (player-at-pos a-player))
                                                 (+ (pos-y (player-at-pos a-player)) 0.5)
                                                 (pos-z (player-at-pos a-player)))
                                       (player-v-pos a-player)
                                       (player-la-pos a-player))
                          (make-player (player-team a-player)
                                       (make-pos (pos-x (player-at-pos a-player))
                                                 (- (pos-y (player-at-pos a-player)) 0.5)
                                                 (pos-z (player-at-pos a-player)))
                                       (player-v-pos a-player)
                                       (player-la-pos a-player))
                          (make-player (player-team a-player)
                                       (make-pos (pos-x (player-at-pos a-player))
                                                 (pos-y (player-at-pos a-player))
                                                 (+ (pos-z (player-at-pos a-player)) 1))
                                       (player-v-pos a-player)
                                       (player-la-pos a-player))
                          (make-player (player-team a-player)
                                       (make-pos (pos-x (player-at-pos a-player))
                                                 (pos-y (player-at-pos a-player))
                                                 (- (pos-z (player-at-pos a-player)) 1))
                                       (player-v-pos a-player)
                                       (player-la-pos a-player)))))

;hit-objects?: player aloo -> b
(define (hit-objects? a-player aloo)
  (ormap (lambda(x)
           (hit-object? a-player x)) aloo))

;auto-player-movement: player aloo -> player
(define (auto-player-movement a-player aloo)
  (make-player (player-team a-player)
               (make-pos (+ (pos-x (player-v-pos a-player)) (pos-x (player-at-pos a-player)))
                         (+ (pos-y (player-v-pos a-player)) (pos-y (player-at-pos a-player)))
                         (+ (pos-z (player-v-pos a-player)) (pos-z (player-at-pos a-player))))
               (cond
                 [(hit-objects? a-player aloo) (make-pos 0 0 0)]
                 [else (make-pos (* momentum (pos-x (player-v-pos a-player)))
                                 (* momentum (pos-y (player-v-pos a-player)))
                                 (* momentum (- (pos-z (player-v-pos a-player)) 0.1)))])
               (player-la-pos a-player)))

;auto-players-movement: world -> world
(define (auto-players-movement a-world)
  (make-world (world-aloo a-world)
              (world-alopl a-world)
              (auto-player-movement (world-yplayer a-world)
                                    (world-aloo a-world))
              (world-rf a-world)
              (world-bf a-world)))


;directional-shift: player s -> pos
(define (directional-shift-up a-player s)
  (make-pos (* (cond
                 [(or (string=? "Left" s)
                      (string=? "Down" s)) -1]
                 [else 1]) (- (pos-x (player-la-pos a-player)) (pos-x (player-at-pos a-player))))
            (* (cond
                 [(or (string=? "Right" s)
                      (string=? "Down" s)) -1]
                 [else 1]) (- (pos-y (player-la-pos a-player)) (pos-y (player-at-pos a-player))))
            0))

;on-key-movement: world s -> world
(define (on-key-movement a-world s)
  (cond
    [(or (or (string=? "Up" s)
             (string=? "Down" s))
         (or (string=? "Left" s)
             (string=? "Right" s))) (make-world (world-alopl a-world)
                                                (world-aloo a-world)
                                                (make-player (player-team (world-yplayer a-world))
                                                             (player-at-pos (world-yplayer a-world))
                                                             (directional-shift (world-yplayer a-world) s)
                                                             (player-la-pos (world-yplayer a-world)))
                                                (world-rf a-world)
                                                (world-bf a-world))]
    [(string=? " " s) (make-world (world-alopl a-world)
                                  (world-aloo a-world)
                                  (make-player (player-team (world-yplayer a-world))
                                               (player-at-pos (world-yplayer a-world))
                                               (make-pos (pos-x (player-v-pos (world-yplayer a-world)))
                                                         (pos-y (player-v-pos (world-yplayer a-world)))
                                                         (cond
                                                           [(= 0 (pos-z (player-z-pos (world-yplayer a-world)))) 1]
                                                           [else (pos-z (player-z-pos (world-yplayer a-world)))]))
                                               (player-la-pos (world-yplayer a-world)))
                                  (world-rf a-world)
                                  (world-bf a-world))]
    [else a-world]))


;pickup: world -> world
(define (p world)
            (cond  
             [(empty?(player-at-pos (first (world-alopl world))))  world]
             [(< (pos-dist (player-at-pos  (first (world-alopl world))) (flag-pos (world-bf world))) .1)
                               (cond
                                        [(symbol=? (player-team (first (world-alopl world))) (flag-color (world-bf world))) (p (make-world (world-aloo world) (rest (world-alopl world)) (world-yplayer world) (world-rf world) (world-bf world) (world-shots world) (world-portal world)))]
                                        [else (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (world-rf world) (make-flag (flag-pos (world-bf world)) true "blue") (world-shots world) (world-portal world))])]
              [(< (pos-dist (player-at-pos  (first (world-alopl world)))(flag-pos (world-rf world))) .1)
                               (cond
                                        [(string=? (player-team (first (world-alopl world))) (flag-color (world-rf world))) (p (make-world (world-aloo world) (rest (world-alopl world)) (world-yplayer world) (world-rf world) (world-bf world)(world-shots world) (world-portal world)))]
                                        [else (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (make-flag (flag-pos (world-rf world)) true "red") (world-bf world) (world-shots world)(world-portal world))])]
              [(< (pos-dist (player-at-pos (world-yplayer world)) (flag-pos (world-bf world))) .1)
               (cond
                 [(symbol=? (player-team (world-yplayer world)) (flag-color (world-bf world))) world]
                 [else (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (world-rf world) (make-flag (flag-pos (world-bf world)) true "blue")(world-shots world) (world-portal world))])]
              [(< (pos-dist (player-at-pos (world-yplayer world))(flag-pos (world-rf world))) .1)
               (cond
                 [(symbol=? (player-team (world-yplayer world)) (flag-color (world-rf world))) world]
                 [else (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (make-flag (flag-pos (world-rf world)) true "red") (world-bf world) (world-shots world) (world-portal world))])]
              [else (p (make-world (world-aloo world) (world-alopl world) (world-yplayer world) (world-rf world) (world-bf world) (world-shots world) (world-portal world)))]))



