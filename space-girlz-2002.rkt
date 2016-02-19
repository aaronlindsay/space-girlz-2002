;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname space-girlz-2002) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Vidya Project

; A location is a (make-posn x y).
; interpretation: x and y are positive graphical coordinates that represent
; the location of a ball.

; A velocity is a (make-vel deltax deltay).
; interpretation: deltax and deltay are the horizontal and vertical
; components of the velocity vector.

(define-struct vel (deltax deltay))

; A ball is a (make-ball location velocity).
; interpretation:
; - location is a Posn
; - velocity is a vel

(define plank-length 2)
(define (plankx n)
   (* n plank-length))
(define galaxy (square (plankx 300) "solid" "black"))

(define-struct world (BALL1 BALL2 MINES))

(define-struct mines (M0 M1 M2 M3))
(define-struct ball (location velocity cc? scr? hd?))

; A mine is a (make-posn).
; interpretation:
; - x, y are the position of the mine in graphics coords

(define-struct mine (location velocity))

; Velocity functions for mines M0,M1,M2,M3
; Velocity is in the direction of the ball position from the mine position
; Velocity increases inversely with distance between ball and mine

(define (M0-s-1 w)
  (pythag (- (posn-x (ball-location (world-BALL1 w))) (posn-x (mine-location (mines-M0 (world-MINES w)))))
                              (- (posn-y (ball-location (world-BALL1 w))) (posn-y (mine-location (mines-M0 (world-MINES w)))))))
(define (M1-s-1 w)
  (pythag (- (posn-x (ball-location (world-BALL1 w))) (posn-x (mine-location (mines-M1 (world-MINES w)))))
                              (- (posn-y (ball-location (world-BALL1 w))) (posn-y (mine-location (mines-M1 (world-MINES w)))))))
(define (M2-s-1 w)
  (pythag (- (posn-x (ball-location (world-BALL1 w))) (posn-x (mine-location (mines-M2 (world-MINES w)))))
                              (- (posn-y (ball-location (world-BALL1 w))) (posn-y (mine-location (mines-M2 (world-MINES w)))))))
(define (M3-s-1 w)
  (pythag (- (posn-x (ball-location (world-BALL1 w))) (posn-x (mine-location (mines-M3 (world-MINES w)))))
                              (- (posn-y (ball-location (world-BALL1 w))) (posn-y (mine-location (mines-M3 (world-MINES w)))))))

(define (M0-s-2 w)
  (pythag (- (posn-x (ball-location (world-BALL2 w))) (posn-x (mine-location (mines-M0 (world-MINES w)))))
                              (- (posn-y (ball-location (world-BALL2 w))) (posn-y (mine-location (mines-M0 (world-MINES w)))))))
(define (M1-s-2 w)
  (pythag (- (posn-x (ball-location (world-BALL2 w))) (posn-x (mine-location (mines-M1 (world-MINES w)))))
                              (- (posn-y (ball-location (world-BALL2 w))) (posn-y (mine-location (mines-M1 (world-MINES w)))))))
(define (M2-s-2 w)
  (pythag (- (posn-x (ball-location (world-BALL2 w))) (posn-x (mine-location (mines-M2 (world-MINES w)))))
                              (- (posn-y (ball-location (world-BALL2 w))) (posn-y (mine-location (mines-M2 (world-MINES w)))))))
(define (M3-s-2 w)
  (pythag (- (posn-x (ball-location (world-BALL2 w))) (posn-x (mine-location (mines-M3 (world-MINES w)))))
                              (- (posn-y (ball-location (world-BALL2 w))) (posn-y (mine-location (mines-M3 (world-MINES w)))))))

(define (M0-s-ratio-1 w)
  (- 1 (sqr (/ (M0-s-1 w) (pythag (image-width galaxy) (image-height galaxy))))))
(define (M1-s-ratio-1 w)
  (- 1 (sqr (/ (M1-s-1 w) (pythag (image-width galaxy) (image-height galaxy))))))
(define (M2-s-ratio-1 w)
  (- 1 (sqr (/ (M2-s-1 w) (pythag (image-width galaxy) (image-height galaxy))))))
(define (M3-s-ratio-1 w)
  (- 1 (sqr (/ (M3-s-1 w) (pythag (image-width galaxy) (image-height galaxy))))))

(define (M0-s-ratio-2 w)
  (- 1 (sqr (/ (M0-s-2 w) (pythag (image-width galaxy) (image-height galaxy))))))
(define (M1-s-ratio-2 w)
  (- 1 (sqr (/ (M1-s-2 w) (pythag (image-width galaxy) (image-height galaxy))))))
(define (M2-s-ratio-2 w)
  (- 1 (sqr (/ (M2-s-2 w) (pythag (image-width galaxy) (image-height galaxy))))))
(define (M3-s-ratio-2 w)
  (- 1 (sqr (/ (M3-s-2 w) (pythag (image-width galaxy) (image-height galaxy))))))

(define (M0-v w)
  (cond [(< (M0-s-1 w) (M0-s-2 w))
         (make-vel (round (* (plankx 1) (M0-s-ratio-1 w)
                             (/ (- (posn-x (ball-location (world-BALL1 w))) (posn-x (mine-location (mines-M0 (world-MINES w)))))
                                (M0-s-1 w))))
                   (round (* (plankx 1) (M0-s-ratio-1 w)
                             (/ (- (posn-y (ball-location (world-BALL1 w))) (posn-y (mine-location (mines-M0 (world-MINES w)))))
                                (M0-s-1 w)))))]
        [else
         (make-vel (round (* (plankx 1) (M0-s-ratio-2 w)
                             (/ (- (posn-x (ball-location (world-BALL2 w))) (posn-x (mine-location (mines-M0 (world-MINES w)))))
                                (M0-s-2 w))))
                   (round (* (plankx 1) (M0-s-ratio-2 w)
                             (/ (- (posn-y (ball-location (world-BALL2 w))) (posn-y (mine-location (mines-M0 (world-MINES w)))))
                                (M0-s-2 w)))))]))

(define (M1-v w)
  (cond [(< (M1-s-1 w) (M1-s-2 w))
         (make-vel (round (* (plankx 1) (M1-s-ratio-1 w)
                             (/ (- (posn-x (ball-location (world-BALL1 w))) (posn-x (mine-location (mines-M1 (world-MINES w)))))
                                (M1-s-1 w))))
                   (round (* (plankx 1) (M1-s-ratio-1 w)
                             (/ (- (posn-y (ball-location (world-BALL1 w))) (posn-y (mine-location (mines-M1 (world-MINES w)))))
                                (M1-s-1 w)))))]
        [else
         (make-vel (round (* (plankx 1) (M1-s-ratio-2 w)
                             (/ (- (posn-x (ball-location (world-BALL2 w))) (posn-x (mine-location (mines-M1 (world-MINES w)))))
                                (M1-s-2 w))))
                   (round (* (plankx 1) (M1-s-ratio-2 w)
                             (/ (- (posn-y (ball-location (world-BALL2 w))) (posn-y (mine-location (mines-M1 (world-MINES w)))))
                                (M1-s-2 w)))))]))

(define (M2-v w)
  (cond [(< (M2-s-1 w) (M2-s-2 w))
         (make-vel (round (* (plankx 1) (M2-s-ratio-1 w)
                             (/ (- (posn-x (ball-location (world-BALL1 w))) (posn-x (mine-location (mines-M2 (world-MINES w)))))
                                (M2-s-1 w))))
                   (round (* (plankx 1) (M2-s-ratio-1 w)
                             (/ (- (posn-y (ball-location (world-BALL1 w))) (posn-y (mine-location (mines-M2 (world-MINES w)))))
                                (M2-s-1 w)))))]
        [else
         (make-vel (round (* (plankx 1) (M2-s-ratio-2 w)
                             (/ (- (posn-x (ball-location (world-BALL2 w))) (posn-x (mine-location (mines-M2 (world-MINES w)))))
                                (M2-s-2 w))))
                   (round (* (plankx 1) (M2-s-ratio-2 w)
                             (/ (- (posn-y (ball-location (world-BALL2 w))) (posn-y (mine-location (mines-M2 (world-MINES w)))))
                                (M2-s-2 w)))))]))

(define (M3-v w)
  (cond [(< (M3-s-1 w) (M3-s-2 w))
         (make-vel (round (* (plankx 1) (M3-s-ratio-1 w)
                             (/ (- (posn-x (ball-location (world-BALL1 w))) (posn-x (mine-location (mines-M3 (world-MINES w)))))
                                (M3-s-1 w))))
                   (round (* (plankx 1) (M3-s-ratio-1 w)
                             (/ (- (posn-y (ball-location (world-BALL1 w))) (posn-y (mine-location (mines-M3 (world-MINES w)))))
                                (M3-s-1 w)))))]
        [else
         (make-vel (round (* (plankx 1) (M3-s-ratio-2 w)
                             (/ (- (posn-x (ball-location (world-BALL2 w))) (posn-x (mine-location (mines-M3 (world-MINES w)))))
                                (M3-s-2 w))))
                   (round (* (plankx 1) (M3-s-ratio-2 w)
                             (/ (- (posn-y (ball-location (world-BALL2 w))) (posn-y (mine-location (mines-M3 (world-MINES w)))))
                                (M3-s-2 w)))))]))

; Mine velocity helper function
; Computes hypotenuse

(define (pythag x y)
  (sqrt (+ (sqr x) (sqr y))))

(define random-angle1 (random 360))
(define random-angle2 (random 360))

(define (mine-img w)
  (overlay (circle (* 5 plank-length) "outline" "dark gray")
           (rotate (+ random-angle1 (* 3 (posn-x (ball-location (world-BALL1 w))))) (ellipse (plankx 16) (plankx 1) "outline" "red"))
           (rotate (+ random-angle2 (* (- 3) (posn-y (ball-location (world-BALL1 w))))) (ellipse (plankx 16) (plankx 1) "outline" "red"))))

(define-struct hitbox (xmin xmax ymin ymax))

; A HB is two sets of two Numbers, the first two being the
; x-range of the hitbox and the second two being the y-range

#;(define (coin-hb c) (make-hitbox
                   (- (posn-x (coin-posn c)) (plankx 5))
                   (+ (posn-x (coin-posn c)) (plankx 5))
                   (- (posn-y (coin-posn c)) (plankx 5))
                   (+ (posn-y (coin-posn c)) (plankx 5))))

; dot-hb: Ball -> HB
; takes a Ball and makes a HB that extends 10 units
; left, right, up, and down

(define (ball1-hb w)
    (make-hitbox
    (- (posn-x (ball-location (world-BALL1 w))) (plankx 9))
    (+ (posn-x (ball-location (world-BALL1 w))) (plankx 9))
    (- (posn-y (ball-location (world-BALL1 w))) (plankx 9))
    (+ (posn-y (ball-location (world-BALL1 w))) (plankx 9))))

(define (ball2-hb w)
    (make-hitbox
    (- (posn-x (ball-location (world-BALL2 w))) (plankx 9))
    (+ (posn-x (ball-location (world-BALL2 w))) (plankx 9))
    (- (posn-y (ball-location (world-BALL2 w))) (plankx 9))
    (+ (posn-y (ball-location (world-BALL2 w))) (plankx 9))))

; A Value is a Number
; A coin is a (make-posn), Value, Boolean
; interpretation:
; - x, y are the position of the coin in graphics coords,
; Value is the score number

(define-struct coin (posn value num))

(define (speed1 w)
  (sqrt (+ (sqr (vel-deltax (ball-velocity (world-BALL1 w)))) (sqr (vel-deltay (ball-velocity (world-BALL1 w)))))))
(define (speed2 w)
  (sqrt (+ (sqr (vel-deltax (ball-velocity (world-BALL2 w)))) (sqr (vel-deltay (ball-velocity (world-BALL2 w)))))))

#;(define (world-next-template w)
    (...(posn-x (ball-location (world-BALL1 w)))
        (posn-y (ball-location (world-BALL1 w)))
        (vel-deltax (ball-velocity (world-BALL1 w)))
        (vel-deltay (ball-velocity (world-BALL1 w)))...))

(define (ball-color w) (list-ref
                       (list "red" "orange" "yellow" "green" "blue" "indigo" "violet")
                       (random 7)))

(define (ring-color w) (list-ref
                       (list "red" "orange" "yellow" "green" "blue" "indigo" "violet")
                       (random 7)))

; images adjusted from images of faces to generated circles for GitHub post
(define alissa-img (scale .75 (circle (plankx 10) "solid" "purple")))
(define gaby-img (scale .75 (circle (plankx 10) "solid" "green")))

(define (singleplayer-image w) (overlay (circle (plankx 10) "outline" (ball-color (world-BALL1 w))) (circle (plankx 10) "solid" "green")
                                        (rotate (- (expt (speed2 w) 1.3)) (ellipse (plankx 33) (plankx 4) "outline" (ring-color (world-BALL1 w))))))
(define (player1-image w) (overlay alissa-img
                                   (circle (plankx 10) "outline" "purple") (circle (plankx 10) "solid" "green")
                                   (rotate (- (expt (speed2 w) 1.3)) (ellipse (plankx 33) (plankx 4) "outline" (ring-color (world-BALL1 w))))))
(define (player2-image w) (overlay gaby-img
                                   (circle (plankx 10) "outline" "green") (circle (plankx 10) "solid" "purple")
                                   (rotate (expt (speed1 w) 1.3) (ellipse (plankx 33) (plankx 4) "outline" (ring-color (world-BALL2 w))))))


(define (afterimage1 w) (overlay (circle (plankx 10) "outline" "light blue")
                                  (rotate (- (expt (speed2 w) 1.3)) (ellipse (plankx 33) (plankx 4) "outline" "light blue"))))
(define (afterimage2 w) (overlay (circle (plankx 10) "outline" "light blue")
                                  (rotate (expt (speed1 w) 1.3) (ellipse (plankx 33) (plankx 4) "outline" "light blue"))))

(define-struct plasmaball (posn-0 posn-1 posn-2 posn-3))

(define (star0 w) (make-plasmaball (make-posn (random (image-width galaxy)) (random (image-height galaxy)))
                             (make-posn (random (image-width galaxy)) (random (image-height galaxy)))
                             (make-posn (random (image-width galaxy)) (random (image-height galaxy)))
                             (make-posn (random (image-width galaxy)) (random (image-height galaxy)))))

(define (rnd-angle w) (random 360))
                             

(define (star-img w) (overlay (circle (plankx 2) 100 "white")
                          (rotate (rnd-angle w) (ellipse (plankx 7) (plankx 1) 100 "white"))
                          (rotate (rnd-angle w) (ellipse (plankx 7) (plankx 1) 100 "white"))
                          (rotate (rnd-angle w) (ellipse (plankx 7) (plankx 1) 100 "white"))
                          (rotate (rnd-angle w) (ellipse (plankx 7) (plankx 1) 100 "white"))))

(define (draw-stars w)
  (place-image (star-img w) (posn-x (plasmaball-posn-0 (star0 w))) (posn-y (plasmaball-posn-0 (star0 w)))
               (place-image (star-img w) (posn-x (plasmaball-posn-1 (star0 w))) (posn-y (plasmaball-posn-1 (star0 w)))
                            (place-image (star-img w) (posn-x (plasmaball-posn-2 (star0 w))) (posn-y (plasmaball-posn-2 (star0 w)))
                                         (place-image (star-img w) (posn-x (plasmaball-posn-3 (star0 w))) (posn-y (plasmaball-posn-3 (star0 w)))
                                                      galaxy)))))

(define ball0 (make-ball (make-posn (/ (image-width galaxy) 3) (/ (image-height galaxy) 3)) (make-vel 0 0) 0 0 0))
(define ball1 (make-ball (make-posn (* 2 (/ (image-width galaxy) 3)) (* 2 (/ (image-height galaxy) 3))) (make-vel 0 0) 0 0 0))

; Initial positions for the mines, picks a random location within 1/4 of each image-dimension length from the edge of the galaxy

(define m0 (make-posn (list-ref (list (random (/ (image-width galaxy) 6)) (+ (* 5 (/ (image-width galaxy) 6)) (random (/ (image-width galaxy) 6)))) (random 2))
                      (list-ref (list (random (/ (image-height galaxy) 6)) (+ (* 5 (/ (image-height galaxy) 6)) (random (/ (image-height galaxy) 6)))) (random 2))))
(define m1 (make-posn (list-ref (list (random (/ (image-width galaxy) 6)) (+ (* 5 (/ (image-width galaxy) 6)) (random (/ (image-width galaxy) 6)))) (random 2))
                      (list-ref (list (random (/ (image-height galaxy) 6)) (+ (* 5 (/ (image-height galaxy) 6)) (random (/ (image-height galaxy) 6)))) (random 2))))
(define m2 (make-posn (list-ref (list (random (/ (image-width galaxy) 6)) (+ (* 5 (/ (image-width galaxy) 6)) (random (/ (image-width galaxy) 6)))) (random 2))
                      (list-ref (list (random (/ (image-height galaxy) 6)) (+ (* 5 (/ (image-height galaxy) 6)) (random (/ (image-height galaxy) 6)))) (random 2))))
(define m3 (make-posn (list-ref (list (random (/ (image-width galaxy) 6)) (+ (* 5 (/ (image-width galaxy) 6)) (random (/ (image-width galaxy) 6)))) (random 2))
                      (list-ref (list (random (/ (image-height galaxy) 6)) (+ (* 5 (/ (image-height galaxy) 6)) (random (/ (image-height galaxy) 6)))) (random 2))))

(define mines0 (make-mines (make-mine m0 (make-vel 0 0))
                           (make-mine m1 (make-vel 0 0))
                           (make-mine m2 (make-vel 0 0))
                           (make-mine m3 (make-vel 0 0))))

(define w0 (make-world ball0 ball1 mines0))

; world-next: World -> World
; world-next takes in a world and updates the positions of the ball
; and mines after 1 tick using their respective velocity components
(define (world-next w)
  (cond [(check-ball-hb w)
         (make-world
     (make-ball
        (make-posn
           (modulo (+ (posn-x (ball-location (world-BALL1 w))) (vel-deltax (ball-velocity (world-BALL2 w)))) (image-width galaxy))
           (modulo (+ (posn-y (ball-location (world-BALL1 w))) (vel-deltay (ball-velocity (world-BALL2 w)))) (image-height galaxy)))
        (ball-velocity (world-BALL2 w))
        (coins-collected?-1 w)
        (score?-1 w)
         0)
      (make-ball
        (make-posn
           (modulo (+ (posn-x (ball-location (world-BALL2 w))) (vel-deltax (ball-velocity (world-BALL1 w)))) (image-width galaxy))
           (modulo (+ (posn-y (ball-location (world-BALL2 w))) (vel-deltay (ball-velocity (world-BALL1 w)))) (image-height galaxy)))
        (ball-velocity (world-BALL1 w))
        (coins-collected?-2 w)
        (score?-2 w)
         0)
     (make-mines
        (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M0 (world-MINES w))))
                         (vel-deltax (mine-velocity (mines-M0 (world-MINES w))))) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M0 (world-MINES w))))
                         (vel-deltay (mine-velocity (mines-M0 (world-MINES w))))) (image-height galaxy)))
           (M0-v w))
        (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M1 (world-MINES w))))
                         (vel-deltax (mine-velocity (mines-M1 (world-MINES w))))) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M1 (world-MINES w))))
                         (vel-deltay (mine-velocity (mines-M1 (world-MINES w))))) (image-height galaxy)))
           (M1-v w))
        (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M2 (world-MINES w))))
                         (vel-deltax (mine-velocity (mines-M2 (world-MINES w))))) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M2 (world-MINES w))))
                         (vel-deltay (mine-velocity (mines-M2 (world-MINES w))))) (image-height galaxy)))
           (M2-v w))
        (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M3 (world-MINES w))))
                         (vel-deltax (mine-velocity (mines-M3 (world-MINES w))))) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M3 (world-MINES w))))
                         (vel-deltay (mine-velocity (mines-M3 (world-MINES w))))) (image-height galaxy)))
           (M3-v w))))]
        [else
         (make-world
     (make-ball
        (make-posn
           (modulo (+ (posn-x (ball-location (world-BALL1 w))) (vel-deltax (ball-velocity (world-BALL1 w)))) (image-width galaxy))
           (modulo (+ (posn-y (ball-location (world-BALL1 w))) (vel-deltay (ball-velocity (world-BALL1 w)))) (image-height galaxy)))
        (ball-velocity (world-BALL1 w))
        (coins-collected?-1 w)
        (score?-1 w)
         0)
      (make-ball
        (make-posn
           (modulo (+ (posn-x (ball-location (world-BALL2 w))) (vel-deltax (ball-velocity (world-BALL2 w)))) (image-width galaxy))
           (modulo (+ (posn-y (ball-location (world-BALL2 w))) (vel-deltay (ball-velocity (world-BALL2 w)))) (image-height galaxy)))
        (ball-velocity (world-BALL2 w))
        (coins-collected?-2 w)
        (score?-2 w)
         0)
     (make-mines
        (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M0 (world-MINES w))))
                         (vel-deltax (mine-velocity (mines-M0 (world-MINES w))))) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M0 (world-MINES w))))
                         (vel-deltay (mine-velocity (mines-M0 (world-MINES w))))) (image-height galaxy)))
           (M0-v w))
        (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M1 (world-MINES w))))
                         (vel-deltax (mine-velocity (mines-M1 (world-MINES w))))) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M1 (world-MINES w))))
                         (vel-deltay (mine-velocity (mines-M1 (world-MINES w))))) (image-height galaxy)))
           (M1-v w))
        (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M2 (world-MINES w))))
                         (vel-deltax (mine-velocity (mines-M2 (world-MINES w))))) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M2 (world-MINES w))))
                         (vel-deltay (mine-velocity (mines-M2 (world-MINES w))))) (image-height galaxy)))
           (M2-v w))
        (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M3 (world-MINES w))))
                         (vel-deltax (mine-velocity (mines-M3 (world-MINES w))))) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M3 (world-MINES w))))
                         (vel-deltay (mine-velocity (mines-M3 (world-MINES w))))) (image-height galaxy)))
           (M3-v w))))]))

#;(define (wn-helper w m)
  (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M0 (world-MINES w))))
                         (M0-v w)) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M0 (world-MINES w))))
                         (M0-v w)) (image-height galaxy)))
           (mine-velocity (mines-M0 (world-MINES w)))))



#;(define (ball-image-template w)
    (...(posn-x (ball-location (world-BALL1 w)))
        (posn-y (ball-location (world-BALL1 w)))...))    
 
; draw-balls: Ball -> Image
; draw-balls places a red dot at the position of the ball on an empty scene.
(define (draw-balls w)
    (place-image
       (player1-image w)
       (posn-x (ball-location (world-BALL1 w)))
       (posn-y (ball-location (world-BALL1 w)))
       (place-image
          (player2-image w)
          (posn-x (ball-location (world-BALL2 w)))
          (posn-y (ball-location (world-BALL2 w)))
             (draw-stars w))))

#;(define (ball-change-template w a-key)
    (...(posn-x (ball-location (world-BALL1 w)))
        (posn-y (ball-location (world-BALL1 w)))
        (vel-deltax (ball-velocity (world-BALL1 w)))
        (vel-deltay (ball-velocity (world-BALL1 w)))
        a-key...))

;(check-expect (ball-change w0 "left") (make-ball (make-posn 150 150) (make-vel -1 0) 0 0))
;(check-expect (ball-change w0 "right") (make-ball (make-posn 150 150) (make-vel 1 0) 0 0))
;(check-expect (ball-change w0 "up") (make-ball (make-posn 150 150) (make-vel 0 -1) 0 0))
;(check-expect (ball-change w0 "down") (make-ball (make-posn 150 150) (make-vel 0 1) 0 0))

; ball-change: Ball Key-Event -> Ball
; ball-change takes a ball and a key-event, and determines what the key-event
; is if the key-event is an arrow key, it returns a new ball whose velocity
; vector in the direction of the arrow key pressed. otherwise, it returns the
; same ball unmodified.

(define (hyperdrive1 w a-key)
  (cond [(key=? a-key "q")
         (make-ball
          (make-posn (+ (posn-x (ball-location (world-BALL1 w)))
                        (* 10 (vel-deltax (ball-velocity (world-BALL1 w)))))
                     (+ (posn-y (ball-location (world-BALL1 w)))
                        (* 10 (vel-deltay (ball-velocity (world-BALL1 w))))))
          (make-vel  (vel-deltax (ball-velocity (world-BALL1 w)))
                                (vel-deltay (ball-velocity (world-BALL1 w))))
          (ball-cc? (world-BALL1 w)) (ball-scr? (world-BALL1 w))
           50)]
        [else
         (make-ball
          (make-posn (posn-x (ball-location (world-BALL1 w))) (posn-y (ball-location (world-BALL1 w))))
          (make-vel  (vel-deltax (ball-velocity (world-BALL1 w)))
                                (vel-deltay (ball-velocity (world-BALL1 w))))
          (ball-cc? (world-BALL1 w)) (ball-scr? (world-BALL1 w))
          (sub1 (ball-hd? (world-BALL1 w))))]))

(define (hyperdrive2 w a-key)
  (cond [(key=? a-key "o")
         (make-ball
          (make-posn (+ (posn-x (ball-location (world-BALL2 w)))
                        (* 10 (vel-deltax (ball-velocity (world-BALL2 w)))))
                     (+ (posn-y (ball-location (world-BALL2 w)))
                        (* 10 (vel-deltay (ball-velocity (world-BALL2 w))))))
          (make-vel  (vel-deltax (ball-velocity (world-BALL2 w)))
                                (vel-deltay (ball-velocity (world-BALL2 w))))
          (ball-cc? (world-BALL2 w)) (ball-scr? (world-BALL2 w))
           50)]
        [else
         (make-ball
          (make-posn (posn-x (ball-location (world-BALL2 w))) (posn-y (ball-location (world-BALL2 w))))
          (make-vel  (vel-deltax (ball-velocity (world-BALL2 w)))
                                (vel-deltay (ball-velocity (world-BALL2 w))))
          (ball-cc? (world-BALL2 w)) (ball-scr? (world-BALL2 w))
          (sub1 (ball-hd? (world-BALL2 w))))]))

(define (ball-change w a-key)
  (make-world
   (make-ball (make-posn (posn-x (ball-location (hyperdrive1 w a-key)))
                         (posn-y (ball-location (hyperdrive1 w a-key))))
             (cond [(key=? a-key "a")
                    (make-vel  (- (vel-deltax (ball-velocity (world-BALL1 w))) (plankx 1))
                                  (vel-deltay (ball-velocity (world-BALL1 w))))]
                   [(key=? a-key "d")
                    (make-vel  (+ (vel-deltax (ball-velocity (world-BALL1 w))) (plankx 1))
                                  (vel-deltay (ball-velocity (world-BALL1 w))))]
                   [(key=? a-key "w")
                    (make-vel     (vel-deltax (ball-velocity (world-BALL1 w)))
                               (- (vel-deltay (ball-velocity (world-BALL1 w))) (plankx 1)))]
                   [(key=? a-key "s")
                    (make-vel     (vel-deltax (ball-velocity (world-BALL1 w)))
                               (+ (vel-deltay (ball-velocity (world-BALL1 w))) (plankx 1)))]
                   [else
                    (make-vel  (vel-deltax (ball-velocity (world-BALL1 w)))
                               (vel-deltay (ball-velocity (world-BALL1 w))))])
             (ball-cc? (world-BALL1 w)) (ball-scr? (world-BALL1 w)) (ball-hd? (hyperdrive1 w a-key)))
    (make-ball (make-posn (posn-x (ball-location (hyperdrive2 w a-key)))
                          (posn-y (ball-location (hyperdrive2 w a-key))))
             (cond [(key=? a-key "l")
                    (make-vel  (- (vel-deltax (ball-velocity (world-BALL2 w))) (plankx 1))
                                  (vel-deltay (ball-velocity (world-BALL2 w))))]
                   [(key=? a-key "'")
                    (make-vel  (+ (vel-deltax (ball-velocity (world-BALL2 w))) (plankx 1))
                                  (vel-deltay (ball-velocity (world-BALL2 w))))]
                   [(key=? a-key "p")
                    (make-vel     (vel-deltax (ball-velocity (world-BALL2 w)))
                               (- (vel-deltay (ball-velocity (world-BALL2 w))) (plankx 1)))]
                   [(key=? a-key ";")
                    (make-vel     (vel-deltax (ball-velocity (world-BALL2 w)))
                               (+ (vel-deltay (ball-velocity (world-BALL2 w))) (plankx 1)))]
                   [else
                    (make-vel  (vel-deltax (ball-velocity (world-BALL2 w)))
                               (vel-deltay (ball-velocity (world-BALL2 w))))])
             (ball-cc? (world-BALL2 w)) (ball-scr? (world-BALL2 w)) (ball-hd? (hyperdrive2 w a-key)))
   (make-mines
        (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M0 (world-MINES w))))
                         (vel-deltax (mine-velocity (mines-M0 (world-MINES w))))) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M0 (world-MINES w))))
                         (vel-deltay (mine-velocity (mines-M0 (world-MINES w))))) (image-height galaxy)))
           (M0-v w))
        (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M1 (world-MINES w))))
                         (vel-deltax (mine-velocity (mines-M1 (world-MINES w))))) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M1 (world-MINES w))))
                         (vel-deltay (mine-velocity (mines-M1 (world-MINES w))))) (image-height galaxy)))
           (M1-v w))
        (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M2 (world-MINES w))))
                         (vel-deltax (mine-velocity (mines-M2 (world-MINES w))))) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M2 (world-MINES w))))
                         (vel-deltay (mine-velocity (mines-M2 (world-MINES w))))) (image-height galaxy)))
           (M2-v w))
        (make-mine
           (make-posn
              (modulo (+ (posn-x (mine-location (mines-M3 (world-MINES w))))
                         (vel-deltax (mine-velocity (mines-M3 (world-MINES w))))) (image-width galaxy))
              (modulo (+ (posn-y (mine-location (mines-M3 (world-MINES w))))
                         (vel-deltay (mine-velocity (mines-M3 (world-MINES w))))) (image-height galaxy)))
           (M3-v w)))))

(define (coin-img w)
  (overlay (text (number->string (+ 1 (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))))) (plankx 7) "orange") 
           (circle (plankx 5) "solid" "gold")
           (circle (plankx 6) "solid" "orange")))

; coin-value: coin -> Number
; assigns a random value from the list to the coin


(define (c-value c) (list-ref
                    (list 5 5 5 5 5 5 10 10 10 10 15 15 15 20 20 25 50)
                    (random 17)))

(define (Coin num) (make-coin (make-posn (random (image-width galaxy)) (random (image-height galaxy))) (c-value num) num))
(define c0 (Coin 0))
(define c1 (Coin 1))
(define c2 (Coin 2))
(define c3 (Coin 3))
(define c4 (Coin 4))
(define c5 (Coin 5))
(define c6 (Coin 6))
(define c7 (Coin 7))
(define c8 (Coin 8))
(define c9 (Coin 9))
(define c10 (Coin 10))
(define c11 (Coin 11))
(define c12 (Coin 12))
(define c13 (Coin 13))
(define c14 (Coin 14))
(define c15 (Coin 15))
(define c16 (Coin 16))
(define c17 (Coin 17))
(define c18 (Coin 18))
(define c19 (Coin 19))


; stop-game: Ball -> Boolean
; takes a Ball, returns true iff the position of the coin falls within
; the x- and y- ranges of the hitbox of the Ball

(define (stop-game-green-win w)
  (cond [(>= (ball-scr? (world-BALL1 w)) 100) true]
        [else false]))
(define (stop-game-purple-win w)
  (cond [(>= (ball-scr? (world-BALL2 w)) 100) true]
        [else false]))

(define (game-over-green-M0 w)
       (and (> (posn-x (mine-location (mines-M0 (world-MINES w)))) (hitbox-xmin (ball1-hb w)))
            (< (posn-x (mine-location (mines-M0 (world-MINES w)))) (hitbox-xmax (ball1-hb w)))
            (> (posn-y (mine-location (mines-M0 (world-MINES w)))) (hitbox-ymin (ball1-hb w)))
            (< (posn-y (mine-location (mines-M0 (world-MINES w)))) (hitbox-ymax (ball1-hb w)))))
(define (game-over-green-M1 w)
       (and (> (posn-x (mine-location (mines-M1 (world-MINES w)))) (hitbox-xmin (ball1-hb w)))
            (< (posn-x (mine-location (mines-M1 (world-MINES w)))) (hitbox-xmax (ball1-hb w)))
            (> (posn-y (mine-location (mines-M1 (world-MINES w)))) (hitbox-ymin (ball1-hb w)))
            (< (posn-y (mine-location (mines-M1 (world-MINES w)))) (hitbox-ymax (ball1-hb w)))))
(define (game-over-green-M2 w)
       (and (> (posn-x (mine-location (mines-M2 (world-MINES w)))) (hitbox-xmin (ball1-hb w)))
            (< (posn-x (mine-location (mines-M2 (world-MINES w)))) (hitbox-xmax (ball1-hb w)))
            (> (posn-y (mine-location (mines-M2 (world-MINES w)))) (hitbox-ymin (ball1-hb w)))
            (< (posn-y (mine-location (mines-M2 (world-MINES w)))) (hitbox-ymax (ball1-hb w)))))
(define (game-over-green-M3 w)
       (and (> (posn-x (mine-location (mines-M3 (world-MINES w)))) (hitbox-xmin (ball1-hb w)))
            (< (posn-x (mine-location (mines-M3 (world-MINES w)))) (hitbox-xmax (ball1-hb w)))
            (> (posn-y (mine-location (mines-M3 (world-MINES w)))) (hitbox-ymin (ball1-hb w)))
            (< (posn-y (mine-location (mines-M3 (world-MINES w)))) (hitbox-ymax (ball1-hb w)))))

(define (game-over-purple-M0 w)
       (and (> (posn-x (mine-location (mines-M0 (world-MINES w)))) (hitbox-xmin (ball2-hb w)))
            (< (posn-x (mine-location (mines-M0 (world-MINES w)))) (hitbox-xmax (ball2-hb w)))
            (> (posn-y (mine-location (mines-M0 (world-MINES w)))) (hitbox-ymin (ball2-hb w)))
            (< (posn-y (mine-location (mines-M0 (world-MINES w)))) (hitbox-ymax (ball2-hb w)))))
(define (game-over-purple-M1 w)
       (and (> (posn-x (mine-location (mines-M1 (world-MINES w)))) (hitbox-xmin (ball2-hb w)))
            (< (posn-x (mine-location (mines-M1 (world-MINES w)))) (hitbox-xmax (ball2-hb w)))
            (> (posn-y (mine-location (mines-M1 (world-MINES w)))) (hitbox-ymin (ball2-hb w)))
            (< (posn-y (mine-location (mines-M1 (world-MINES w)))) (hitbox-ymax (ball2-hb w)))))
(define (game-over-purple-M2 w)
       (and (> (posn-x (mine-location (mines-M2 (world-MINES w)))) (hitbox-xmin (ball2-hb w)))
            (< (posn-x (mine-location (mines-M2 (world-MINES w)))) (hitbox-xmax (ball2-hb w)))
            (> (posn-y (mine-location (mines-M2 (world-MINES w)))) (hitbox-ymin (ball2-hb w)))
            (< (posn-y (mine-location (mines-M2 (world-MINES w)))) (hitbox-ymax (ball2-hb w)))))
(define (game-over-purple-M3 w)
       (and (> (posn-x (mine-location (mines-M3 (world-MINES w)))) (hitbox-xmin (ball2-hb w)))
            (< (posn-x (mine-location (mines-M3 (world-MINES w)))) (hitbox-xmax (ball2-hb w)))
            (> (posn-y (mine-location (mines-M3 (world-MINES w)))) (hitbox-ymin (ball2-hb w)))
            (< (posn-y (mine-location (mines-M3 (world-MINES w)))) (hitbox-ymax (ball2-hb w)))))



(define (stop-game-dbl-loss w)
  (cond [(and (>= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 20)
              (and (< (ball-scr? (world-BALL1 w)) 100) (< (ball-scr? (world-BALL2 w)) 100)))
              true]
         [else false]))

(define (stop-game-green-loss w)
  (cond [(or (game-over-green-M0 w)
             (game-over-green-M1 w)
             (game-over-green-M2 w)
             (game-over-green-M3 w))
         true]
        [else false]))

(define (stop-game-purple-loss w)
  (cond [(or (game-over-purple-M0 w)
             (game-over-purple-M1 w)
             (game-over-purple-M2 w)
             (game-over-purple-M3 w))
         true]
        [else false]))

(define (stop-game w)
  (cond [(or (stop-game-green-win w)
             (stop-game-purple-win w)
             (stop-game-green-loss w)
             (stop-game-purple-loss w)
             (stop-game-dbl-loss w)) true]
        [else false]))


; End screen conditions:

(define green-won (text "u won!!" (plankx 75) "green"))
(define purple-won (text "u won!!" (plankx 75) "purple"))
(define green-lost (text "better luck next time u green little shit" (plankx 14) "green"))
(define purple-lost (text "try Winning insted haha fuck u purple" (plankx 14) "purple"))

(define both-lost (text "sry, not enough Space Plunder" (plankx 19) "red"))

; end-screen-win: Ball -> Image
; overlays u-won message on the canvas when the game ends in a victory

(define (end-screen-green-win w)
  (place-image green-won (/ (image-width galaxy) 2) (* 3 (/ (image-height galaxy) 7))
               (place-image purple-lost (/ (image-width galaxy) 2) (* 4 (/ (image-height galaxy) 7))
                           (draw-canvas w))))
(define (end-screen-purple-win w)
  (place-image purple-won (/ (image-width galaxy) 2) (* 3 (/ (image-height galaxy) 7))
               (place-image green-lost (/ (image-width galaxy) 2) (* 4 (/ (image-height galaxy) 7))
                           (draw-canvas w))))

; end-screen-lose: Ball -> Image
; overlays u-lost message on the canvas when the game ends in a loss

(define (end-screen-loss w)
  (overlay both-lost
           (draw-canvas w)))

; end-screen: Ball -> Image
; chooses the correct message when the game ends

(define (end-screen w)
  (cond [(or (stop-game-green-win w) (stop-game-purple-loss w)) (end-screen-green-win w)]
        [(or (stop-game-purple-win w) (stop-game-green-loss w)) (end-screen-purple-win w)]
        [(stop-game-dbl-loss w) (end-screen-loss w)]))

; Draws an image of the mines overlayed atop the ball-image

#;(define (draw-mines w)
   (place-image (mine-img w) (posn-x m0) (posn-y m0)
      (place-image (mine-img w) (posn-x m1) (posn-y m1)
         (place-image (mine-img w) (posn-x m2) (posn-y m2)
            (place-image (mine-img w) (posn-x m3) (posn-y m3)
               (draw-balls w))))))


(define (draw-mines w)
   (place-image (mine-img w) (posn-x (mine-location (mines-M0 (world-MINES w))))
                             (posn-y (mine-location (mines-M0 (world-MINES w))))
      (place-image (mine-img w) (posn-x (mine-location (mines-M1 (world-MINES w))))
                                (posn-y (mine-location (mines-M1 (world-MINES w))))
         (place-image (mine-img w) (posn-x (mine-location (mines-M2 (world-MINES w))))
                                   (posn-y (mine-location (mines-M2 (world-MINES w))))
            (place-image (mine-img w) (posn-x (mine-location (mines-M3 (world-MINES w))))
                                      (posn-y (mine-location (mines-M3 (world-MINES w))))
               (draw-balls w))))))
; is true when the player has collected a nonzero # of coins and is within
; the range of the next coin


(define (cc?-not-zero-1 w) (or  (check-coin-hb-1 w c0)
                                (check-coin-hb-1 w c1)
                                (check-coin-hb-1 w c2)
                                (check-coin-hb-1 w c3)
                                (check-coin-hb-1 w c4)
                                (check-coin-hb-1 w c5)
                                (check-coin-hb-1 w c6)
                                (check-coin-hb-1 w c7)
                                (check-coin-hb-1 w c8)
                                (check-coin-hb-1 w c9)
                                (check-coin-hb-1 w c10)
                                (check-coin-hb-1 w c11)
                                (check-coin-hb-1 w c12)
                                (check-coin-hb-1 w c13)
                                (check-coin-hb-1 w c14)
                                (check-coin-hb-1 w c15)
                                (check-coin-hb-1 w c16)
                                (check-coin-hb-1 w c17)
                                (check-coin-hb-1 w c18)
                                (check-coin-hb-1 w c19)))

(define (cc?-not-zero-2 w) (or  (check-coin-hb-2 w c0)
                                (check-coin-hb-2 w c1)
                                (check-coin-hb-2 w c2)
                                (check-coin-hb-2 w c3)
                                (check-coin-hb-2 w c4)
                                (check-coin-hb-2 w c5)
                                (check-coin-hb-2 w c6)
                                (check-coin-hb-2 w c7)
                                (check-coin-hb-2 w c8)
                                (check-coin-hb-2 w c9)
                                (check-coin-hb-2 w c10)
                                (check-coin-hb-2 w c11)
                                (check-coin-hb-2 w c12)
                                (check-coin-hb-2 w c13)
                                (check-coin-hb-2 w c14)
                                (check-coin-hb-2 w c15)
                                (check-coin-hb-2 w c16)
                                (check-coin-hb-2 w c17)
                                (check-coin-hb-2 w c18)
                                (check-coin-hb-2 w c19)))


; coins-collected?: World -> Boolean
; takes a world, checks to see whether or not the current coin has been collected,
; makes a new coin with (+ 1 ball-cc?) every time a new coin is reached
(define (coins-collected?-1 w)
  (cond [(cc?-not-zero-1 w)
         (add1 (ball-cc? (world-BALL1 w)))]
        [else (ball-cc? (world-BALL1 w))]))

(define (coins-collected?-2 w)
  (cond [(cc?-not-zero-2 w)
         (add1 (ball-cc? (world-BALL2 w)))]
        [else (ball-cc? (world-BALL2 w))]))


; helper function that checks to see whether or not:
; 1 the ball and coin hitboxes overlap
; 2 the coin at the ball's current posn has already been collected
(define (check-coin-hb-1 w c)
  (and (> (posn-x (coin-posn c)) (hitbox-xmin (ball1-hb w)))
       (< (posn-x (coin-posn c)) (hitbox-xmax (ball1-hb w)))
       (> (posn-y (coin-posn c)) (hitbox-ymin (ball1-hb w)))
       (< (posn-y (coin-posn c)) (hitbox-ymax (ball1-hb w)))
          (= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) (coin-num c))))

(define (check-coin-hb-2 w c)
  (and (> (posn-x (coin-posn c)) (hitbox-xmin (ball2-hb w)))
       (< (posn-x (coin-posn c)) (hitbox-xmax (ball2-hb w)))
       (> (posn-y (coin-posn c)) (hitbox-ymin (ball2-hb w)))
       (< (posn-y (coin-posn c)) (hitbox-ymax (ball2-hb w)))
          (= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) (coin-num c))))

(define (check-ball-hb w)
  (and (> (hitbox-xmax (ball1-hb w)) (hitbox-xmin (ball2-hb w)))
       (> (hitbox-xmax (ball2-hb w)) (hitbox-xmin (ball1-hb w)))
       (> (hitbox-ymax (ball1-hb w)) (hitbox-ymin (ball2-hb w)))
       (> (hitbox-ymax (ball2-hb w)) (hitbox-ymin (ball1-hb w)))))

(define (coin-check-1 w)
  (cond [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 0) (cc-helper-1 w c0)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 1) (cc-helper-1 w c1)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 2) (cc-helper-1 w c2)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 3) (cc-helper-1 w c3)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 4) (cc-helper-1 w c4)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 5) (cc-helper-1 w c5)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 6) (cc-helper-1 w c6)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 7) (cc-helper-1 w c7)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 8) (cc-helper-1 w c8)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 9) (cc-helper-1 w c9)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 10) (cc-helper-1 w c10)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 11) (cc-helper-1 w c11)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 12) (cc-helper-1 w c12)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 13) (cc-helper-1 w c13)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 14) (cc-helper-1 w c14)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 15) (cc-helper-1 w c15)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 16) (cc-helper-1 w c16)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 17) (cc-helper-1 w c17)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 18) (cc-helper-1 w c18)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 19) (cc-helper-1 w c19)]
        [else (ball-cc? (world-BALL1 w))]))

(define (cc-helper-1 w c)
  (+ (ball-scr? (world-BALL1 w)) (coin-value c)))

(define (coin-check-2 w)
  (cond [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 0) (cc-helper-2 w c0)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 1) (cc-helper-2 w c1)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 2) (cc-helper-2 w c2)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 3) (cc-helper-2 w c3)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 4) (cc-helper-2 w c4)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 5) (cc-helper-2 w c5)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 6) (cc-helper-2 w c6)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 7) (cc-helper-2 w c7)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 8) (cc-helper-2 w c8)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 9) (cc-helper-2 w c9)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 10) (cc-helper-2 w c10)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 11) (cc-helper-2 w c11)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 12) (cc-helper-2 w c12)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 13) (cc-helper-2 w c13)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 14) (cc-helper-2 w c14)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 15) (cc-helper-2 w c15)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 16) (cc-helper-2 w c16)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 17) (cc-helper-2 w c17)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 18) (cc-helper-2 w c18)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 19) (cc-helper-2 w c19)]
        [else (ball-cc? (world-BALL2 w))]))

(define (cc-helper-2 w c)
  (+ (ball-scr? (world-BALL2 w)) (coin-value c)))

; checks the score i think?

(define (score?-1 w)
  (cond [(cc?-not-zero-1 w)
         (coin-check-1 w)]
        [else (ball-scr? (world-BALL1 w))]))

(define (score?-2 w)
  (cond [(cc?-not-zero-2 w)
         (coin-check-2 w)]
        [else (ball-scr? (world-BALL2 w))]))


(define (draw-score-total-1 w)
  (overlay (text (number->string (score?-1 w)) (plankx 25) "gold")
           (text (number->string (score?-1 w)) (plankx 27) "green")))
(define (draw-score-total-2 w)
  (overlay (text (number->string (score?-2 w)) (plankx 25) "gold")
           (text (number->string (score?-2 w)) (plankx 27) "purple")))

(define (draw-afterimage w)
  (cond [(and (> (ball-hd? (world-BALL1 w)) 0) (> (ball-hd? (world-BALL2 w)) 0))
         (place-image (afterimage1 w)
                      (- (posn-x (ball-location (world-BALL1 w)))
                      (* 10 (vel-deltax (ball-velocity (world-BALL1 w)))))
                      (- (posn-y (ball-location (world-BALL1 w)))
                      (* 10 (vel-deltay (ball-velocity (world-BALL1 w)))))
                      (place-image (afterimage2 w)
                                   (- (posn-x (ball-location (world-BALL2 w)))
                                   (* 10 (vel-deltax (ball-velocity (world-BALL2 w)))))
                                   (- (posn-y (ball-location (world-BALL2 w)))
                                   (* 10 (vel-deltay (ball-velocity (world-BALL2 w)))))
                                   (draw-coins w)))]
        [(> (ball-hd? (world-BALL1 w)) 0)
         (place-image (afterimage1 w)
                      (- (posn-x (ball-location (world-BALL1 w)))
                      (* 10 (vel-deltax (ball-velocity (world-BALL1 w)))))
                      (- (posn-y (ball-location (world-BALL1 w)))
                      (* 10 (vel-deltay (ball-velocity (world-BALL1 w)))))
                      (draw-coins w))]
        [(> (ball-hd? (world-BALL2 w)) 0)
         (place-image (afterimage2 w)
                                   (- (posn-x (ball-location (world-BALL2 w)))
                                   (* 10 (vel-deltax (ball-velocity (world-BALL2 w)))))
                                   (- (posn-y (ball-location (world-BALL2 w)))
                                   (* 10 (vel-deltay (ball-velocity (world-BALL2 w)))))
                                   (draw-coins w))]
        [else (draw-coins w)]))

(define (dsl-helper c)
  (text (string-append "+" (number->string (coin-value c)) "!") (plankx 18) "gold"))

(define (draw-score-last w)
  (cond [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 1) (dsl-helper c0)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 2) (dsl-helper c1)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 3) (dsl-helper c2)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 4) (dsl-helper c3)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 5) (dsl-helper c4)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 6) (dsl-helper c5)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 7) (dsl-helper c6)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 8) (dsl-helper c7)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 9) (dsl-helper c8)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 10) (dsl-helper c9)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 11) (dsl-helper c10)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 12) (dsl-helper c11)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 13) (dsl-helper c12)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 14) (dsl-helper c13)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 15) (dsl-helper c14)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 16) (dsl-helper c15)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 17) (dsl-helper c16)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 18) (dsl-helper c17)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 19) (dsl-helper c18)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 20) (dsl-helper c19)]
        [else (text "" (plankx 18) "gold")]))


(define (dc-helper w c)
  (place-image (coin-img w) (posn-x (coin-posn c)) (posn-y (coin-posn c))
                          (draw-mines w)))
  
(define (draw-coins w)
  (cond [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 0) (dc-helper w c0)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 1) (dc-helper w c1)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 2) (dc-helper w c2)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 3) (dc-helper w c3)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 4) (dc-helper w c4)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 5) (dc-helper w c5)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 6) (dc-helper w c6)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 7) (dc-helper w c7)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 8) (dc-helper w c8)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 9) (dc-helper w c9)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 10) (dc-helper w c10)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 11) (dc-helper w c11)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 12) (dc-helper w c12)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 13) (dc-helper w c13)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 14) (dc-helper w c14)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 15) (dc-helper w c15)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 16) (dc-helper w c16)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 17) (dc-helper w c17)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 18) (dc-helper w c18)]
        [(= (+ (ball-cc? (world-BALL1 w)) (ball-cc? (world-BALL2 w))) 19) (dc-helper w c19)]
        [else (draw-mines w)]))

; Draws final canvas for main function

(define (draw-canvas w)
  (place-image (draw-score-total-1 w) (/ (image-width galaxy) 4) (plankx 30)
               (place-image (draw-score-total-2 w) (* 3 (/ (image-width galaxy) 4)) (plankx 30)
                            (place-image (draw-score-last w) (/ (image-width galaxy) 2) (plankx 30)
                           (draw-afterimage w)))))


(define (game w)
  (big-bang w (on-tick world-next .1)
                           (on-key ball-change)
                           (to-draw draw-canvas)
                           (stop-when stop-game end-screen)))

(game w0)
