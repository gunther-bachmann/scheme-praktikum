#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(define FACKEL-1 (bitmap "./media/Sprite-fackel-rot.animiert1.png"))
(define FACKEL-2 (bitmap "./media/Sprite-fackel-rot.animiert2.png"))
(define FACKEL-3 (bitmap "./media/Sprite-fackel-rot.animiert3.png"))
(define FACKEL-4 (bitmap "./media/Sprite-fackel-rot.animiert4.png"))

(define (create-fackel-scene frame)
  (scale 6 (fackel frame)))
 
(define (fackel frame)
  (list-ref (list FACKEL-1 FACKEL-2 FACKEL-3 FACKEL-4)
            (modulo (quotient frame 10) 4) ) )
 
; (animate create-fackel-scene)

(big-bang 0
  (on-tick (lambda (n) (+ 1 n)))
  (to-draw create-fackel-scene))
