#lang racket

(require 2htdp/image)
(require lang/posn)

(define window (rectangle 15 15 "outline" "white"))

(define Annemike
  (above (beside/align "bottom"
                       (triangle 40 "solid" "red")
                       (triangle 30 "solid" "blue"))
         (underlay/xy 
          (rectangle 70 40 "solid" "black")
          7 15
          window)))

(define door (rectangle 15 25 "solid" "brown"))



; (overlay/align "center" "bottom" door Annemike)
(define door-with-knob
  (overlay/align "right" "center" (circle 3 "solid" "white") door))
; (overlay/align "center" "bottom" door-with-knob Annemike)


(define boden (bitmap "./media/Sprite-boden-blau.png"))

(define wand (bitmap "./media/Sprite-wand-gruen.png"))

;(above/align "left" wand boden)

(define eckwand (bitmap "./media/Sprite-eckwand-gruen.png"))

;(beside/align "bottom" wand eckwand)

(define spielfeld
  (list 'wand 'wand 'wand 'eck-wand
        'wand 'boden 'boden 'wand
        'wand 'boden 'boden 'wand
        'wand 'wand 'wand 'wand))

(define WAND-BILD (bitmap "./media/Sprite-wand-gruen.png"))
(define ECK-WAND-BILD (bitmap "./media/Sprite-eckwand-gruen.png"))
(define BODEN-BILD (bitmap "./media/Sprite-boden-blau.png"))

(define (kachel->bild kachel)
  (cond
   [(eq? kachel 'wand) WAND-BILD]
   [(eq? kachel 'eck-wand) ECK-WAND-BILD]
   [(eq? kachel 'boden) BODEN-BILD]))

(define KACHEL-BREITE 16)
(define KACHEL-HÖHE 16)

(define (kachel->posn)
  (map (lambda (number)
              (make-posn (+ (/ KACHEL-BREITE 2)
                           (* KACHEL-BREITE (modulo number 4)))
                         (+ (/ KACHEL-HÖHE 2)
                           (* KACHEL-HÖHE (quotient number 4)))))
        (range 0 16)))


(define start-szene
  (empty-scene (* 4 KACHEL-BREITE)
               (* 4 KACHEL-HÖHE)
               "black"))


(define (spielfeld-szene das-spielfeld die-szene)
  (place-images (map kachel->bild das-spielfeld)
                (kachel->posn)
                die-szene))


(spielfeld-szene spielfeld start-szene)
