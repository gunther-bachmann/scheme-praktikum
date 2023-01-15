#lang racket

(require 2htdp/image)
(require lang/posn)
(require 2htdp/universe)

(define window (rectangle 15 15 "outline" "white"))

(define block
  (above (beside/align "bottom"
                       (triangle 40 "solid" "red")
                       (triangle 30 "solid" "blue"))
         (underlay/xy 
          (rectangle 70 40 "solid" "black")
          7 15
          window)))

(define door (rectangle 15 25 "solid" "brown"))



; (overlay/align "center" "bottom" door block)
(define door-with-knob
  (overlay/align "right" "center" (circle 3 "solid" "white") door))
; (overlay/align "center" "bottom" door-with-knob block)


;(above/align "left" wand boden)

(define eckwand (bitmap "./media/Sprite-eckwand-gruen.png"))

;(beside/align "bottom" wand eckwand)
        

(define WAND-BILD (bitmap "./media/Sprite-wand-gruen.png"))
(define ECK-WAND-BILD (bitmap "./media/Sprite-eckwand-gruen.png"))
(define BODEN-BILD (bitmap "./media/Sprite-boden-blau.png"))
(define HOLZBODEN-BILD (bitmap "./media/Sprite-holzboden.png"))
(define MAUERWAND-BILD (bitmap "./media/Sprite-mauerwand.png"))
(define FACKEL-ROT-BILD (bitmap "./media/Sprite-fackel-rot.png"))
(define ECKWAND-ROT-BILD (bitmap "./media/Sprite-eckwand-rot.png"))
(define FALLTUER-BILD (bitmap "./media/Sprite-falltuer.png"))
(define SCHATZTRUHE-BILD (bitmap "./media/Sprite-schatztruhe.png"))
(define ECKWAND-ROT-90-BILD (bitmap "./media/Sprite-eckwand-rot.90.png"))
(define ECKWAND-ROT-180-BILD (bitmap "./media/Sprite-eckwand-rot.180.png"))
(define ECKWAND-ROT-270-BILD (bitmap "./media/Sprite-eckwand-rot.270.png"))
(define MAUERWAND-90-BILD (bitmap "./media/Sprite-mauerwand.90.png"))
(define MAUERWAND-180-BILD (bitmap "./media/Sprite-mauerwand.180.png"))
(define MAUERWAND-270-BILD (bitmap "./media/Sprite-mauerwand.270.png"))
(define SCHATZTRUHE-90-BILD (bitmap "./media/Sprite-schatztruhe.90.png"))
(define SCHATZTRUHE-270-BILD (bitmap "./media/Sprite-schatztruhe.270.png"))
(define FACKEL-rot-90-BILD (bitmap "./media/Sprite-fackel-rot.90.png"))
(define FACKEL-rot-180-BILD (bitmap "./media/Sprite-fackel-rot.180.png"))
(define FACKEL-rot-270-BILD (bitmap "./media/Sprite-fackel-rot.270.png"))
(define TUER-ROT-BILD (bitmap "./media/Sprite-tuer-rot.png"))
(define TUER-ROT-2.0-BILD (bitmap "./media/Sprite-tuer-rot-2.0.png"))
(define TUER-ROT-2.0-180-BILD (bitmap "./media/Sprite-tuer-rot-2.0.180.png"))
(define NORMADE-2.-BILD (bitmap "./media/Sprite-normade 2.png"))
(define HERZOG-2.-BILD (bitmap "./media/Sprite-herzog 2.png"))
(define MAGIER-BILD (bitmap "./media/Sprite-magier.png"))
(define TOPF-BILD (bitmap "./media/Sprite-food-topf.png"))
(define PRIESTER-BILD (bitmap "./media/Sprite-sonnenprister.png"))

(define MAGIER-1 (bitmap "./media/Sprite-magier1.png"))
(define MAGIER-2 (bitmap "./media/Sprite-magier2.png"))
(define MAGIER-3 (bitmap "./media/Sprite-magier3.png"))

(define SONNENPRIESTER-1 (bitmap "./media/Sprite-sonnenprister1.png"))
(define SONNENPRIESTER-2 (bitmap "./media/Sprite-sonnenprister2.png"))


(define FACKEL-1 (bitmap "./media/Sprite-fackel-rot.animiert1.png"))
(define FACKEL-2 (bitmap "./media/Sprite-fackel-rot.animiert2.png"))
(define FACKEL-3 (bitmap "./media/Sprite-fackel-rot.animiert3.png"))
(define FACKEL-4 (bitmap "./media/Sprite-fackel-rot.animiert4.png"))

(define spielfeld
  (list 'rote-eckwand.270 'mauer 'mauer 'fackel 'mauer 'tuer-rot-2.0 'mauer 'rote-eckwand
        'mauer.270 'holzboden 'holzboden 'holzboden 'holzboden 'holzboden 'holzboden 'mauer.90
        'mauer.270 'holzboden 'holzboden 'holzboden 'holzboden 'holzboden 'holzboden 'mauer.90
        'fackel.270 'holzboden 'holzboden 'holzboden 'holzboden 'holzboden 'holzboden 'mauer.90
        'mauer.270 'holzboden 'holzboden 'holzboden 'holzboden 'holzboden 'holzboden 'fackel.90
        'mauer.270 'holzboden 'holzboden 'holzboden 'holzboden 'holzboden 'holzboden 'mauer.90
        'mauer.270 'holzboden 'holzboden 'holzboden 'holzboden 'holzboden 'holzboden 'mauer.90
        'rote-eckwand.180 'mauer.180 'mauer.180 'mauer.180 'mauer.180 'mauer.180 'mauer.180 'rote-eckwand.90
        ))

(define SKALIERUNG 3)


(define (magier frame)
  (list-ref (list MAGIER-1 MAGIER-2 MAGIER-3 MAGIER-2  MAGIER-1  MAGIER-1)
            (modulo (quotient frame 30) 6)))

(define (sonnenpriester frame)
  (list-ref (list SONNENPRIESTER-1 SONNENPRIESTER-2)
            (modulo (quotient frame 40) 2)))

(define (fackel frame)
  (list-ref (list FACKEL-1 FACKEL-2 FACKEL-3 FACKEL-4)
            (modulo (quotient frame 10) 4) ) )

(define (kachel->bild kachel frame)
  (cond
    [(eq? kachel 'sonnenpriester) (sonnenpriester frame)]
    [(eq? kachel 'topf) TOPF-BILD]
    [(eq? kachel 'magier) (magier frame)]
    [(eq? kachel 'herzog-2) HERZOG-2.-BILD]
    [(eq? kachel 'normade-2) NORMADE-2.-BILD]
    [(eq? kachel 'tuer-rot-2.0.180) TUER-ROT-2.0-180-BILD]
    [(eq? kachel 'tuer-rot-2.0) TUER-ROT-2.0-BILD]
    [(eq? kachel 'tuer-rot) TUER-ROT-BILD]
    [(eq? kachel 'fackel.270) FACKEL-rot-270-BILD]
    [(eq? kachel 'fackel.180) FACKEL-rot-180-BILD]
    [(eq? kachel 'fackel.90) FACKEL-rot-90-BILD]
    [(eq? kachel 'schatz.270) SCHATZTRUHE-270-BILD]
    [(eq? kachel 'schatz.90) SCHATZTRUHE-90-BILD]
    [(eq? kachel 'mauer.270) MAUERWAND-270-BILD]
    [(eq? kachel 'mauer.180) MAUERWAND-180-BILD]
    [(eq? kachel 'mauer.90) MAUERWAND-90-BILD]
    [(eq? kachel 'rote-eckwand.270) ECKWAND-ROT-270-BILD]
    [(eq? kachel 'rote-eckwand.180) ECKWAND-ROT-180-BILD]
    [(eq? kachel 'rote-eckwand.90) ECKWAND-ROT-90-BILD]
    [(eq? kachel 'falltuer) FALLTUER-BILD]
    [(eq? kachel 'rote-eckwand) ECKWAND-ROT-BILD]
    [(eq? kachel 'fackel) (fackel frame)]
    [(eq? kachel 'mauer) MAUERWAND-BILD] 
    [(eq? kachel 'holzboden) HOLZBODEN-BILD]
    [(eq? kachel 'wand) WAND-BILD]
    [(eq? kachel 'eckwand) ECK-WAND-BILD]
    [(eq? kachel 'boden) BODEN-BILD]
    [else BODEN-BILD]))


(define SPIELFELD-BREITE 8)
(define SPIELFELD-HÖHE 8)

(define KACHEL-BREITE 16)
(define KACHEL-HÖHE 16)

(define (kachel-koordinaten->posn kachel-x kachel-y)
  (make-posn (* SKALIERUNG (+ (/ KACHEL-BREITE 2)
                              (* KACHEL-BREITE
                                 kachel-x)))
             (* SKALIERUNG (+ (/ KACHEL-HÖHE 2)
                              (* KACHEL-HÖHE
                                 kachel-y)))))

(define (kachel->posn)
  (map (lambda (number)
         (kachel-koordinaten->posn (modulo number SPIELFELD-BREITE ) (quotient number SPIELFELD-HÖHE )))
       (range 0 (* SPIELFELD-BREITE SPIELFELD-HÖHE))))

(define start-szene
  (empty-scene (* SKALIERUNG SPIELFELD-BREITE KACHEL-BREITE)
               (* SKALIERUNG SPIELFELD-HÖHE KACHEL-HÖHE)
               "black"))


(define (spielfeld-szene das-spielfeld die-szene frame)
  (place-images (map (lambda (bild) (scale SKALIERUNG bild))
                     (map (lambda (kachel) (kachel->bild kachel frame)) das-spielfeld))
                (kachel->posn)
                die-szene))

(define spielelemente
  (list 'schatz.90 6 3
        'schatz.270 1 1
        'magier 5 3
        'sonnenpriester 2 1
        ))

(define (kachel-liste->sk-bild-liste kachel-liste frame)
  (map (lambda (bild) (scale SKALIERUNG bild))                  
       (map (lambda (kachel) (kachel->bild kachel frame)) kachel-liste)))

(define (-make-pairs lst res)
  (if (null? lst) res
      (-make-pairs (cddr lst)
                   (append res (list (cons (first lst) (second lst)))))))
(define (make-pairs lst) (-make-pairs lst (list)))

(define (spielelemente->bilder elemente frame)
  (kachel-liste->sk-bild-liste (filter (lambda (element) (symbol? element)) elemente) frame))

(define (spielelemente->posn elemente)
  (map (lambda (koordinaten) (kachel-koordinaten->posn (car koordinaten) (cdr koordinaten)))
                   (make-pairs (filter (lambda (element) (number? element)) elemente))))

(define (draw frame)
(place-images (spielelemente->bilder spielelemente frame)
              (spielelemente->posn spielelemente)
              (spielfeld-szene spielfeld start-szene frame)))

(big-bang 0
  (on-tick (lambda (n) (+ 1 n)))
  (to-draw draw))
