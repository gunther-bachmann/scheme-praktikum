#lang racket

(require 2htdp/image)
(require lang/posn)
(require 2htdp/universe)

(struct world (frame figure kreaturen gegenstaende) #:transparent)

(struct figure (position start-frame target-posn) #:transparent)

(define eckwand (bitmap "./media/Sprite-eckwand-gruen.png"))
       

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
;(define DUCK-BILD (bitmap "./media/Sprite-duck.png"))

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

(define (fackel frame [angle 0] [offset 0])
  (rotate angle
          (list-ref (list FACKEL-1 FACKEL-2 FACKEL-3 FACKEL-4)
                    (modulo (quotient (+ offset frame) 10) 4) ) ))

(define (kachel->bild kachel frame)
  (cond
 ;   [(eq? kachel 'duck) DUCK-BILD]
    [(eq? kachel 'sonnenpriester) (sonnenpriester frame)]
    [(eq? kachel 'topf) TOPF-BILD]
    [(eq? kachel 'magier) (magier frame)]
    [(eq? kachel 'herzog-2) HERZOG-2.-BILD]
    [(eq? kachel 'normade-2) NORMADE-2.-BILD]
    [(eq? kachel 'tuer-rot-2.0.180) TUER-ROT-2.0-180-BILD]
    [(eq? kachel 'tuer-rot-2.0) TUER-ROT-2.0-BILD]
    [(eq? kachel 'tuer-rot) TUER-ROT-BILD]
    [(eq? kachel 'fackel.270) (fackel frame 90 3)]
    [(eq? kachel 'fackel.180) (fackel frame 180 5)]
    [(eq? kachel 'fackel.90) (fackel frame 270 7)]
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
        ; 'magier 5 3
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

; geschwindigkeit der figuren, sollte Teiler von KACHEL-BREITE sein
(define WALKING-SPEED 2)

; aktuelle position einer figure, die sich moeglicherweise bewegt
(define (current-figure-posn frame a-figure)
  (define posndx (- (posn-x (figure-target-posn a-figure)) (posn-x (figure-position a-figure))))
  (define posndy (- (posn-y (figure-target-posn a-figure)) (posn-y (figure-position a-figure))))
  (define finished (- frame (figure-start-frame a-figure)))
  (if (> frame 
         (+ (quotient KACHEL-BREITE WALKING-SPEED) (figure-start-frame a-figure)))
      (figure-target-posn a-figure)
      (make-posn (+ (/ (* finished posndx WALKING-SPEED) KACHEL-BREITE) (posn-x (figure-position a-figure)))
                 (+ (/ (* finished posndy WALKING-SPEED) KACHEL-HÖHE) (posn-y (figure-position a-figure))))))

; zeichne die aktuelle Welt
(define (draw a-world)
  (define frame (world-frame a-world))
  (define figure-posn (current-figure-posn (world-frame a-world) (world-figure a-world)))
  (place-images (append (spielelemente->bilder (list 'magier) frame) (spielelemente->bilder spielelemente frame))
                (append (list figure-posn) (spielelemente->posn spielelemente))
                (spielfeld-szene spielfeld start-szene frame)))

; Bildschirmposition in Kachelposition umrechnen
(define (posn->kachel-koordinaten a-posn)
  (cons  (quotient
          (- (quotient (posn-x a-posn) SKALIERUNG)
             (/ KACHEL-BREITE 2))
          KACHEL-BREITE)
         (quotient
          (- (quotient (posn-y a-posn) SKALIERUNG)
             (/ KACHEL-HÖHE 2))
          KACHEL-HÖHE)))

; Ziel Bildschirmposition aus aktueller Positin der Figure +dx +dy berechnen
(define (target-posn a-figure dx dy)
  (define fig-kachel-pos (posn->kachel-koordinaten (figure-target-posn a-figure))) 
  (kachel-koordinaten->posn (+ (car fig-kachel-pos) dx) (+ (cdr fig-kachel-pos) dy)))

; Mache die Figure bewegt ab start-frame um +dx +dy
(define (moving-figure a-figure dx dy start-frame)
  (figure (figure-target-posn a-figure) start-frame (target-posn a-figure dx dy)))

; Tastaturkommandos
(define (tastatur-behandlung a-world key-event)
  (cond [(key=? key-event "left") (struct-copy world a-world [figure (moving-figure (world-figure a-world) -1 0 (world-frame a-world))])]
        [(key=? key-event "right") (struct-copy world a-world [figure (moving-figure (world-figure a-world) 1 0 (world-frame a-world))])]
        [(key=? key-event "up") (struct-copy world a-world [figure (moving-figure (world-figure a-world) 0 -1 (world-frame a-world))])]
        [(key=? key-event "down") (struct-copy world a-world [figure (moving-figure (world-figure a-world) 0 1 (world-frame a-world))])]
        [else a-world]))

;; (big-bang (world 0 (figure (kachel-koordinaten->posn 5 3) 0 (kachel-koordinaten->posn 5 3)) (list)(list))
;;   (on-tick (lambda (gerade) (struct-copy world gerade [frame (+ 1 (world-frame gerade))])))
;;   (on-key tastatur-behandlung)
;;   (to-draw draw))
