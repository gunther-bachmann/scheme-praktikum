#lang racket

(require 2htdp/image)
(require lang/posn)
(require 2htdp/universe)

(define SKALIERUNG 4)

(define SPIELFELD-BREITE 8)
(define SPIELFELD-HÖHE 8)

(define KACHEL-BREITE 16)
(define KACHEL-HÖHE 16)

(define INVENTORY-BREITE 4)
(define INVENTORY-HÖHE 3)

(define ANIMATIONS-FRAMES 6)

(struct welt (frame feld elemente figur zustand))
(struct figur
  (kachel start-position end-position start-frame inventory))
(struct inventory (gegenstände))
(struct gegenstand (kachel))

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

(struct spielelement (kachel position))

(define WAND-BILD (bitmap "./media/Sprite-wand-gruen.png"))
(define ECK-WAND-BILD (bitmap "./media/Sprite-eckwand-gruen.png"))
(define BODEN-BILD (bitmap "./media/Sprite-boden-blau.png"))
(define HOLZBODEN-BILD (bitmap "./media/Sprite-holzboden.png"))
(define MAUERWAND-BILD (bitmap "./media/Sprite-mauerwand.png"))
(define FACKEL-ROT-BILD (bitmap "./media/Sprite-fackel-rot.png"))
(define ECKWAND-ROT-BILD (bitmap "./media/Sprite-eckwand-rot.png"))
(define FALLTUER-BILD (bitmap "./media/Sprite-falltuer.png"))
(define SCHATZTRUHE-BILD (bitmap "./media/Sprite-schatztruhe.png"))
(define TUER-ROT-BILD (bitmap "./media/Sprite-tuer-rot.png"))
(define TUER-ROT-2.0-BILD (bitmap "./media/Sprite-tuer-rot-2.0.png"))
(define TUER-ROT-2.0-180-BILD (bitmap "./media/Sprite-tuer-rot-2.0.180.png"))
(define NORMADE-2.-BILD (bitmap "./media/Sprite-normade 2.png"))
(define HERZOG-2.-BILD (bitmap "./media/Sprite-herzog 2.png"))
(define MAGIER-BILD (bitmap "./media/Sprite-magier.png"))
(define TOPF-BILD (bitmap "./media/Sprite-food-topf.png"))
(define PRIESTER-BILD (bitmap "./media/Sprite-sonnenprister.png"))
(define TRANK-BLAU-BILD (bitmap "./media/Sprite-trank-blau.png"))
(define BROT-BILD (bitmap "./media/Sprite-brot.png"))
(define KAMPFSTAB-BILD (bitmap "./media/Sprite-kampfstab.png"))
(define MUENZE-A-BILD (bitmap "./media/Sprite-muenz.a.png"))
(define TRANK-ROT-BILD (bitmap "./media/Sprite-trank-rot.png"))
(define INVENTORY-BLAU-BILD (bitmap "./media/Sprite-inventory-blau.png"))
(define MUENZE-BILD (bitmap "./media/Sprite-muenze.png"))


(define BUERGER-1 (bitmap "./media/Sprite-reicher-Buerger1.png"))
(define BUERGER-2 (bitmap "./media/Sprite-reicher-Buerger2.png"))

(define MAGIER-1 (bitmap "./media/Sprite-magier1.png"))
(define MAGIER-2 (bitmap "./media/Sprite-magier2.png"))
(define MAGIER-3 (bitmap "./media/Sprite-magier3.png"))

(define SONNENPRIESTER-1 (bitmap "./media/Sprite-sonnenprister1.png"))
(define SONNENPRIESTER-2 (bitmap "./media/Sprite-sonnenprister2.png"))

(define FACKEL-1 (bitmap "./media/Sprite-fackel-rot.animiert1.png"))
(define FACKEL-2 (bitmap "./media/Sprite-fackel-rot.animiert2.png"))
(define FACKEL-3 (bitmap "./media/Sprite-fackel-rot.animiert3.png"))
(define FACKEL-4 (bitmap "./media/Sprite-fackel-rot.animiert4.png"))

;; ================ Animierte Kacheln

; Bild (nicht skaliert) vom Buerger (für Animation)
(define (buerger frame)
  (list-ref (list BUERGER-1 BUERGER-2)
            (modulo (quotient frame 40) 2)))

;
(define (magier frame)
  (list-ref (list MAGIER-1 MAGIER-2 MAGIER-3 MAGIER-2  MAGIER-1  MAGIER-1)
            (modulo (quotient frame 30) 6)))

(define (sonnenpriester frame)
  (list-ref (list SONNENPRIESTER-1 SONNENPRIESTER-2)
            (modulo (quotient frame 40) 2)))

(define (fackel frame)
  (list-ref (list FACKEL-1 FACKEL-2 FACKEL-3 FACKEL-4)
            (modulo (quotient frame 10) 4) ) )

; Kachel in animiertes Bild umgewandelt (nicht skaliert)
(define (kachel->bild kachel frame)
  (cond
    [(eq? kachel 'muenze-blau) MUENZE-BILD]
    [(eq? kachel 'inventory-blau) INVENTORY-BLAU-BILD]
    [(eq? kachel 'trank-rot) TRANK-ROT-BILD]
    [(eq? kachel 'muenze.a) MUENZE-A-BILD]
    [(eq? kachel 'kampfstab) KAMPFSTAB-BILD]
    [(eq? kachel 'brot) BROT-BILD]
    [(eq? kachel 'trank-blau) TRANK-BLAU-BILD]
    [(eq? kachel 'buerger) (buerger frame)]
    [(eq? kachel 'sonnenpriester) (sonnenpriester frame)]
    [(eq? kachel 'topf) TOPF-BILD]
    [(eq? kachel 'magier) (magier frame)]
    [(eq? kachel 'herzog-2) HERZOG-2.-BILD]
    [(eq? kachel 'normade-2) NORMADE-2.-BILD]
    [(eq? kachel 'tuer-rot-2.0.180) TUER-ROT-2.0-180-BILD]
    [(eq? kachel 'tuer-rot-2.0)  TUER-ROT-2.0-BILD]
    [(eq? kachel 'tuer-rot) TUER-ROT-BILD]
    [(eq? kachel 'fackel) (fackel frame)]
    [(eq? kachel 'fackel.270) (rotate 90 (fackel frame))]
    [(eq? kachel 'fackel.180) (rotate 180 (fackel frame))]
    [(eq? kachel 'fackel.90) (rotate 270 (fackel frame))]
    [(eq? kachel 'schatz.270) (rotate 90 SCHATZTRUHE-BILD)]
    [(eq? kachel 'schatz.90) (rotate 270 SCHATZTRUHE-BILD)]
    [(eq? kachel 'mauer.270) (rotate 90 MAUERWAND-BILD)]
    [(eq? kachel 'mauer.180) (rotate 180 MAUERWAND-BILD)]
    [(eq? kachel 'mauer.90) (rotate 270 MAUERWAND-BILD)]
    [(eq? kachel 'rote-eckwand.270) (rotate 90 ECKWAND-ROT-BILD)]
    [(eq? kachel 'rote-eckwand.180) (rotate 180 ECKWAND-ROT-BILD) ]
    [(eq? kachel 'rote-eckwand.90) (rotate 270 ECKWAND-ROT-BILD)]
    [(eq? kachel 'falltuer) FALLTUER-BILD]
    [(eq? kachel 'rote-eckwand) ECKWAND-ROT-BILD]
    [(eq? kachel 'mauer) MAUERWAND-BILD] 
    [(eq? kachel 'holzboden) HOLZBODEN-BILD]
    [(eq? kachel 'wand) WAND-BILD]
    [(eq? kachel 'eckwand) ECK-WAND-BILD]
    [(eq? kachel 'boden) BODEN-BILD]
    [else MUENZE-BILD]))

;;================ INVENTORY

(define start-inventory
  (inventory (list (gegenstand 'trank-rot)
                   (gegenstand 'topf)
                   (gegenstand 'kampfstab)
                   (gegenstand 'brot)
                   (gegenstand 'muenze)
                   (gegenstand 'trank-blau))))

(define empty-inventory
  (empty-scene (* SKALIERUNG INVENTORY-BREITE KACHEL-BREITE)
               (* SKALIERUNG INVENTORY-HÖHE KACHEL-HÖHE)
               "light gray")) 

(define (gegenstände->bilder die-gegenstände)
  (map (lambda (gegenstand)
         (kachel->bild (gegenstand-kachel gegenstand) frame))
       die-gegenstände))

(define (draw-inventory das-inventory frame)
  (place-images
   (map (lambda (bild) (scale SKALIERUNG bild))
        (append (gegenstände->bilder
                 (inventory-gegenstände das-inventory))
                (make-list (*  INVENTORY-BREITE INVENTORY-HÖHE) (kachel->bild 'inventory-blau 0))))
   (append (take (kachel->posn INVENTORY-BREITE INVENTORY-HÖHE) (length (inventory-gegenstände das-inventory)))
           (kachel->posn INVENTORY-BREITE INVENTORY-HÖHE))
   empty-inventory))

(define (inventory-posn)
  (make-posn
   (/ (* SKALIERUNG KACHEL-BREITE SPIELFELD-BREITE) 2)
   (/ (* SKALIERUNG KACHEL-HÖHE SPIELFELD-HÖHE) 2)))
;; ============ Umrechnungen Bildschirmposition und Kachelposition

; Kachelposition in Bildschirmposition umrechnen
(define (kachel-koordinaten->posn kachel-x kachel-y)
  (make-posn (* SKALIERUNG (+ (/ KACHEL-BREITE 2)
                              (* KACHEL-BREITE
                                 kachel-x)))
             (* SKALIERUNG (+ (/ KACHEL-HÖHE 2)
                              (* KACHEL-HÖHE
                                 kachel-y)))))


(define (kachel->posn breite höhe)
  (map (lambda (number)
         (kachel-koordinaten->posn
          (modulo number breite)
          (quotient number breite)))
       (range 0 (* breite höhe))))

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

(define (figur-position-anpassen position)
  (make-posn (posn-x position) (- (posn-y position) (* SKALIERUNG (/ KACHEL-HÖHE 2)))))



;; ========= Positionierung Spielelemente

(define spielelemente
  (list (spielelement 'buerger (kachel-koordinaten->posn 4 5))
        (spielelement 'schatz.90 (kachel-koordinaten->posn 6 3))
        (spielelement 'schatz.270 (kachel-koordinaten->posn 1 1))
        ))


(define (kachel-liste->sk-bild-liste kachel-liste frame)
  (map (lambda (bild) (scale SKALIERUNG bild))                  
       (map (lambda (kachel) (kachel->bild kachel frame)) kachel-liste)))

;; =========

(define (spielfeld-kachel die-welt kachel-x kachel-y)
  (list-ref (welt-feld die-welt)
            (+ kachel-x (* kachel-y SPIELFELD-BREITE))))

(define (spielelemente-auf die-welt kachel-x kachel-y)
  (filter
   (lambda (das-element)
     (define pos (posn->kachel-koordinaten
                  (spielelement-position das-element)))
     (and (eq? (car pos) kachel-x)
          (eq? (cdr pos) kachel-y)))
   (welt-elemente die-welt)))

(define (feld-blockiert-nicht? die-welt feld)
  (memq feld (list 'boden 'holzboden)))

(define (element-blockiert-nicht? die-welt element)
  (memq element (list 'trank-rot 'muenze.a 'kampfstab 'brot 'trank-blau 'topf)))

(define (elemente-blockieren-nicht? die-welt elemente)
  (andmap identity (map (lambda (element) (element-blockiert-nicht? die-welt element)) elemente)))

(define (aktion-auf-möglich? die-welt aktion kachel-x kachel-y)
  (define feld-kachel
    (spielfeld-kachel die-welt kachel-x kachel-y))
  (define elemente
    (map spielelement-kachel (spielelemente-auf die-welt kachel-x kachel-y)))
  (cond
    [(eq? aktion 'gehe-zu)
     (and (feld-blockiert-nicht? die-welt feld-kachel)
          (elemente-blockieren-nicht? die-welt elemente))]
    [else #f]))
  
;; =======  Figur
(define sonnenpriester-figur
  (figur 'sonnenpriester
         (kachel-koordinaten->posn 2 1)
         (kachel-koordinaten->posn 2 1)
         0
         start-inventory))

(define (figur-zwischen-position die-figur frame)
  (if (>= frame
          (+ (figur-start-frame die-figur) ANIMATIONS-FRAMES))
      (figur-end-position die-figur)
      (zwischen-position
       (figur-start-position die-figur)
       (figur-end-position die-figur)
       (- frame (figur-start-frame die-figur)))))

(define (figur->bild die-figur der-frame)
  (scale SKALIERUNG (kachel->bild (figur-kachel die-figur) der-frame)))

; bewege die figur um kachel-delta-x horizontal (nach rechts/links)
(define (bewege-figur die-figur kachel-delta-x kachel-delta-y frame)
  (define figur-pos (figur-end-position die-figur))
  (define kachel-koordinaten
    (posn->kachel-koordinaten figur-pos))
  
  (define neue-kachel-koordinate-x
    (+ kachel-delta-x (car kachel-koordinaten)))
  (define neue-kachel-koordinate-y
    (+ kachel-delta-y (cdr kachel-koordinaten)))
  (define neue-position
    (kachel-koordinaten->posn
     neue-kachel-koordinate-x
     neue-kachel-koordinate-y))
  
  (struct-copy figur die-figur
               [start-position figur-pos]
               [end-position neue-position]
               [start-frame frame]))

;; ========= die welt

(define start-welt
  (welt 0 spielfeld spielelemente sonnenpriester-figur 'abenteuer))


(define (figur-auf-neue-position die-welt kachel-delta-x kachel-delta-y)
  (struct-copy
   welt die-welt
   [figur (bewege-figur (welt-figur die-welt) kachel-delta-x kachel-delta-y (welt-frame die-welt))]))

(define (tastatur-behandlung die-welt taste)
  (define figur-kachel-pos (posn->kachel-koordinaten (figur-end-position (welt-figur die-welt))))
  
  (define (taste-in? taste-erwartet zustand)
    (and (eq? (welt-zustand die-welt) zustand)
         (key=? taste taste-erwartet)))

  (define (aktion-möglich? aktion kachel-delta-x kachel-delta-y)
    (aktion-auf-möglich? die-welt aktion (+ (car figur-kachel-pos) kachel-delta-x) (+ (cdr figur-kachel-pos) kachel-delta-y)))
  
  (cond [(and (taste-in?  "left" 'abenteuer) (aktion-möglich? 'gehe-zu -1 0))
         (figur-auf-neue-position die-welt -1 0)]
        [(and (taste-in? "right" 'abenteuer) (aktion-möglich? 'gehe-zu 1 0))
         (figur-auf-neue-position die-welt 1 0)]
        [(and (taste-in? "up" 'abenteuer) (aktion-möglich? 'gehe-zu 0 -1))
         (figur-auf-neue-position die-welt 0 -1)]
        [(and (taste-in? "down" 'abenteuer) (aktion-möglich? 'gehe-zu 0 1))
         (figur-auf-neue-position die-welt 0 1)]
        [(and (eq? (welt-zustand die-welt) 'abenteuer) (key=? taste "i"))
         (struct-copy
          welt die-welt
          [zustand 'inventory])]
        [(and (eq? (welt-zustand die-welt) 'inventory) (key=? taste "i"))
         (struct-copy
          welt die-welt
          [zustand 'abenteuer])]
        [else die-welt]))

(define (zwischen-position start-position
                           end-position
                           frame)
  (define neue-x-position
    (+ (posn-x start-position)
       (/ (* frame
             (- (posn-x end-position)
                (posn-x start-position)))
          ANIMATIONS-FRAMES)))
  (define neue-y-position
    (+ (posn-y start-position)
       (/ (* frame
             (- (posn-y end-position)
                (posn-y start-position)))
          ANIMATIONS-FRAMES)))
  (make-posn neue-x-position neue-y-position))

(define start-szene
  (empty-scene (* SKALIERUNG SPIELFELD-BREITE KACHEL-BREITE)
               (* SKALIERUNG SPIELFELD-HÖHE KACHEL-HÖHE)
               "black"))

(define (spielfeld-szene das-spielfeld die-szene frame)
  (place-images (map (lambda (bild) (scale SKALIERUNG bild))
                     (map (lambda (kachel) (kachel->bild kachel frame)) das-spielfeld))
                (kachel->posn SPIELFELD-BREITE SPIELFELD-HÖHE)
                die-szene))

(define (draw die-welt)
  (define frame (welt-frame die-welt))
  (define die-figur (welt-figur die-welt))
  (define das-spielfeld (welt-feld die-welt))
  (define die-spielelemente (welt-elemente die-welt))
  (place-images
   (filter (lambda (el) (not (void? el)))
           (append (list (when (eq? (welt-zustand die-welt) 'inventory) (draw-inventory (figur-inventory die-figur) frame))
                         (figur->bild die-figur frame))
                   (kachel-liste->sk-bild-liste (map spielelement-kachel die-spielelemente) frame)))
   (filter (lambda (el) (not (void? el)))
           (append (list (when (eq? (welt-zustand die-welt) 'inventory) (inventory-posn))
                         (figur-position-anpassen (figur-zwischen-position die-figur frame)))
                   (map spielelement-position die-spielelemente)))
   (spielfeld-szene das-spielfeld start-szene frame)))


(define (next-frame die-welt)
  (struct-copy welt die-welt
               [frame (+ 1 (welt-frame die-welt))]))

(big-bang start-welt
  (on-tick next-frame)
  (on-key tastatur-behandlung)
  (to-draw draw))