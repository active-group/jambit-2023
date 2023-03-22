#lang deinprogramm/sdp/beginner

; Ein Gürteltier hat folgende Eigenschaften
; - lebendig oder tot
; - hat Gewicht in g
; -> zusammengesetzte Daten

(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight natural))

(define dillo1 (make-dillo #t 20000)) ; lebendig, 20 kg
(define dillo2 (make-dillo #f 15000)) ; tot, 15 kg

; Überfahre ein Gürteltier
; rein: Gürteltier
; raus: Gürteltier
(: run-over-dillo (dillo -> dillo))
(check-expect (run-over-dillo dillo1) (make-dillo #f 20000))
(check-expect (run-over-dillo dillo2) dillo2)
(define run-over-dillo
  (lambda (d)
    (make-dillo #f (dillo-weight d))))

#;(define run-over-dillo
  (lambda (d)
    (if (dillo-alive? d)
        (make-dillo #f (dillo-weight d))
        d)))

;;; ÜBUNG weiteres Tier
; Ein Papagei hat folgende Eigenschaften
; - Gewicht in g
; - Einen Satz, den er sagt
(define-record parrot
  make-parrot
  (parrot-weight natural)
  (parrot-sentence string))

(define parrot1 (make-parrot 500 "Hallo"))    ; lebendig
(define parrot2 (make-parrot 750 "Tschüss"))  ; lebendig
(define parrot3 (make-parrot 450 ""))         ; tot

; Ist ein Papagei lebendig?
(: parrot-alive? (parrot -> boolean))
(check-expect (parrot-alive? parrot1) #t)
(check-expect (parrot-alive? parrot3) #f)
(define parrot-alive?
  (lambda (p)
    (not (string=? (parrot-sentence p) ""))))

