#lang deinprogramm/sdp

; Ein Gürteltier hat folgende Eigenschaften
; - lebendig oder tot
; - hat Gewicht in g
; -> zusammengesetzte Daten

(define-record dillo
  make-dillo
  dillo?
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
  parrot?
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

; Überfahre einen Papagei
(: run-over-parrot (parrot -> parrot))
(check-expect (run-over-parrot parrot1) (make-parrot 500 ""))
(check-expect (run-over-parrot parrot3) parrot3)
(define run-over-parrot
  (lambda (parrot)
    (make-parrot (parrot-weight parrot) "")))


;;; GEMISCHTE DATEN
; Ein Tier ist eins der folgenden:
; - Dillo
; - Papagei
; "ist eins der folgenden" -> gemischten Daten

#|
class Dillo implements IRunOverAble {
  bool alive;
  integer weight;

  void runOver(){
    this.alive = false;
  }
}

interface IRunOverAble {
  void runOver();
}

|#

(define animal
  (signature (mixed parrot dillo)))

; Überfahre ein Tier
(: run-over-animal (animal -> animal))
(check-expect (run-over-animal parrot1) (make-parrot 500 ""))
(check-expect (run-over-animal dillo1) (run-over-dillo dillo1))
(define run-over-animal
  (lambda (animal)
    (cond
      ((parrot? animal) (run-over-parrot animal))
      ((dillo? animal) (run-over-dillo animal)))))

;; Füttere ein Tier, Futter in g
; rein: Tier, Futter in Gramm
; raus: Tier
(: feed-animal (animal natural -> animal))
(check-expect (feed-animal dillo1 1000) (make-dillo #t 21000))
(check-expect (feed-animal dillo2 1000) dillo2)
(check-expect (feed-animal parrot1 50) (make-parrot 550 (parrot-sentence parrot1)))
(define feed-animal
  (lambda (animal food)
    (cond
      ((dillo? animal)
       (if (dillo-alive? animal)
           (make-dillo #t (+ (dillo-weight animal) food))
           animal))
       ((parrot? animal)
        (if (parrot-alive? animal)
            (make-parrot (+ food (parrot-weight animal))
                         (parrot-sentence animal))
            animal)))))

;;; ÜBUNG: Implementiere Brüche
; Multiplikation zweier Brüche
(define-record bruch
  really-make-bruch
  bruch?
  (bruch-zähler integer)
  (bruch-nenner natural))

(define make-bruch
  (lambda (z n)
    (if (= n 0)
        (violation "Nenner ist 0")
        (really-make-bruch z n))))

; Multipliziere zwei Brüche
; rein: Bruch 1 und Bruch 2
; raus: Bruch
(: mult (bruch bruch -> bruch))
(check-expect (mult (make-bruch 3 4) (make-bruch 2 3)) (make-bruch 6 12))
(define mult
  (lambda (bruch1 bruch2)
    (define zähler1 (bruch-zähler bruch1))
    (define nenner1 (bruch-nenner bruch1))
    (define zähler2 (bruch-zähler bruch2))
    (define nenner2 (bruch-nenner bruch2))
    (make-bruch (* zähler1 zähler2)
                (* nenner1 nenner2))))


;;; Liste von dillos
(define dlist (list dillo1 dillo2))
;;; Liste von Papageien
(define plist (list parrot1 parrot2))
; Liste von Tieren
(define alist (list dillo1 parrot1 dillo2 parrot2))

;;; Alle Tiere einer Liste überfahren
; Rein: Liste von Tieren
; Raus: Liste von Tieren
(: run-over-animals ((list-of animal) -> (list-of animal)))
(check-expect (run-over-animals dlist) (list (make-dillo #f 20000) dillo2))
(check-expect (run-over-animals plist) (list (run-over-animal parrot1)
                                             (run-over-animal parrot2)))
(define run-over-animals
  (lambda (lis)
    (cond
      ((empty? lis) empty)
      ((cons? lis) (cons (run-over-animal (first lis))
                         (run-over-animals (rest lis)))))))



; ÜBUNG: An jeden String einer String-Liste "!" anfügen
; Hilfsfunktion (string-append "Hallo " "du") -> "Hallo du"

; ÜBUNG: Jedes Element einer Integer-Liste um 1 erhöhen
(: inc-list ((list-of integer) -> (list-of integer)))
(check-expect (inc-list (list 1 2 3 4 5)) (list 2 3 4 5 6))
(define inc-list
  (lambda (lis)
    (cond
      ((empty? lis) empty)
      ((cons? lis) (cons (inc (first lis))
                         (inc-list (rest lis)))))))

(define inc
  (lambda (x)
    (+ x 1)))

(: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))
(define list-map
  (lambda (f lis)
    (cond
      ((empty? lis) empty)
      ((cons? lis) (cons (f (first lis))
                         (list-map f (rest lis)))))))

;; Jedes Element um 1 erniedrigen
(check-expect (dec-list (list 1 2 3)) (list 0 1 2))
(define dec-list
  (lambda (lis)
    (list-map dec lis)))

(define dec
  (lambda (x) (- x 1)))


(define lis2 (list 15 7))
(define lis4 (list 3 4 5 6))


;; Alle Elemente einer Liste aufsummieren
(: list-sum ((list-of integer) -> integer))
(check-expect (list-sum lis4) 18)
(check-expect (list-sum lis2) 22)
(check-expect (list-sum empty) 0)
(define list-sum
  (lambda (lis)
    (cond
      ((empty? lis) 0)                           ; neutrale Element der Addition
      ((cons? lis) (+ (first lis)
                      (list-sum (rest lis)))))))


(define list-mult
  (lambda (lis)
    (cond
      ((empty? lis) 1)                           ; neutrale Element der Multiplikation
      ((cons? lis) (* (first lis)
                      (list-mult (rest lis)))))))

(: list-fold (%b (%a %b -> %b) (list-of % a) -> %b))
(check-expect (list-fold 0 + (list 1 2 3)) 6)
(define list-fold
  (lambda (e op lis)
    (cond
      ((empty? lis) e)                           ; initiales Element
      ((cons? lis) (op (first lis)
                       (list-fold e op (rest lis)))))))


; Liste aufmultiplizieren, benutze "list-fold"
(check-expect (list-mult-2 (list 1 2 3)) (list-mult (list 1 2 3)))
(define list-mult-2
  (lambda (lis)
    (list-fold 1 * lis)))

;; list-sum-2
(check-expect (list-sum-2 (list 1 2 3)) 6)
(define list-sum-2
  (lambda (lis)
    (list-fold 0 + lis)))



;; Implentiere list-map mit list-fold
(: list-map-2 ((%a -> %b) (list-of %a) -> (list-of %b)))
(check-expect (list-map-2 inc (list 1 2 3)) (list 2 3 4))
(define list-map-2
  (lambda (...)
    (list-fold ... ... ...)))







