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

(: list-fold (%b (%a %b -> %b) (list-of %a) -> %b))
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

(define make-greet
  (lambda (gruß)
    (lambda (name)
      (string-append gruß name))))

(define sag-hallo (make-greet "Hallo "))



;; Implentiere list-map mit list-fold
(: list-map-2 ((%a -> %b) (list-of %a) -> (list-of %b)))
(check-expect (list-map-2 inc (list 1 2 3)) (list 2 3 4))
(define list-map-2
  (lambda (f lis)
    (list-fold empty
               (lambda (x result)
                 (cons (f x)
                       result))
               lis)))

;; Implementiere list-filter mit list-fold
(: list-filter-2 ((%a -> boolean) (list-of %a) -> (list-of %a)))
(check-expect (list-filter-2 even? (list 1 2 3 4)) (list 2 4))
(define list-filter-2
  (lambda (p lis)
    (list-fold empty
               (lambda (x result)
                 (if (p x)
                     (cons x result)
                     result))
               lis)))

;;; Schreibe eine Funktion, die die Gewichte von Dillos aufaddiert
(: sum-dillo-weights ((list-of dillo) -> natural))
(check-expect (sum-dillo-weights (list dillo1 dillo2)) 35000)
#;(define sum-dillo-weights
  (lambda (dillos)
    (list-fold 0
               (lambda (dillo result)
                 (+ (dillo-weight dillo)
                    result))
               dillos)))

(define sum-dillo-weights
  (lambda (dillos)
    (list-fold 0
               +
               (list-map dillo-weight dillos))))


;; ... MapReduce-Algorithmus von Google / Hadoop...
;; -----

;;; ÜBUNG Zwei Listen aneinander hängen (ohne append)
(: concat ((list-of %a) (list-of %a) -> (list-of %a)))
(check-expect (concat (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
(define concat
  (lambda (lis1 lis2)
    (list-fold lis2
               (lambda (x result)
                 (cons x result))
               lis1)))
#;(define concat
  (lambda (lis1 lis2)
    (cond
      ((empty? lis1) lis2)
      ((cons? lis1) (cons (first lis1)
                          (concat (rest lis1) lis2))))))

;; Interleave zweier Listen
(: interleave ((list-of %a) (list-of %a) -> (list-of %a)))
(check-expect (interleave (list 1 2 3) (list 100 99 98)) (list 1 100 2 99 3 98))
(check-expect (interleave (list 1 2 3 4 5) (list 9 9)) (list 1 9 2 9 3 4 5))
(check-expect (interleave (list 1 2) (list 9 9 9 9)) (list 1 9 2 9 9 9))
#;(define interleave
  (lambda (lis1 lis2)
    (cond
      ((empty? lis1) lis2)
      ((empty? lis2) lis1)
      (else
       (concat
        (list (first lis1)
              (first lis2))
        (interleave (rest lis1) (rest lis2)))))))

(define interleave
  (lambda (lis1 lis2)
    (cond
      ((empty? lis1) lis2)
      ((cons? lis1)
       (cons (first lis1)
             (interleave lis2 (rest lis1)))))))

;;; Länge einer Liste bestimmen!
(: list-length ((list-of %a) -> natural))
(check-expect (list-length (list 1 2 3 4)) 4)
(check-expect (list-length empty) 0)
#;(define list-length
  (lambda (lis)
    (cond
      ((empty? lis) 0)
      ((cons? lis)
       (+ 1 (list-length (rest lis)))))))

(define list-length
  (lambda (lis)
    (list-fold 0
               (lambda (a b) (+ b 1))
               lis)))

;; map mit Akkumulator
(define map-2
  (lambda (f lis res)
    (cond
      ((empty? lis) res)
      ((cons? lis)
       (map-2 f (rest lis) (concat res
                                 (list (f (first lis)))))))))

(define map-22
  (lambda (f lis)
    (map-2 f lis empty)))

;; list-sum-a mit Akkumulator implementieren
(: list-sum-a ((list-of number) number -> number))
(check-expect (list-sum-a (list 1 2 3) 0) 6)
(define list-sum-a
  (lambda (lis res)
    (cond
      ((empty? lis) res)
      ((cons? lis)
       (list-sum-a (rest lis) (+ (first lis) res))))))

;;; Liste umdrehen
; vllt. Hilfsfunktion hilfreich: (add-to-list 5 (list 1 2 3 4)) -> (list 1 2 3 4 5)
; auch möglich: mit append
(: rev ((list-of %a) -> (list-of %a)))
(check-expect (rev (list 1 2 3)) (list 3 2 1))
(define rev
  (lambda (lis)
    (cond
      ((empty? lis) empty)
      ((cons? lis)
       (concat (rev (rest lis))
               (list (first lis)))))))

;; rev mit Akku
(: rev-a ((list-of %a) (list-of %a) -> (list-of %a)))
(check-expect (rev-a (list 1 2 3) empty) (list 3 2 1))
(define rev-a
  (lambda (lis acc)
    (cond
      ((empty? lis) acc)
      ((cons? lis)
       (rev-a (rest lis) (append (list (first lis)) acc))))))


(define reverse-2
  (lambda (lis)
    (rev-a lis empty)))


;;; Rekursion über natürliche Zahlen
; eine natürliche Zahl ist eins der folgenden
; - 0
; - der Nachfolger einer natürlichen Zahl

;; Fakultät einer Zahl n berechnen
(: factorial (natural -> natural))
(check-expect (factorial 5) 120)
(check-expect (factorial 3) 6)
(define factorial
  (lambda (n)
    (cond
      ((= n 0) 1)
      ((> n 0) (* n (factorial (dec n)))))))

;; Fibonacci-Zahlen
;; Fib(n) = Fib(n-1) + F(n-2)
;; Fib(1), Fib(0)

(: fib (natural -> natural))
(check-expect (fib 0) 1)
(check-expect (fib 1) 1)
(check-expect (fib 2) 2)
(check-expect (fib 3) 3)
(check-expect (fib 4) 5)
(check-expect (fib 5) 8)
(define fib
  (lambda (n)
    (cond
      ((= n 0) 1)
      ((= n 1) 1)
      ((> n 1) (+ (fib (- n 1)) (fib (- n 2)))))))


;; n-tes Element einer Liste herausholen
(: nth (natural (list-of %a) -> %a))
(check-expect (nth 2 (list 9 8 7 6)) 7)
(check-expect (nth 1 (list "h" "a" "l" "l" "o")) "a")
(check-error (nth 5 (list 1 2)) "Index out of bounds")
(define nth
  (lambda (n lis)
    (cond
      ((empty? lis) (violation "Index out of bounds"))
      ((= n 0) (first lis))
      ((> n 0) (nth (- n 1) (rest lis))))))


;; PROPERTY-BASED TESTING

; quicktest -> testet auf Eigenschaften mit zufällig generierten Daten

(check-property
 (for-all ((a natural)
           (b natural)
           (c natural))
   (= (+ a (+ b c))
      (+ (+ a b) c))))

;; Teste Operatoren auf Kommutativität: (a + b) = (b + a)
;; Teste eine Listeneigenschaft!

; Reverse Reverse List = List
(check-property
 (for-all ((lis (list-of natural)))
   (expect lis
           (reverse (reverse lis)))))

(check-property
 (for-all ((lis (list-of natural)))
   (or (empty? lis) 
       (< (length (rest lis))
          (length lis)))))

(check-property
 (for-all ((a natural)
           (b natural)
           (c natural))
   (if (and (> a b)
            (> b c))
       (> a c)
       #t)))










