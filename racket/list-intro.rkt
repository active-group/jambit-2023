#lang deinprogramm/sdp/beginner

; Eine Liste ist eins der Folgenden
; - leere Liste
; - nicht-leere Liste
; -> gemischte Daten

(define list-of
  (lambda (a)
    (signature (mixed empty-list (cons-list a)))))

; Eine leere Liste ist ... nix?

(define-record empty-list
  empty
  empty?)

; Eine nicht-leere Liste besteht aus
; - einer Zahl
; - einer Liste mit restlichen Zahlen
; --> Selbstbezug
(define-record (cons-list a)
  cons
  cons?
  (first a)
  (rest (list-of a)))

(: lis1 (list-of integer))
(define lis1 (cons 3 (empty)))
(define lis2 (cons 15 (cons 7 (empty))))
(define lis3 (cons 2 lis2))
(define lis4 (cons 3 (cons 4 (cons 5 (cons 6 (empty))))))

(define list-of-integers
  (signature (list-of integer)))

;; Alle Elemente einer Liste aufsummieren
(: list-sum ((list-of integer) -> integer))
(check-expect (list-sum lis4) 18)
(check-expect (list-sum lis2) 22)
(check-expect (list-sum (empty)) 0)
(define list-sum
  (lambda (lis)
    (cond
      ((empty? lis) 0)                           ; neutrale Element der Addition
      ((cons? lis) (+ (first lis)
                      (list-sum (rest lis)))))))



;; Alle Elemente einer Liste aufmultiplizieren
(: list-mult (list-of-integers -> integer))
(check-expect (list-mult lis2) 105)
(check-expect (list-mult (empty)) 1)
(define list-mult
  (lambda (lis)
    (cond
      ((empty? lis) 1)                           ; neutrale Element der Multiplikation
      ((cons? lis) (* (first lis)
                      (list-mult (rest lis)))))))


;; Aus einer Liste die geraden Zahlen herausholen
; Hilfsfunktion: even?: (even? 6) = #t
(: evens (list-of-integers -> list-of-integers))
(check-expect (evens lis2) (empty))
(check-expect (evens lis4) (cons 4 (cons 6 (empty))))
(check-expect (evens (cons 1 (cons 2 (empty)))) (cons 2 (empty)))
(define evens
  (lambda (lis)
    (cond
      ((empty? lis) (empty))
      ((cons? lis) (if (even? (first lis))
                       (cons (first lis) (evens (rest lis)))
                       (evens (rest lis)))))))

;; Aus einer Liste die positiven Zahlen herausholen
(: positives (list-of-integers -> list-of-integers))
(check-expect (positives (cons -5 (cons 3 (empty)))) (cons 3 (empty)))
(define positives
  (lambda (lis)
    (cond
      ((empty? lis) (empty))
      ((cons? lis) (if (> (first lis) 0)
                       (cons (first lis) (positives (rest lis)))
                       (positives (rest lis)))))))

(: list-filter ((%a -> boolean) (list-of %a) -> (list-of %a)))
(define list-filter
  (lambda (predicate lis)
    (cond
      ((empty? lis) (empty))
      ((cons? lis) (if (predicate (first lis))
                       (cons (first lis) (list-filter predicate (rest lis)))
                       (list-filter predicate (rest lis)))))))

(define odds
  (lambda (lis)
    (list-filter odd? lis)))


;;; KURZE WIEDERHOLUNG


;; Lisp-Syntax, Präfix-Notation
;; 1. Datenmodellierung:
;; - zusammengesetzte Daten
;; - gemischte Daten
;; - Daten mit Selbstbezug (Rekursion)

;; 2. Funktionen und Funktionen höherer Ordnung:
;;    Funktionen, die Funktionen als Parameter erhalten, oder Funktionen zurückgeben
;; 3. Konstruktionsanleitung / systematisches Programmieren

;; Konzepte:
;; - Immutability
;; - Komposition
;; - Funktionen erster Klasse
;; - kommt noch:
;;   - erweiterte Datenmodellierung
;;   - funktionale Software-Architektur
;;   - (Makros)

;; Ein Auto besteht aus:
;; - Marke
;; - Farbe
;; - PS
;; -> zusammengesetzte Daten
(define-record auto
  make-auto
  auto?
  (auto-marke string)
  (auto-farbe string)
  (auto-ps natural))

;; Ein Fahrzeug ist eins der Folgenden:
;; - Ein Auto
;; - Ein Fahrrad
;; -> gemischte Daten

#;(define fahrzeug
  (signature (mixed auto fahrrad)))


(define long-string?
  (lambda (str)
    (> (string-length str) 3)))

;;; Schreibe Funktion, die aus Liste von Strings alle entfernt, die kürzer als 4 Buchstaben sind
; Hilfsfunktion: string-length

(: remove-short-words ((list-of string) -> (list-of string)))
(check-expect (remove-short-words (cons "Hallo" (cons "du" (empty)))) (cons "Hallo" (empty)))
(define remove-short-words
  (lambda (words)
    (list-filter long-string? words)))



  