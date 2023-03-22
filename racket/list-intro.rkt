#lang deinprogramm/sdp/beginner

; Eine Liste ist eins der Folgenden
; - leere Liste
; - nicht-leere Liste
; -> gemischte Daten

(define list-of-integers
  (signature (mixed empty-list cons-list)))

; Eine leere Liste ist ... nix?

(define-record empty-list
  empty
  empty?)

; Eine nicht-leere Liste besteht aus
; - einer Zahl
; - einer Liste mit restlichen Zahlen
; --> Selbstbezug
(define-record cons-list
  cons
  cons?
  (first integer)
  (rest list-of-integers))

(define lis1 (cons 3 (empty)))
(define lis2 (cons 15 (cons 7 (empty))))
(define lis3 (cons 2 lis2))
(define lis4 (cons 3 (cons 4 (cons 5 (cons 6 (empty))))))

;; Alle Elemente einer Liste aufsummieren
(: list-sum (list-of-integers -> integer))
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

(define odds2 (curry list-filter))