#lang deinprogramm/sdp/beginner

;;; DATEN MIT SELBSTBEZUG
; Ein Fluss ist eins der Folgenden:
; - Bach mit Ursprungsort
; - Zusammentreffen zweier Flüsse
(define fluss
  (signature (mixed bach zufluss)))


; Ein Bach besteht aus:
; - Ursprungsort

(define-record bach
  make-bach
  bach?
  (bach-ursprung string))

(define eschach (make-bach "Heimliswald"))
(define prim (make-bach "Dreifaltigkeitsberg"))
(define schlichem (make-bach "Tieringen"))

; Ein Zufluss besteht aus
; - dem Ort des Zusammentreffens
; - Hauptfluss
; - Nebenfluss
(define-record zufluss
  make-zufluss
  zufluss?
  (zufluss-ort string)
  (zufluss-haupt fluss)
  (zufluss-neben fluss))

(define neckar (make-zufluss "Rottweil" eschach prim))
(define neckar2 (make-zufluss "Epfendorf" neckar schlichem))
(define neckar3 (make-zufluss "Ort" neckar2 prim))


;; Fließt der Fluss durch den angegeben Ort
; rein: fluss, ort
; raus: wahrheitswert
(: fließt-durch? (fluss string -> boolean))
(check-expect (fließt-durch? eschach "Heimliswald") #t)
(check-expect (fließt-durch? eschach "Rottweil") #f)
(check-expect (fließt-durch? neckar "Heimliswald") #t)
(check-expect (fließt-durch? neckar "Heimliswald") #t)
(check-expect (fließt-durch? neckar "Epfendorf") #f)
(check-expect (fließt-durch? neckar2 "Epfendorf") #t)

(define fließt-durch?
  (lambda (fluss ort)
    (cond
      ((bach? fluss) (string=? ort (bach-ursprung fluss)))
      ((zufluss? fluss)
       (or
        (string=? ort (zufluss-ort fluss))
        (fließt-durch? (zufluss-haupt fluss) ort)
        (fließt-durch? (zufluss-neben fluss) ort))))))

