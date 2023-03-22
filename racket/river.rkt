#lang deinprogramm/sdp/beginner

;;; DATEN MIT SELBSTBEZUG
; Ein Fluss ist eins der Folgenden:
; - Bach mit Ursprungsort
; - Zusammentreffen zweier Flüsse

; Ein Bach besteht aus:
; - Ursprungsort

(define-record bach
  make-bach
  bach?
  (bach-ursprung string))

; Ein zusammentreffen besteht aus
; - dem Ort des Zusammentreffens
; - Hauptfluss
; - Nebenfluss
(define-record zusammentreffen
  make-zusammentreffen
  zusammentreffen?
  (zusammentreffen-ort


