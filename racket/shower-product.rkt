#lang deinprogramm/sdp/beginner
;;; Duschprodukte
; Es gibt Seife, Shampoo, Duschgel, Mixtur
; Seife besteht aus:
; - pH-Wert
; Shampoo besteht aus:
; - Farbe Haartyp
; Duschgel besteht zu gleichen Teilen aus Shampoo und Seife
; Mixtur:
; besteht aus zwei Duschprodukten, mit prozentualem Anteil

(define product
  (signature (mixed soap shampoo gel mixture)))

