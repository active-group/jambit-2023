#lang deinprogramm/sdp/beginner
;;; Duschprodukte
; Es gibt Seife, Shampoo, Duschgel, Mixtur
; Seife besteht aus:
; - pH-Wert
; Shampoo besteht aus:
; - Farbe
; - Haartyp
; Duschgel besteht zu gleichen Teilen aus Shampoo und Seife
; Mixtur:
; besteht aus zwei Duschprodukten, mit prozentualem Anteil

(define product
  (signature (mixed soap shampoo gel mixture)))

(define-record soap
  make-soap
  soap?
  (soap-ph rational))

(define soap1 (make-soap 5.7))
(define soap2 (make-soap 6.2))

(define-record shampoo
  make-shampoo
  shampoo?
  (shampoo-color string)
  (shampoo-type string))


(define shampoo1 (make-shampoo "brown" "dry"))
(define shampoo2 (make-shampoo "blonde" "dandruff"))

(define-record gel
  make-gel
  gel?
  (gel-soap soap)
  (gel-shampoo shampoo))

(define gel1 (make-gel soap1 shampoo1))
(define gel2 (make-gel soap2 shampoo2))

(define-record mixture
  make-mixture
  mixture?
  (mixture-product-1 product)
  (mixture-ratio-1 rational)
  (mixture-product-2 product))

(define mixture-ratio-2
  (lambda (mix)
    (- 1 (mixture-ratio-1 mix))))

(define mix1 (make-mixture gel1 0.4 shampoo1))
(define mix2 (make-mixture mix1 0.2 gel2))

; Seifenanteil eines Produkts berechnen
(: product-soap-ratio (product -> rational))
(check-expect (product-soap-ratio mix1) 0.2)
(define product-soap-ratio
  (lambda (prod)
    (cond
      ((soap? prod) 1)
      ((shampoo? prod) 0)
      ((gel? prod) 0.5)
      ((mixture? prod)
       (+ (* (mixture-ratio-1 prod)
             (product-soap-ratio (mixture-product-1 prod)))
          (* (mixture-ratio-2 prod)
             (product-soap-ratio (mixture-product-2 prod))))))))








