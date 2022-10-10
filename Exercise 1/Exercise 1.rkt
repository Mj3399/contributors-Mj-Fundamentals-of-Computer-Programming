(require 2htdp/image)
(require "./iterated-images.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Question 1: Using iterated-overlay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part (a): using overlay
(define question-1a
 (overlay (circle 10 "outline" "black")
          (circle 20 "outline" "black")
          (circle 30 "outline" "black")
          (circle 40 "outline" "black")
          (circle 50 "outline" "black")))

(check-expect question-1a .)

;; Part (b): using iterated-overlay
; first define g:
(define (g num)
  (circle (* (+ 1 num) 10) "outline" "black"))

(define question-1b
  (iterated-overlay g 5))

(check-expect question-1b .)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Question 2: Using iterated-beside
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;define h
(define (h num) (rectangle (* (+ 1 num) 10) 50 "outline" "black"))
(define question-2
  (iterated-beside h 7))

(check-expect question-2 .)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Question 3: A Simple Flower
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (l num) (rotate (* num 72) (ellipse 100 25 "solid" "blue")))

(define question-3
 (iterated-overlay l 5))

(check-expect question-3 .)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Question 4: A Colorful Flower
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (f num) (rotate (* num 72) (ellipse 100 25 "solid" (color (* 25 num) (- 255 (* 25 num)) 0))))

(define question-4
  (iterated-overlay f 5))

(check-expect question-4 .)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Question 5: A Fancy Flower
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;just to save the red blue mix (interpolate-colors (color 255 0 0) (color 0 0 255) fraction)
;(define (k num) (rotate (* num 72) (ellipse 100 25 "solid" (color (* 25 num) (- 255 (* 25 num)) 0))))
(define (r num) (rotate (* num 72) (ellipse 100 25 "solid" (interpolate-colors (color 0 0 255 100) (color 255 0 0 100) (* .25 num)))))
(define question-5
  (iterated-overlay r 5))

(check-expect question-5 .)

;; Use this to test your colors.
;; Remember, your final answer must pass the previous test, so you should
;; only use this image as a temporary aid.

;; (define q5-colors .)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Question 6: Paint Chips
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; purpose: return a row of n (given by the number argument)
;; 50x50 squares where the colors of the squares interpolate
;; between the two color arguments
;; signature: swatch : color, color, number -> image
(define swatch
  (λ (color1 color2 num-squares)
    (iterated-beside (λ (n) (square 50 "solid" (interpolate-colors color1 color2 (/ n (- num-squares 1))))) num-squares)))

(check-expect (swatch (color 86 180 233) (color 213 94 0) 5)
              .)
(check-expect (swatch (color 0 0 0) (color 255 255 255) 2)
              .)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Question 7: Swatch Grids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; purpose: return a grid with the given number of rows and columns
;; where each row interpolates between the two color arguments and
;; each column interpolates between the color of the top square and
;; black (color 0 0 0) (see the pdf for more detail if this is confusing)
;; NOTE: you MUST use your swatch function from the previous step
;; signature: swatch-grid : color, color, number, number -> image
(define swatch-grid
  (λ (color1 color2 num-rows num-cols)
    (iterated-above (λ (n) (swatch (interpolate-colors color1 (color 0 0 0) (/ n num-rows))
                                   (interpolate-colors color2 (color 0 0 0) (/ n num-rows)) num-cols))
                    num-rows)))

(check-expect (swatch-grid (color 86 180 233) (color 213 94 0) 5 5)
              .)
(check-expect (swatch-grid (color 204 121 167) (color 0 158 115) 4 3)
              .)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Question 8: Bullseye Revisited
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; purpose: to return an image of a bullseye with the given number of rings that are the given color, and the final circle should have the given radius.
;; signature: bullseye/simple : num-rings radius color --> image
(define bullseye/simple
  (λ (num-rings radius clr)
    (iterated-overlay (λ (n) (circle (* (+ 1 n) (/ radius num-rings)) "outline" clr)) num-rings)))

(check-expect (bullseye/simple 5 50 (color 0 0 0)) question-1b)
(check-expect (bullseye/simple 5 50 (color 0 0 0)) question-1a)

; You must turn these images into test cases, using the information above the image

; 20 rings, radius of 100, line color - (color 86 180 233)
(check-expect (bullseye/simple 20 100 (color 86 180 233)).)

; 3 rings, radius of 75, line color - (color 204 121 167)
(check-expect (bullseye/simple 3 75 (color 204 121 167)) .)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Question 9: Colorful Bullseye
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; purpose: to return an image of a bullseye with the given number of rings, with the outermost circle having the given radius, the innermost ring having the colour given
;; by the third argument, the outermost ring having the color given by the fourth argument, and the intermediate rings interpolated evenly between the two colors.
;; signature: bullseye/color : num-rings radius inner-color outer-color --> image
(define bullseye/color
  (λ (num-rings radius inner-color outer-color)
    (iterated-overlay (λ (n) (circle (* (+ 1 n) (/ radius num-rings)) "solid" (interpolate-colors inner-color outer-color (/ n (- num-rings 1))))) num-rings)))

; turn the below images into test cases
; NOTE THE ORDER OF THE ARGUMENTS!
; 6 rings, radius 50, inner color (color 0 158 115), outer color (color 204 121 167)
(check-expect (bullseye/color 6 50 (color 0 158 115) (color 204 121 167)) .)

; 4 rings, radius 75, inner color (color 213 94 0), outer color (color 86 180 233)
(check-expect (bullseye/color 4 75 (color 213 94 0) (color 86 180 233)) .)
