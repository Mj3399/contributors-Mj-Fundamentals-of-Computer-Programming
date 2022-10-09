;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise_0) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Write your code here!
(require 2htdp/image)
(define a-red-square (square 100 "solid" "red"))
(define a-blue-circle (circle 50 "solid" "blue"))
(define outlined-square (square 100 "outline" "red"))
(define outlined-circle (circle 50 "outline" "blue"))
(define row-of-squares (beside (square 100 "solid" "red")
                               (square 100 "solid" "blue")
                               (square 100 "solid" "green")))
(define column-of-squares (above(square 100 "solid" "red")
                               (square 100 "solid" "blue")
                               (square 100 "solid" "green"))) 
(define nested-squares (overlay
                                (square 25 "solid" "black")
                                (square 50 "solid" "green")
                                (square 75 "solid" "blue")
                                (square 100 "solid" "red")))
(define rotated-squares (overlay
                         (rotate 45 (square 25 "solid" "black"))
                         (rotate 45 (square 50 "solid" "green"))
                         (rotate 45 (square 75 "solid" "blue"))
                         (rotate 45 (square 100 "solid" "red"))))
(define flag-of-chicago (overlay/xy
(overlay/xy (rectangle 475 50 "solid" "light blue") 100 60
(overlay/xy
(overlay/xy
(overlay/xy (radial-star 6 13 30 "solid" "red") 75 0
            (radial-star 6 13 30 "solid" "red")) 75 0
(overlay/xy (radial-star 6 13 30 "solid" "red") 75 0
            (radial-star 6 13 30 "solid" "red"))) 75 0
(overlay/xy
(overlay/xy (radial-star 6 13 30 "solid" "red") 75 0
            (radial-star 6 13 30 "solid" "red")) 75 0
(overlay/xy (radial-star 6 13 30 "solid" "red") 75 0
            (radial-star 6 13 30 "solid" "red"))))) 0 125
            (rectangle 475 50 "solid" "light blue")))



;; DON'T CHANGE THESE!!!
;; These are just tests to make sure all of your images are named correctly for grading
(check-expect (image? a-red-square) #t)
(check-expect (image? a-blue-circle) #t)
(check-expect (image? outlined-square) #t)
(check-expect (image? outlined-circle) #t)
(check-expect (image? row-of-squares) #t)
(check-expect (image? column-of-squares) #t)
(check-expect (image? nested-squares) #t)
(check-expect (image? rotated-squares) #t)
(check-expect (image? flag-of-chicago) #t)