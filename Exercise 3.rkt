(require 2htdp/image)

; not needed to implement your functions
; but very helpful for testing
(require "./iterated_images.rkt")

; Part 1: Ordinary Recursion

; FILL IN FUNCTIONS HERE
; function names are given but you
; MUST add signatures and purpose statements

; multiply-list : (listof number) -> number
; compute the product of a list of numbers
; (multiple-list ()) => 1

(define multiply-list (λ (lon) (cond [(equal? lon '()) 1]
                                     [else (* (first lon) (multiply-list (rest lon)))])))
(check-expect (multiply-list '()) 1)
(check-expect (multiply-list '(1)) 1)
(check-expect (multiply-list '(1 2)) 2)
(check-expect (multiply-list '(1 2 3 4)) 24)


; my-iterated-overlay
; my-iterated-overlay : (number -> image), number -> image
; to behave exactly like iterated-overlay, i.e. to repeatedly overly images on top of eachother in a row with one procedure.
(define my-iterated-overlay (λ (proc n) (cond [(= n 0) empty-image]
                                              [else (overlay (my-iterated-overlay proc (- n 1))
                                                             (proc (- n 1)))])))
(define (f num) (rotate (* num 72) (ellipse 100 25 "solid" (color (* 25 num) (- 255 (* 25 num)) 0))))
(check-expect (my-iterated-overlay f 5) .)


; iterated-any
; iterated-any : (image, image -> image), (number -> image), number -> image
; to take in an arbitrary combiner and behave exactly like the iterated version of it, i.e to take a combiner generater and count and iterate the procuder of the combiner on the generator for the count amount of times
(define iterated-any (λ (comb gen n) (cond [(= n 0) empty-image]
                                              [else (comb (iterated-any comb gen (- n 1))
                                                             (gen (- n 1)))])))
(check-expect (iterated-any beside h 7) .)
(define (h num) (rectangle (* (+ 1 num) 10) 50 "outline" "black"))


; Interlude BinaryTrees
; tree struct definitions
(define-struct branch [val left right])

; here's a few example trees, define your own for more testing
(define just-leaf 1)
(define just-branch (make-branch 1 2 -1))
(define test-tree-one
  (make-branch 1
               (make-branch 2
                            1
                            (make-branch 1 12 0))
               (make-branch 1 3 7)))
(define test-tree-two
  (make-branch 1
               0
               (make-branch 2
                            0
                            (make-branch 3 0 0))))

             
; count-tree : BinaryTree -> number
;to count the number of numbers in a binary tree
(define count-tree (λ (tree) (cond [(number? tree) 1]
                                   [else (+ 1 (count-tree (branch-left tree))
                                            (count-tree (branch-right tree)))])))
(check-expect (count-tree test-tree-one) 9)
(check-expect (count-tree test-tree-two) 7)

; sum-tree : BinaryTree -> number
; to sum the number of numbers in a binary tree
(define sum-tree (λ (tree) (cond [(number? tree) tree]
                                   [else (+ (branch-val tree) (sum-tree (branch-left tree))
                                            (sum-tree (branch-right tree)))])))
(check-expect (sum-tree 0) 0)
(check-expect (sum-tree test-tree-one) 28)
(check-expect (sum-tree test-tree-two) 6)

; max-tree
(define max-tree (λ (tree) (cond [(number? tree) tree]
                                 [else (max (branch-val tree) (max-tree (branch-left tree))
                                            (max-tree (branch-right tree)))])))
(check-expect (max-tree 3) 3)
(check-expect (max-tree test-tree-two) 3)
(check-expect (max-tree test-tree-one) 12)

; depth-tree : BinaryTree -> number
; to return the number of levels of nesting in a binary tree
(define depth-tree (λ (tree) (cond [(number? tree) 0]
                                    [else  (+ 1 (max (depth-tree (branch-left tree))
                                            (depth-tree (branch-right tree))))])))
(check-expect (depth-tree test-tree-one) 3)
(check-expect (depth-tree test-tree-two) 3)

; Part 2: Iterative Recursion

; FILL IN FUNCTIONS HERE

; multiply-list/iter : (listof number) -> number
; to act identically to multiply-list but with iterated recursion instead of just recursion
(define multiply-list/iter (λ (lon) (local [(define
                                     help (λ (lon msf) (cond [(empty? lon) msf]
                                                              [else (help (rest lon) (* msf (first lon)))])))]
                                      (help lon 1))))

(check-expect (multiply-list/iter '()) 1)
(check-expect (multiply-list/iter '(1)) 1)
(check-expect (multiply-list/iter '(1 2)) 2)
(check-expect (multiply-list/iter '(1 2 3 4)) 24)
; iterated-overlay/iter : (number -> image), number -> image
; to act identically to my-iterated-overlay/iter but with iterated recursion instead of just recursion
(define iterated-overlay/iter (λ (proc n) (local [(define
                                                help (λ (proc n psf) (cond [(= n 0) psf]
                                                                           [else (help proc (- n 1) (overlay (proc (- n 1)) psf))])))]
                                (help proc n empty-image))))
(check-expect (iterated-overlay/iter f 5) .)

; iterated-any/iter : (image, image -> image), (number -> image), number -> image
; to act identically to iterated-any but with iterated recursion instead of just recursion

(define iterated-any/iter (λ (comb gen n)
                            (local [(define
                                      help (λ (comb gen n fsf)
                                             (cond [(= n 0) fsf]
                                                   [else (help comb gen (- n 1) (comb (gen (- n 1)) fsf))])))]
                              (help comb gen n empty-image))))

(check-expect (iterated-any/iter beside h 7) .)
(check-expect (iterated-any/iter overlay f 5) .)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     We are providing fewer and fewer tests!
;;     Come up with your own tests carefully following the PDF.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-expect (multiply-list '()) 1)

;; square-gen : number -> image
(define (square---gen n)
  (square (* n 10)
          "solid"
          (color (* n 50)
                 0 0)))
(check-expect (my-iterated-overlay square---gen 3)
              (overlay (square---gen 0)
                       (square---gen 1)
                       (square---gen 2)))

(check-expect (my-iterated-overlay square---gen 5)
              (iterated-overlay square---gen 5))

(check-expect (count-tree -10) 1)
(check-expect (count-tree just-branch) 3)

(check-expect (sum-tree (make-branch 1
                                     (make-branch 0 -3 1)
                                     (make-branch 2
                                                  0
                                                  (make-branch 3 4 0))))
              8)

(check-expect (depth-tree test-tree-one) 3)

(check-expect (procedure? multiply-list) #true)
(check-expect (procedure? my-iterated-overlay) #true)
(check-expect (procedure? iterated-any) #true)
(check-expect (procedure? count-tree) #true)
(check-expect (procedure? sum-tree) #true)
(check-expect (procedure? max-tree) #true)
(check-expect (procedure? depth-tree) #true)
(check-expect (procedure? multiply-list/iter) #true)
(check-expect (procedure? iterated-overlay/iter) #true)
(check-expect (procedure? iterated-any/iter) #true)
