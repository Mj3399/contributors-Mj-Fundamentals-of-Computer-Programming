;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./remove_duplicates.rkt")

; This defines the basic album datatype.
(define-struct album (title artist genre))
; define-struct automatically creates the following functions for you:
;
; `make-<struct-name>` (in this case `make-album`)
;   a function to create an instance of the struct
;   this function takes arguments for each of the fields listed, so for example
;   (make-struct 'Sway' 'Tove Styrke' 'Pop') will create an album struct
;    with title 'Sway', artist 'Tove Styrke' & genre 'Pop
;
; `<struct-name>-<field-name>` (for each field)
;    functions for accessing values of each field in the struct
;    for album this would mean we'd have the following functions:
;    `album-title`, `album-artist`, `album-genre`
;    the following examples creates an album and then accesses its fields
;    ```
;    (define sway (make-album 'Sway' 'Tove Styrke' 'Pop')
;    (album-title sway) ; returns 'Sway'
;    (album-artist sway) ; returns 'Tove Styrke'
;    (album-genre sway) ; returns 'Pop'
;    ```
;
; `<struct-name>?` (in this case `album?`)
;   a predicate (function which returns a boolean) that checks a value and
;   returns true if it's an instance of the struct, false otherwise
;   using the `sway` album defined in the previous example
;   ```
;   (album? sway) ; returns true
;   (album? 1) ; returns false
;   (album? 'hi') ; returns false
;   ```

;;; Enter a list of albums below
;;; They need not be the actual albums you own.
;;; But you should include enough variety to adequately
;;; test your code.
;;;
;;; Here's what we mean.  One of the questions involves
;;; writing a procedure that finds all the albums of a
;;; given genre.  If all the albums in the library are
;;; in the rock genre, then there's only one genre and
;;; when you ask for all the rock albums and it gives
;;; back all the albums, you don't know whether that's
;;; because the code really works, or because it's
;;; not even paying attention to the genre.  So you want
;;; to make sure there are multiple artists and genres,
;;; some artists with only one album or genre, others
;;; with multiple artists or genres, etc.

(define testing-library-1
  (list (make-album "Frozen" "Kristen Anderson-Lopez" "Musicals")
        (make-album "Tangled-C" "Mandy Moore" "TV")
        (make-album "Tangled-M" "Mandy Moore" "Movie")
        (make-album "Charming" "Amber Gray" "Musicals")))

;;; Add the procedures you write (e.g. all-genres, versatile-artists)
;;; below.  Be sure to test your procedures to make sure they work.
;;; We only provide very few test cases this time, so you need
;;; to write your own test cases to make sure the code works.
;;; We will use our own test cases when grading and assign you
;;; a grade based on the number of test cases that passed.


;; all-titles : (listof album) -> (listof string)
(define all-titles
  (λ (lib)
    (map album-title lib)))
(check-expect (all-titles testing-library-1) (list "Frozen" "Tangled-C" "Tangled-M" "Charming"))


;; all-artists: (listof album) -> (listof string)
(define all-artists (λ (lib) (remove-duplicates (map album-artist lib))))
(check-expect (all-artists testing-library-1)
                  (list  "Kristen Anderson-Lopez" "Mandy Moore" "Amber Gray"))

;; all-genres: (listof album) -> (listof string)

(define all-genres (λ (lib) (remove-duplicates (map album-genre lib))))
(check-expect (all-genres testing-library-1) (list "Musicals" "TV" "Movie"))

;; artist-albums : string, (listof album) -> (listof album)

(define artist-albums (λ (desired-artist lib)
                        (filter (λ (album) (string=? (album-artist album) desired-artist)) lib)))
(check-expect (artist-albums "Mandy Moore" testing-library-1)
               (list (make-album "Tangled-C" "Mandy Moore" "TV") (make-album "Tangled-M" "Mandy Moore" "Movie")))

;; artist-genres: string, (listof album) -> (listof string)
(define artist-genres (λ (desired-artist lib) (remove-duplicates (map album-genre (artist-albums desired-artist lib)))))

(check-expect (artist-genres "Mandy Moore" testing-library-1) (list "TV" "Movie"))

;; artist-is-versatile?: string, (listof album) -> boolean
(define artist-is-versatile? (λ (desired-artist lib) (cond [(> (length (artist-genres desired-artist lib)) 1) true]
                                                                     [else false])))
  (check-expect (artist-is-versatile? "Mandy Moore" testing-library-1) true)
                                                                     
(check-expect (artist-is-versatile? "Amber Gray" testing-library-1) false)

;; versatile-artists: (listof album) -> (listof string)

(define versatile-artists (λ (lib) (filter (λ (current-artist) (artist-is-versatile? current-artist lib)) (all-artists lib))))

(check-expect (versatile-artists testing-library-1) (list "Mandy Moore"))

;; artist-album-counts: (listof album) -> (listof (list string number))
(define artist-album-count-list (λ (someartist lib) (list someartist (artist-album-count someartist lib))))
(define artist-album-count (λ (someartist lib) (length (artist-albums someartist lib))))

(define artist-album-counts (λ (lib) (map (λ (someartist) (artist-album-count-list someartist lib)) (all-artists lib))))

(check-expect (artist-album-counts testing-library-1) (list (list "Kristen Anderson-Lopez" 1)
                                                            (list "Mandy Moore" 2)
                                                            (list "Amber Gray" 1)))
;; genre-album-counts: (listof album) -> (listof (list string number))
(define genre-album-count-list (λ (somegenre lib) (list somegenre (genre-album-count somegenre lib))))
(define genre-album-count (λ (somegenre lib) (length (genre-albums somegenre lib))))
(define genre-albums (λ (desired-genre lib) (filter (λ (album) (string=? (album-genre album) desired-genre)) lib)))
(define genre-album-counts (λ (lib) (map (λ (somegenre) (genre-album-count-list somegenre lib)) (all-genres lib))))

(check-expect (genre-album-counts testing-library-1) (list (list "Musicals" 2)
                                                           (list "TV" 1)
                                                           (list "Movie" 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Below are two example album libraries and a few example tests
;;; for SOME functions.
;;;
;;; ALL functions in this assignment should work for ANY given
;;; library, and the test cases below illustrate how libraries
;;; are passed to album functions.
;;;
;;; These test cases are far from complete. Remember to write new
;;; album library and tests to see if your solution works as expected.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define example-testing-library-1
  (list (make-album "You're Stronger Than You Know" "James Morrison" "Pop")
        (make-album "Bootleg"                       "Kenshi Yonezu"  "J-Pop")))

(define example-testing-library-2
  (list (make-album "Arthur Rubinstein Collection" "Arthur Rubinstein" "Classic")
        (make-album "Scott Joplin Piano Rags"      "Joshua Rifkin"     "Rags")
        (make-album "The Violin Sonatas (4 CDs)"   "Lev Oborin"        "Classic")))

(check-expect (all-titles example-testing-library-1)
              (list "You're Stronger Than You Know"
                    "Bootleg"))

(check-expect (all-titles example-testing-library-2)
              (list "Arthur Rubinstein Collection"
                    "Scott Joplin Piano Rags"
                    "The Violin Sonatas (4 CDs)"))

(check-expect (all-genres example-testing-library-2)
              (list "Classic" "Rags"))

(check-expect (artist-albums "Scott Joplin" example-testing-library-2)
              (list))

(check-expect (artist-albums "Joshua Rifkin" example-testing-library-2)
              (list (make-album "Scott Joplin Piano Rags" "Joshua Rifkin" "Rags")))

(check-expect (artist-albums "Joshua Rifkin" example-testing-library-1)
              (list))

(check-expect (artist-albums "Kenshi Yonezu" example-testing-library-1)
              (list (make-album "Bootleg" "Kenshi Yonezu" "J-Pop")))

(check-expect (artist-is-versatile? "James Morrison" example-testing-library-1)
              #false)

(check-expect (artist-album-counts example-testing-library-1)
              (list (list "James Morrison" 1)
                    (list "Kenshi Yonezu" 1)))

(check-expect (procedure? all-titles) #true)
(check-expect (procedure? all-artists) #true)
(check-expect (procedure? all-genres) #true)
(check-expect (procedure? artist-albums) #true)
(check-expect (procedure? artist-genres) #true)
(check-expect (procedure? artist-is-versatile?) #true)
(check-expect (procedure? versatile-artists) #true)
(check-expect (procedure? artist-album-counts) #true)
(check-expect (procedure? genre-album-counts) #true)
