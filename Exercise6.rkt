;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "./file_operations.rkt")

; Part 1

; backup! : Path Path -> Void
;
; Recursively copies all the files and subdirectories in the `from`
; directory to the `to` directory. This is a modified version of the
; `copy-tree` procedure we discussed in class.
;
; EFFECT: `to` and all its contents now exist
; EFFECT: may overwrite existing files at `to`
(define (backup! from to) 
  (begin
    ; create the destination directory if it doesn't already exist
    (unless (directory-exists? to)
      (make-directory! to))

    ; for each file (leaf node) in the origin directory,
    ; copy it over to the destination directory
    (for-each (λ (file)
                ; print the name of the file being copied into the REPL
                ; for more on how `printf` works, see Appendix 1 in the pdf
                (unless (and (file-exists? (build-path to (path-filename file))) (> (file-or-directory-modify-seconds (build-path to (path-filename file))) (file-or-directory-modify-seconds (build-path from (path-filename file)))))
                  (begin
                  (printf "Copying file ~A to ~A~n"
                          file
                          (build-path to (path-filename file)))
                  (copy-file! file
                              (build-path to (path-filename file))
                              #true))))
              (directory-files from))

    ; for each folder (recursive child node) in the origin directory,
    ; recursively `backup!` its contents
    (for-each (λ (subdir)
                (backup! subdir
                         ; add the subdirectory's name to the
                         ; end of the original destination path
                         (build-path to (path-filename subdir))))
              (directory-subdirectories from))))
;test for backup! : (backup! (string->path "test") (string->path "output"))
; Part 2

; count-files: path -> number
(define (count-files path)
  (foldl + (length (directory-files path)) (map count-files (directory-subdirectories path))))
 

; directory-size: path -> number
(define (directory-size path)
  (foldl + (foldl + 0 (map file-size (directory-files path))) (map directory-size (directory-subdirectories path))))

; search-directory: string path -> (listof path)
(define (search-directory name path)
  (cond [(not (path? path)) '()]
        [(file-exists? path) (if (string-contains? name (path->string (path-filename path))) (list path)
                                                   '())]
        [else (append (apply append (map (λ (file) (search-directory name file)) (directory-files path)))
                      (apply append (map (λ (subdir) (search-directory name subdir)) (directory-subdirectories path))))]))

; filter-directory: (path -> boolean) path -> (listof path)
(define (filter-directory predicate path)
  (cond [(not (path? path)) '()]
        [(file-exists? path) (if (predicate path) (list path) '())]
        [else (append (apply append (map (λ (file)
                                           (filter-directory predicate file))
                                         (directory-files path)))
                      
                      (apply append (map (λ (subdir)
                                           (filter-directory predicate subdir))
                                         (directory-subdirectories path))))]))

; find-file-type: string path -> (listof path)
(define (find-file-type extension path)
  (filter-directory (λ (pth) (path-has-extension? pth extension)) path))

; file-type-disk-usage: string path -> number
(define (file-type-disk-usage extension path)
  (foldl + 0 (map (λ (file) (file-size file)) (find-file-type extension path))))
