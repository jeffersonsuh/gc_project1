#lang racket
(require csc151)
(require plot)
(define file1 (read-csv-file "/Users/jeffersuhn/Documents/grinnell/season_one/CSC-151-03/boxoffice.csv"))
(define file2 (read-csv-file "/Users/jeffersuhn/Documents/grinnell/season_one/CSC-151-03/movies.csv"))

;;;Procedure
;;; newfile1
;;;Parameters
;;; file, a list of lists
;;;Purpose
;;; to clean the data file by removing the 2 element of every list
;;;Produces
;;; lst, a cleaned list of lists
;;;Preconditions
;;; the input file must be in the format of file1
;;; the element that needs to be removed needs to be the second element
;;;Postconditions
;;; if file is empty then produces null
;;; if file is not empty then produces list of lists with the third
;;;element removed from the intial file, all other elements should be in the same order from the intial file
(define newfile1
  (lambda (file)
    (let kernel ([fle file]
                 [lst-so-far null])
      (cond
        [(null? fle) (reverse lst-so-far)]
        [else (kernel (cdr fle) (cons (remove (list-ref (car fle) 2) (car fle)) lst-so-far))]))))


;;;Procedure
;;; newfile2
;;;Parameters
;;; file, a list of lists
;;;Purpose
;;; to clean the data file by removing the 5,8,10,12,14 elements of every list
;;;Produces
;;; lst, a cleaned list of lists
;;;Preconditions
;;; the input file must be in the format of file2
;;; the elements that needs to be removed need to be the 5,8,10,12,14 elements
;;;Postconditions
;;; if file is empty then produces null
;;; if file is not empty then produces list of lists with the 5,8,10,12,14 elements removed from the intial file, all other elements should be in the same order from the intial file
(define newfile2
  (lambda (file)
    (let kernel ([fle file]
                 [lst-so-far null])
      (cond
        [(null? fle) (reverse lst-so-far)]
        [else (kernel (cdr fle) (cons (remove (list-ref (car fle) 5)
                                              (remove (list-ref (car fle) 8)
                                                      (remove (list-ref (car fle) 10)
                                                              (remove (list-ref (car fle) 12)
                                                                      (remove (list-ref (car fle) 14) (car fle))))))
                                      lst-so-far))]))))

;;;Citation (for new-assoc): http://www.cs.grinnell.edu/~eikmeier/csc151F19/readings/association-lists.html
(define assoc-new
  (let ([match? (lambda (val1 val2)
                  (or (and (string? val1)
                           (string? val2)
                           (string-ci=? val1 val2))
                      (equal? val1 val2)))])
    (lambda (key alist)
      (cond
        ; If there are no entries left in the association list,
        ; there are no entries with the given key.
        [(null? alist)
         #f]
        ; If the key we're looking for matches the key of the first
        ; entry, then use that entry.
        [(match? key (list-ref (car alist) 1))
         (car alist)]
        ; Otherwise, look in the rest of the association list.
        [else
         (assoc-new key (cdr alist))]))))

; Citation (for sort): https://docs.racket-lang.org/reference/pairs.html

;;;Procedure
;;; filecombiner
;;;Parameters
;;; fle1, a list of lists
;;; fle2, a list of lists
;;;Purpose
;;; to combine two cleaned lists for the elements that include the same movie title
;;;Produces
;;; result-table, a list of lists
;;;Preconditions
;;; the input file must be in the format of newfile1, the cleaned table that was initially file1,
;;; and newfile2, the cleaned table that was initially file2.
;;;Postconditions
;;; if fl1 or fl2 is empty then result-table will be null
;;; if file is not empty then result-table will be a list of lists that contain elements that are
;;; ordered by rank, title, gross, year, budget, company, country, director, genre, name, parental
;;; ratings, runtime, star, and writer.
(define filecombiner
  (lambda (fle1 fle2)
    (let kernel ([lst-so-far null]
                 [fl1 fle1]
                 [fl2 fle2])
      (cond
        [(null? fl2)
         (sort lst-so-far #:key car <)]
        [(not (equal? #f (assoc-new (list-ref (car fl2) 5) fl1)))
         (kernel (cons (append (assoc-new (list-ref (car fl2) 5) fl1) (car fl2)) lst-so-far) fl1 (cdr fl2))]
        [else (kernel lst-so-far fl1 (cdr fl2))])))) 
;;;Procedure
;;; finalclean
;;;Parameters
;;; file, a list of lists
;;;Purpose
;;; to remove the repeated name from the cleaned list
;;;Produces
;;; result-table, a list of lists
;;;Preconditions
;;; the input file should be the appended version of cleaned file1 and file2
;;;Postconditions
;;; if file is empty then produces null
;;; if file is not empty then produces list of lists with the second element removed from the intial file,
;;;all other elements should be in the same order from the intial file
;;;ordered by rank, gross, year, budget, company, country, director, genre, name, parental
;;; ratings, runtime, star, and writer
(define finalclean
  (lambda (file)
    (let kernel ([fle file]
                 [lst-so-far null])
      (cond
        [(null? fle) (reverse lst-so-far)]
        [else (kernel (cdr fle) (cons (remove (list-ref (car fle) 1) (car fle)) lst-so-far))]))))


(define cleanfile (finalclean (take (filecombiner (newfile1 file1) (newfile2 file2)) 100)))

(define plotyear
  (lambda (file)
    (let* ([year
            (sort (tally-all (map caddr file)) #:key car <)])
      (let kernel ([fle year]
                   [lst-so-far null])
        (cond
          [(null? fle) (reverse lst-so-far)]
          [(>= (caar fle) 2000) (kernel (cdr fle) (cons (cons (- (caar fle) 2000) (cdar fle)) lst-so-far))]
          [else (kernel (cdr fle) (cons (cons (- (caar fle) 1900) (cdar fle)) lst-so-far))])))))


;graph of movies in top 100 my year
#|(plot (discrete-histogram (drop (plotyear cleanfile) 8))
        #:title "Movies in top 100 by Year"
        #:x-label "Year"
        #:y-label "Number of Movies")|#
(define year
  (lambda (file)
    (sort (tally-all (map caddr file)) #:key car <)))
#|(plot (discrete-histogram (year (take cleanfile 10)))
        #:title "Movies in top 10 by Year"
        #:x-label "Year"
        #:y-label "Number of Movies")|#

(define parental-rating
  (lambda (file)
    (tally-all (map cadddr (map reverse file)))))
(plot (discrete-histogram (parental-rating cleanfile))
        #:title "Movies in top 100 by Parental Rating"
        #:x-label "Parental Rating"
        #:y-label "Number of Movies")
(plot (discrete-histogram (parental-rating (take cleanfile 10)))
        #:title "Movies in top 10 by Parental Rating"
        #:x-label "Parental Rating"
        #:y-label "Number of Movies")

(define genre
  (lambda (file)
    (sort (tally-all (map car (map drop file (make-list 100 7)))) #:key cadr >)))
(plot (discrete-histogram (genre cleanfile))
        #:title "Movies in top 100 by Genre"
        #:x-label "Genre"
        #:y-label "Number of Movies")
(define genre2
  (lambda (file)
    (sort (tally-all (map car (map drop file (make-list 10 7)))) #:key cadr >)))
(plot (discrete-histogram (genre2 (take cleanfile 10)))
        #:title "Movies in top 10 by Genre"
        #:x-label "Genre"
        #:y-label "Number of Movies")

(define star
  (lambda (file)
    (sort (tally-all (map cadr (map reverse file))) #:key cadr >)))
(star cleanfile)