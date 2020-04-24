#lang racket

; This module implements a citation system meant to be used in Pollen.

(provide
 ; To be used in author source
 ; ---------------------------

 ; Declares a work that can be then cited.
 declare-work
 ; Alternate form for declare-work that doesn't accept an id. It renders the work in-place.
 ; If you use this form, the work cannot be referred to later using 'cite'.
 format-work
 ; Actually cites the work (returns a txexpr with the formatted citation).
 cite
 ; Get bibliography.
 bibliography-ids
 ; Gets a bibliography-formatted citation.
 bib-entry
 bib-sort-value
 ; Gets the short-form
 short-form

 ; To be used in your Pollen module to let the citation system
 ; do the work it needs to do.
 ; ------------------------------------------------------------

 ; Use this within your note tag to let this citation system transform
 ; the citations it created with 'cite' into back-references or to include
 ; short forms.
 transform-cite-in-a-note
 ; Use during the final decode (txexpr-proc) to insert short-forms where
 ; they're needed.
 show-necessary-short-forms)

;----------------------------------------------------------------------
; Implementation

(require pollen/core)
(require racket/contract)
(require txexpr)

(module+ test
  (require rackunit))

(define work-metadata (make-hash))
(define first-place-cited (make-hash))
(define most-recent-ibid-or-supra #f)
(define short-form-used? (make-hash))
(define unidentified-work-count 0)

(define/contract (bibliography-ids #:category [category #f])
  (()(#:category (or/c "jurisprudence" "secondary" "legislation" "other" #f)) . ->* . (listof string?))
  (define (create-category-filter category)
    (case category
      [("jurisprudence") (λ (key) (member (hash-ref (hash-ref work-metadata key) 'type) '("legal-case" "legal-case-US")))]
      [("secondary") (λ (key) (member (hash-ref (hash-ref work-metadata key) 'type) '("article" "book" "magazine/news" "thesis" "proceedings" "unpublished")))]
      [("legislation") (λ (key) (member (hash-ref (hash-ref work-metadata key) 'type) '("bill" "statute" "regulation" "debate")))]
      [("other") (λ (key) (member (hash-ref (hash-ref work-metadata key) 'type) '("custom")))]))
  (if category
      (filter (create-category-filter category) (hash-keys first-place-cited))
      (hash-keys first-place-cited)))

; Accessor
(define/contract (get-work-by-id id)
  (string? . -> . hash?)
  (hash-ref work-metadata (clean-param id)))

; Helpers
(define (declared-id? id)
  (hash-has-key? work-metadata (clean-param id)))

(define-syntax-rule (when-or-empty condition lst)
  (if condition lst '()))

(define (string-is-affirmative? x)
  (define (member? v lst)
    (list? (member v lst)))
  (member? (string-downcase x) '("yes" "true" "y" "t" "#t")))

(module+ test
  (check-true (string-is-affirmative? "yes"))
  (check-true (string-is-affirmative? "Yes"))
  (check-true (string-is-affirmative? "true"))
  (check-true (string-is-affirmative? "True"))
  (check-true (string-is-affirmative? "#t"))
  (check-true (string-is-affirmative? "#T"))
  (check-true (string-is-affirmative? "y"))
  (check-true (string-is-affirmative? "Y"))
  (check-true (string-is-affirmative? "t"))
  (check-true (string-is-affirmative? "T"))
  (check-false (string-is-affirmative? "No"))
  (check-false (string-is-affirmative? ""))
  (check-false (string-is-affirmative? "#F")))

(define/contract (merge-successive-strings elements)
  (txexpr-elements? . -> . txexpr-elements?)
  (define (conditional-merge element current-list)
    (if (and (not (empty? current-list))
             (string? (first current-list))
             (string? element))
        (cons (string-append (first current-list) element) (drop current-list 1))
        (cons element current-list)))
  (reverse (foldl conditional-merge '() elements)))

(module+ test
  (check-equal? (merge-successive-strings '()) '())
  (check-equal? (merge-successive-strings '("a")) '("a"))
  (check-equal? (merge-successive-strings '(a)) '(a))
  (check-equal? (merge-successive-strings '(a "b")) '(a "b"))
  (check-equal? (merge-successive-strings '("a" "b")) '("ab"))
  (check-equal? (merge-successive-strings '("a" a "b")) '("a" a "b"))
  (check-equal? (merge-successive-strings '("a" "b" " " "c")) '("ab c"))
  (check-equal? (merge-successive-strings '("a" "b" a " " b "c" c d "e" " f" g)) '("ab" a " " b "c" c d "e f" g))
  )

; Citation system. Following the McGill Guide, with Chicago Manual of Style for any ambiguities.
; ----------------------------------------------------------------------------------------------

(define/contract (valid-work-type? type)
  (string? . -> . boolean?)
  (if (member type '("article" "chapter" "thesis" "proceedings" "unpublished" "legal-case" "legal-case-US"
                               "bill" "statute" "regulation" "debate" "book" "magazine/news" "custom"))
      #t #f))

(module+ test
  (check-true (valid-work-type? "thesis"))
  (check-false (valid-work-type? "invalid work")))

(define/contract (strip-at str)
  (string? . -> . string?)
  (if (string-prefix? (string-downcase str) "at ")
      (substring str 3)
      str))

(module+ test
  (check-equal? (strip-at "page 13") "page 13")
  (check-equal? (strip-at "at 13") "13")
  (check-equal? (strip-at "At para 12") "para 12")
  (check-equal? (strip-at "cat 5") "cat 5"))

(define/contract (pinpoint-is-pages? str)
  (string? . -> . boolean?)
  (or (string-prefix? (string-downcase str) "p ")
      (string-prefix? (string-downcase str) "pp ")
      (string-prefix? (string-downcase str) "page ")
      (string-prefix? (string-downcase str) "pages ")
      (regexp-match-exact? #rx"([-0-9, ]*)" str)))

(module+ test
  (check-true (pinpoint-is-pages? "12--39"))
  (check-true (pinpoint-is-pages? "p 12"))
  (check-false (pinpoint-is-pages? "para 12"))
  (check-false (pinpoint-is-pages? "paras 12--14"))
  (check-false (pinpoint-is-pages? "paras 12, 14"))
  (check-true (pinpoint-is-pages? "pp 32, 33"))
  (check-true (pinpoint-is-pages? "Page 1"))
  (check-true (pinpoint-is-pages? "1"))
  (check-true (pinpoint-is-pages? "1, 35")))

(define/contract (pinpoint-is-timestamp? str)
  (string? . -> . boolean?)
  (or (regexp-match? #px"\\d\\dh:\\d\\dm:\\d\\ds" str)
      (regexp-match? #px"\\d\\dm:\\d\\ds" str)))

(module+ test
  (check-false (pinpoint-is-timestamp? "12"))
  (check-false (pinpoint-is-timestamp? "s 12"))
  (check-true (pinpoint-is-timestamp? "00h:12m:32s"))
  (check-true (pinpoint-is-timestamp? "00m:33s")))

(define (pinpoint-requires-at? str) (or (string-prefix? (strip-at str) "para") (pinpoint-is-pages? (strip-at str)) (pinpoint-is-timestamp? (strip-at str))))
(define (pinpoint-requires-comma? str) (not (pinpoint-requires-at? str)))

(define/contract (normalize-pinpoint pinpoint)
  (string? . -> . string?)
  (define cleaned-pinpoint (clean-param pinpoint))
  (define pinpoint-without-at (strip-at cleaned-pinpoint))
  (define to-replace (first (string-split pinpoint-without-at)))
  (define replacement
    (case (string-downcase to-replace)
      [("page" "p") ""]
      [("pages" "pp") ""]
      [("paragraph" "para.") "para"]
      [("paragraphs" "paras.") "paras"]
      [("clause" "cl.") "cl"]
      [("clauses" "cls.") "cls"]
      [("section" "s.") "s"]
      [("sections" "ss.") "ss"]
      [else to-replace]))
  (define pinpoint-content (string-trim (string-replace pinpoint-without-at to-replace replacement #:all? #f)))
  (if (pinpoint-requires-at? cleaned-pinpoint) (format " at ~a" pinpoint-content) (format ", ~a" pinpoint-content)))

(module+ test
  (check-equal? (normalize-pinpoint "page 1") " at 1")
  (check-equal? (normalize-pinpoint "page\n1") " at 1")
  (check-equal? (normalize-pinpoint "at page 1") " at 1")
  (check-equal? (normalize-pinpoint "pages 2, 3") " at 2, 3")
  (check-equal? (normalize-pinpoint "p 1--3") " at 1--3")
  (check-equal? (normalize-pinpoint "paragraph 1") " at para 1")
  ; TODO (check-equal? (normalize-pinpoint "paragraph 1--3") " at paras 1--3")
  (check-equal? (normalize-pinpoint "at clause 1") ", cl 1")
  (check-equal? (normalize-pinpoint "clause 1") ", cl 1")
  (check-equal? (normalize-pinpoint "cls 2--3") ", cls 2--3")
  )

(define/contract (strip-http/https-protocol url)
  (string? . -> . string?)
  (if (string-prefix? url "https://")
      (string-replace url "https://" "")
      (if (string-prefix? url "http://")
          (string-replace url "http://" "")
          url)))

(module+ test
  (check-equal? (strip-http/https-protocol "https://www.ubc.ca") "www.ubc.ca")
  (check-equal? (strip-http/https-protocol "http://www.ubc.ca") "www.ubc.ca")
  (check-equal? (strip-http/https-protocol "www.ubc.ca") "www.ubc.ca"))

(define/contract (year-is-necessary? citation)
  (string? . -> . boolean?)
  ; Are the first alphanumeric characters in the citation a four-digit year? Ie. Is there a sequence of four consecutive
  ; digits before any letter?
  (not (regexp-match? #px"[[:digit:]]{4}" (first (regexp-match #px"\\S*[[:space:]]" citation)))))

(module+ test
  (check-true (year-is-necessary? "301 DLR (4th) 34"))
  (check-false (year-is-necessary? "[1977] 1 SCR 193"))
  (check-false (year-is-necessary? "2012 SCC 1")))

(define/contract (year-is-different? work)
  (hash? . -> . boolean?)
  ; Is there a declared year that is different than the first four digits of the citation?
  (and (hash-ref work 'year)
       (not (year-is-necessary? (hash-ref work 'citation)))
       (not (equal? (hash-ref work 'year) (first (regexp-match #px"[[:digit:]]{4}" (first (regexp-match #px"\\S*[[:space:]]" (hash-ref work 'citation)))))))))

(module+ test
  (check-false (year-is-different? (hash 'year "2018" 'citation "2018 SCC 1")))
  (check-false (year-is-different? (hash 'year #f 'citation "2018 SCC 1")))
  (check-true (year-is-different? (hash 'year "2017" 'citation "[2018] 1 SCR 1")))
  (check-false (year-is-different? (hash 'year "2017" 'citation "[2017] 1 SCR 1"))))

(define/contract (clean-param param)
  ((or/c string? #f) . -> . (or/c string? #f))
  (if param (string-normalize-spaces param) param))

(module+ test
  (check-false (clean-param #f))
  (check-exn exn:fail? (λ () (clean-param #t)))
  (check-equal? (clean-param "multi-line\nargument") "multi-line argument"))

(define/contract (get-given-from-author author)
  (string? . -> . string?)
  (define parts (string-split (clean-param author)))
  (if (not (equal? (length parts) 2))
      (raise-user-error "Specified an author (a shortcut keyword) that has fewer or more than two parts: " author)
      (first parts)))

(define/contract (get-family-from-author author)
  (string? . -> . string?)
  (define parts (string-split (clean-param author)))
  (if (not (equal? (length parts) 2))
      (raise-user-error "Specified an author (a shortcut keyword) that has fewer or more than two parts: " author)
      (second parts)))

(module+ test
  (check-equal? (get-given-from-author "Sancho McCann") "Sancho")
  (check-equal? (get-family-from-author "Sancho McCann") "McCann")
  (check-exn exn:fail? (λ () (get-given-from-author "Sancho J McCann")))
  (check-exn exn:fail? (λ () (get-family-from-author "Sancho J McCann")))
  (check-exn exn:fail? (λ () (get-given-from-author "Sancho")))
  (check-exn exn:fail? (λ () (get-family-from-author "Sancho"))))

(define/contract (extract-first-page pages)
  (string? . -> . string?)
  (first (regexp-match #rx"[0-9]+" pages)))

(module+ test
  (check-equal? (extract-first-page "1--10") "1")
  (check-equal? (extract-first-page "10") "10")
  (check-equal? (extract-first-page "11--101") "11")
  (check-equal? (extract-first-page "11---101") "11")
  (check-equal? (extract-first-page "11-101") "11")
  (check-exn exn:fail? (λ () (extract-first-page "not a page or range"))))

; ------------------------------------------------------------------------------
; These validators check that a declared work has
; the mandatory elements and has none that are incompatible
; with each other.

(define/contract (validate-work-or-die w)
  (hash? . -> . void?)
  (validate-short-form w)
  (case (hash-ref w 'type)
    [("article") (validate-article w)]
    [("chapter") (validate-chapter w)]
    [("thesis") (validate-thesis w)]
    [("proceedings") (validate-proceedings w)]
    [("unpublished") (validate-unpublished w)]
    [("legal-case") (validate-legal-case w)]
    [("legal-case-US") (validate-legal-case-US w)]
    [("bill") (validate-bill w)]
    [("statute") (validate-statute w)]
    [("regulation") (validate-regulation w)]
    [("debate") (validate-debate w)]
    [("book") (validate-book w)]
    [("magazine/news") (validate-magazine/news w)]
    [("custom") (validate-custom w)]
    [else (raise-user-error "Unrecognized type for work: " (hash-ref w 'type))]))

(module+ test
  (check-not-exn (λ () (validate-work-or-die (hash 'type "article"
                                                   'author-given "Sancho"
                                                   'author-family "McCann"
                                                   'title "My article"
                                                   'journal "A journal"
                                                   'volume "2"
                                                   'short-form "McCann"))))
  (check-exn exn:fail? (λ () (validate-work-or-die (hash 'type "garbage"
                                                         'author-given "Sancho"
                                                         'author-family "McCann"
                                                         'title "My article"
                                                         'journal "A journal"
                                                         'volume "2"
                                                         'short-form "McCann")))))


(define (validate-short-form w)
  (define (short-form-taken s)
    (ormap (λ (v) (equal? (hash-ref v 'short-form) s)) (hash-values work-metadata)))
  (when (short-form-taken (hash-ref w 'short-form))
    (raise-user-error "Attempt to use duplicate short-form: " `(,(hash-ref w 'short-form) ,w))))

(define/contract (validate-mandatory-elements type w mandatory-elements)
  (valid-work-type? hash? (listof symbol?) . -> . void?)
  (for-each
   (λ (e)
     (when (not (hash-ref w e))
       (raise-user-error (format "~a is missing required field: ~a" type e)))) mandatory-elements))

(module+ test
  (check-not-exn (λ () (validate-mandatory-elements "article" (hash 'key1 "value1" 'key2 "value2") '(key1 key2))))
  (check-exn exn:fail? (λ () (validate-mandatory-elements "article" (hash 'key1 "value1" 'key2 #f) '(key1 key2))))
  (check-exn exn:fail? (λ () (validate-mandatory-elements "article" (hash 'key1 "value1" 'key2 "value2") '(key1 key2 key3))))
  )

(define (work-has-author-or-die w)
  (when (not (or (and (hash-ref w 'author-given #f) (hash-ref w 'author-family #f)) (hash-ref w 'author-institutional #f)))
    (raise-user-error "failed to specify an author of any kind for this work: " w)))

(define (validate-article w)
  (validate-mandatory-elements "article" w '(title journal volume))
  (work-has-author-or-die w))

(define (validate-chapter w)
  (validate-mandatory-elements "chapter" w '(title in-book first-page))
  (work-has-author-or-die w))

(define (validate-book w)
  (validate-mandatory-elements "book" w '(title year)))

(define (validate-thesis w)
  (validate-mandatory-elements "thesis" w '(title institution thesis-description year))
  (work-has-author-or-die w))

(define (validate-proceedings w)
  (validate-mandatory-elements "proceedings" w '(title proceedings year))
  (work-has-author-or-die w))

(define (validate-unpublished w)
  (validate-mandatory-elements "unpublished" w '(title year))
  (work-has-author-or-die w))

(define (validate-bill w)
  (validate-mandatory-elements "bill" w '(number title legislative-body year)))

(define (validate-statute w)
  (validate-mandatory-elements "statute" w '(title volume year chapter)))

(define (validate-regulation w)
  (validate-mandatory-elements "regulation" w '(title volume year)))

(define (validate-debate w)
  (validate-mandatory-elements "debate" w '(jurisdiction legislative-body year))
  ; These next two elements must go together. They must either both be specified
  ; or both be unspecified.
  (when (and (hash-ref w 'title) (not (hash-ref w 'reading)))
    (raise-user-error "specified the title of a bill under debate without specifying which reading: " w))
  (when (and (hash-ref w 'reading) (not (hash-ref w 'title)))
    (raise-user-error "specified a reading of a bill without specifying the title of the bill: " w)))

(define (validate-magazine/news w)
  (validate-mandatory-elements "magazine/news" w '(title)))

(define (validate-custom w)
  (validate-mandatory-elements "custom" w '(custom-format)))

(define (validate-legal-case w)
  (validate-mandatory-elements "legal-case" w '(title citation))
  (when (and (hash-ref w 'cited-to) (not (string-contains? (hash-ref w 'citation) (hash-ref w 'cited-to))))
    (raise-user-error "the cited-to helper information, if provided, must refer to the primary citation: " (hash-ref w 'citation)))
  (when (and (year-is-necessary? (hash-ref w 'citation)) (not (hash-ref w 'year)))
    (raise-user-error "Failed to declare year when year is not the first element of the first citation: " (hash-ref w 'citation))))

(define (validate-legal-case-US w)
  (validate-mandatory-elements "legal-case-US" w '(title citation year)))

(define/contract (build-author-based-short-form author1 author2 author3)
  (string? (or/c string? #f) (or/c string? #f) . -> . string?)
  (apply string-append
   `(,author1
     ,@(when-or-empty (and author2 (not author3)) `(" & " ,author2))
     ,@(when-or-empty (and author2 author3) `(", " ,author2))
     ,@(when-or-empty author3 `(" & " ,author3)))))

; "article" "chapter" "thesis" "proceedings" "unpublished" "debate" "book" "magazine/news"
(define/contract (default-short-form work)
  (hash? . -> . txexpr-elements?)
  (case (hash-ref work 'type)
    [("legal-case" "legal-case-US" "statute" "regulation") `((em ,(hash-ref work 'title)))]
    [("bill") `(,(string-append "Bill " (hash-ref work 'number)))]
    [("article" "chapter" "thesis" "proceedings" "unpublished" "book")
     (if (hash-ref work 'author-family)
         `(,(build-author-based-short-form
             (hash-ref work 'author-family)
             (hash-ref work 'author2-family)
             (hash-ref work 'author3-family)))
         (raise-user-error "Failed to create default short-form for work." work))]
    [("magazine/news")
     (if (hash-ref work 'author-family)
         `(,(build-author-based-short-form
             (hash-ref work 'author-family)
             (hash-ref work 'author2-family)
             (hash-ref work 'author3-family)))
         (merge-successive-strings (append '("“") (style-markedup-text (hash-ref work 'title)) '("”"))))]
    [else (raise-user-error "Can't create default short-forms for works of this type." work)]))

(define/contract (default-short-form-undetermined? work)
  (hash? . -> . boolean?)
  (case (hash-ref work 'type)
    [("legal-case" "legal-case-US" "statute" "regulation" "bill") #f]
    [("article" "chapter" "thesis" "proceedings" "unpublished" "book") (not (hash-ref work 'author-family))]
    [("magazine/news") (and (not (hash-ref work 'author-family)) (not (hash-ref work 'title)))]
    [else #t]))

(module+ test
  (test-begin
   (declare-work #:type "book" #:author "Peyton Manning" #:title "How to throw a football" #:year "2012" #:id "manning")
   (check-equal? (hash-ref (hash-ref work-metadata "manning") 'short-form) `("Manning"))
   (declare-work #:type "magazine/news" #:title "title with some *italicized* portion" #:id "title-only-work")
   (check-equal? (hash-ref (hash-ref work-metadata "title-only-work") 'short-form) `("“title with some " (em "italicized") " portion”"))))
  ; (check-equal? (default-short-form "book" "McCann" #f #f "Title") `("McCann"))
  ; (check-equal? (default-short-form "article" "McCann" "Lowe" #f "Title") `("McCann & Lowe"))
  ; (check-equal? (default-short-form "article" "McCann" "Lowe" "Muja" "Title") `("McCann, Lowe & Muja")))

(define/contract (work-has-short-form? work)
  (hash? . -> . boolean?)
  (hash-ref work 'short-form #f))

(define/contract (add-short-form-to-incomplete-work work)
  ((not/c work-has-short-form?) . -> . hash?)
  ; Copy the existing work structure but with a new short-form
  (define complete-work (hash-copy work))
  (hash-set! complete-work 'short-form (default-short-form work))
  complete-work)

(define (declare-work #:type type
                      #:id id
                      #:title [title #f]
                      #:author [author #f] ; a shortcut for simple "author-given author-family" names --- incompatible with author-given / author-family
                      #:author-institutional [author-institutional #f] ; if the author is an institution rather than a person
                      #:author-given [author-given #f]
                      #:author-family [author-family #f]
                      #:author2-given [author2-given #f]
                      #:author2-family [author2-family #f]
                      #:author3-given [author3-given #f]
                      #:author3-family [author3-family #f]
                      #:editors? [editors? ""] ; will be treated as false unless an affirmative string is given
                      #:etal? [etal? ""] ; will be treated as false unless an affirmative string is given
                      #:in-book [in-book #f] ; needed for chapter citations
                      #:journal [journal #f]
                      #:edition [edition #f]
                      #:year [year #f] ; alias for "date" --- incompatible with date
                      #:date [date #f] ; alias for "year" --- incompatible with year
                      #:volume [volume #f]
                      #:publication [publication #f] ; for magazine/news
                      #:issue [issue #f]
                      #:citation [citation #f]
                      #:parallel-citation [parallel-citation #f]
                      #:jurisdiction [jurisdiction #f]
                      #:case-judge [case-judge #f]
                      #:institution [institution #f]
                      #:legislative-body [legislative-body #f]
                      #:number [number #f] ; for bills and some regulations
                      #:chapter [chapter #f] ; for statutes and some regulations
                      #:reading [reading #f] ; for legislative debates
                      #:bill-status [bill-status #f] ; a parenthetical at the end of bill's citation
                      #:eventual-statute [eventual-statute #f] ; additional detail for a bill that has passed
                      #:proceedings [proceedings #f]
                      #:publisher [publisher #f]
                      #:publisher-location [publisher-location #f]
                      #:thesis-description [thesis-description #f]
                      #:description [description #f]
                      #:comment-info [comment-info #f]
                      #:forthcoming [forthcoming #f]
                      #:pages [pages #f] ; will extract the first-page from this; incompatible with first-page
                      #:first-page [first-page #f]
                      #:url [url #f]
                      #:display-url? [display-url? #f]
                      #:online-type [online-description #f]
                      #:online-title [online-title #f]
                      #:custom-format [custom-format #f] ; only for type "custom"
                      #:short-form [short-form #f]
                      #:cited-to [cited-to #f])
  (define cleaned-id (clean-param id))
  (when (and author (or author-given author-family))
    (raise-user-error "You used #:author and either #:author-given or #:author-family. #:author is a substitute for the latter when the name is simple." `(,author ,author-given ,author-family)))
  (when (and author-institutional (or author author-given author-family))
    (raise-user-error "You used #:author-institutional, which precludes use of the other author fields #:author, #:author-given, #:author-family." `(,author-institutional)))
  (when (and year date)
    (raise-user-error "You specified both a year and a date. Use only one of these." `(,year ,date ,title)))
  (when (and pages first-page)
    (raise-user-error "You specified both pages and first-page. Use only one of these." `(,pages ,first-page)))
  (when (hash-has-key? work-metadata cleaned-id) (raise-user-error "duplicate id" cleaned-id))
  (define incomplete-work
    (hash 'type type
          'id cleaned-id
          'title (clean-param title)
          'author-institutional (clean-param author-institutional)
          'author-given (if author (get-given-from-author author) (clean-param author-given))
          'author-family (if author (get-family-from-author author) (clean-param author-family))
          'author2-given (clean-param author2-given)
          'author2-family (clean-param author2-family)
          'author3-given (clean-param author3-given)
          'author3-family (clean-param author3-family)
          'editors? (clean-param editors?)
          'etal? (clean-param etal?)
          'in-book (clean-param in-book)
          'journal (clean-param journal)
          'edition (clean-param edition)
          'publication (clean-param publication)
          'year (if year year date)
          'volume volume
          'issue issue
          'citation (clean-param citation)
          'parallel-citation (clean-param parallel-citation)
          'jurisdiction (clean-param jurisdiction)
          'case-judge (clean-param case-judge)
          'institution (clean-param institution)
          'legislative-body (clean-param legislative-body)
          'number (clean-param number)
          'chapter (clean-param chapter)
          'bill-status (clean-param bill-status)
          'eventual-statute (clean-param eventual-statute)
          'reading (clean-param reading)
          'proceedings (clean-param proceedings)
          'publisher (clean-param publisher)
          'publisher-location (clean-param publisher-location)
          'thesis-description (clean-param thesis-description)
          'description (clean-param description)
          'comment-info (clean-param comment-info)
          'forthcoming forthcoming
          'first-page (if pages (extract-first-page pages) first-page)
          'url url
          'display-url? display-url?
          'online-description (clean-param online-description)
          'online-title (clean-param online-title)
          'custom-format (clean-param custom-format)
          'short-form (if short-form
                          (style-markedup-text (clean-param short-form))
                          #f)
          'cited-to cited-to))
  (define this-work
    (if (hash-ref incomplete-work 'short-form)
        incomplete-work
        (add-short-form-to-incomplete-work incomplete-work)))
  (validate-work-or-die this-work)
  (hash-set! work-metadata cleaned-id this-work))

; Just forward all arguments to declare-work, but this form does not accept
; an id. It will return a txexpr to be rendered in-place.
; TODO: Find a way to programmatically re-use these arguments.
(define (format-work #:type type
                     #:title [title #f]
                     #:author [author #f] ; a shortcut for simple "author-given author-family" names --- incompatible with author-given / author-family
                     #:author-institutional [author-institutional #f] ; used for institutional authors rather than people
                     #:author-given [author-given #f]
                     #:author-family [author-family #f]
                     #:author2-given [author2-given #f]
                     #:author2-family [author2-family #f]
                     #:author3-given [author3-given #f]
                     #:author3-family [author3-family #f]
                     #:editors? [editors? ""]
                     #:etal? [etal? ""]
                     #:in-book [in-book #f]
                     #:journal [journal #f]
                     #:edition [edition #f]
                     #:year [year #f] ; alias for "date" --- incompatible with date
                     #:date [date #f] ; alias for "year" --- incompatible with year
                     #:volume [volume #f]
                     #:publication [publication #f] ; for magazine/news
                     #:issue [issue #f]
                     #:citation [citation #f]
                     #:parallel-citation [parallel-citation #f]
                     #:jurisdiction [jurisdiction #f]
                     #:case-judge [case-judge #f]
                     #:institution [institution #f]
                     #:legislative-body [legislative-body #f]
                     #:number [number #f] ; for bills
                     #:chapter [chapter #f] ; for statutes
                     #:reading [reading #f] ; for legislative debates
                     #:bill-status [bill-status #f] ; a parenthetical at the end of bill's citation
                     #:eventual-statute [eventual-statute #f] ; additional detail for a bill that has passed
                     #:proceedings [proceedings #f]
                     #:publisher [publisher #f]
                     #:publisher-location [publisher-location #f]
                     #:thesis-description [thesis-description #f]
                     #:description [description #f]
                     #:comment-info [comment-info #f]
                     #:forthcoming [forthcoming #f]
                     #:pages [pages #f] ; will extract the first-page from this; incompatible with first-page
                     #:first-page [first-page #f]
                     #:url [url #f]
                     #:display-url? [display-url? #f]
                     #:online-type [online-description #f]
                     #:online-title [online-title #f]
                     #:custom-format [custom-format #f]
                     #:short-form [short-form #f])
  (define id (format "unidentified-work-~a" unidentified-work-count))
  (set! unidentified-work-count (+ 1 unidentified-work-count))
  (declare-work #:type type
                #:id id
                #:title title
                #:author author
                #:author-institutional author-institutional
                #:author-given author-given
                #:author-family author-family
                #:author2-given author2-given
                #:author2-family author2-family
                #:author3-given author3-given
                #:author3-family author3-family
                #:editors? editors?
                #:etal? etal?
                #:in-book in-book
                #:journal journal
                #:edition edition
                #:year year
                #:date date
                #:volume volume
                #:publication publication
                #:issue issue
                #:citation citation
                #:parallel-citation parallel-citation
                #:jurisdiction jurisdiction
                #:case-judge case-judge
                #:institution institution
                #:legislative-body legislative-body
                #:number number
                #:chapter chapter
                #:reading reading
                #:bill-status bill-status
                #:eventual-statute eventual-statute
                #:proceedings proceedings
                #:publisher publisher
                #:publisher-location publisher-location
                #:thesis-description thesis-description
                #:description description
                #:comment-info comment-info
                #:forthcoming forthcoming
                #:pages pages
                #:first-page first-page
                #:url url
                #:display-url? display-url?
                #:online-type online-description
                #:online-title online-title
                #:custom-format custom-format
                #:short-form id) ; short form will not be used
  (cite id))

(define/contract (style-markedup-text markedup-text)
  (string? . -> . txexpr-elements?)
  (define italic-range (regexp-match-positions #rx"\\*.*?\\*" markedup-text))
  (if italic-range
      (let* ([before (substring markedup-text 0 (car (car italic-range)))]
             [special-content (substring markedup-text (+ (car (car italic-range)) 1) (- (cdr (car italic-range)) 1))]
             [after (substring markedup-text (cdr (car italic-range)))])
        `(,@(when-or-empty (non-empty-string? before) `(,before))
          (em ,special-content)
          ,@(when-or-empty (non-empty-string? after) `(,@(style-markedup-text after)))))
      `(,markedup-text)))

(module+ test
  (check-equal? (style-markedup-text "title") '("title"))
  (check-equal? (style-markedup-text "title *with italics*") '("title " (em "with italics")))
  (check-equal? (style-markedup-text "*italics* at start of title") '((em "italics") " at start of title"))
  (check-equal? (style-markedup-text "*entire title italics*") '((em "entire title italics")))
  (check-equal? (style-markedup-text "multiple *italics* sections *in* title") '("multiple " (em "italics") " sections " (em "in") " title"))
  )

(define/contract (cite-ibid id #:pinpoint [pinpoint #f] #:chapter-first-page [chapter-first-page #f]
                            #:parenthetical [parenthetical #f] #:judge [judge #f] #:speaker [speaker #f] #:signal [signal #f] #:terminal [terminal "."])
  (((and/c string? declared-id?)) (#:pinpoint (or/c string? #f) #:chapter-first-page (or/c string? #f) #:parenthetical (or/c string? #f)
                                   #:judge (or/c string? #f) #:speaker (or/c string? #f)
                                   #:signal (or/c string? #f) #:terminal string?) . ->* . txexpr?)
  (define c-pinpoint (clean-param pinpoint))
  (define c-chapter-first-page (clean-param chapter-first-page))
  (define c-parenthetical (clean-param parenthetical))
  (define c-judge (clean-param judge))
  (define c-speaker (clean-param speaker))
  (define c-signal (clean-param signal))
  (define w (hash-ref work-metadata (clean-param id)))
  `(span [[class "bibliography-entry"] [data-citation-id ,(clean-param id)]]
         ,@(merge-successive-strings
            `(
              ,@(when-or-empty c-signal `(,c-signal " "))
              ,(if c-signal `(em "ibid") `(em "Ibid"))
              ,@(when-or-empty c-chapter-first-page `(", " ,c-chapter-first-page))
              ,@(when-or-empty c-parenthetical `(" (" ,c-parenthetical))
              ,@(when-or-empty c-pinpoint `(,(normalize-pinpoint c-pinpoint)))
              ,@(when-or-empty c-judge `(", " ,c-judge))
              ,@(when-or-empty c-parenthetical '(")"))
              ,@(when-or-empty c-speaker `(" (" ,c-speaker ")")) ; Only relevant for debates (TODO: consider specializing back-reference forms).
              ,terminal))))

(module+ test
  (test-begin
   (declare-work #:id "persons" #:type "legal-case"
                 #:title "Edwards v Canada (AG)" #:year "1929" #:citation "[1930] AC 124"
                 #:parallel-citation "1929 UKPC 86" #:short-form "*Persons Case*")
   (check-equal? (get-elements (cite-ibid "persons"))
                 `((em "Ibid") "."))))

(define/contract (cite-supra id back-ref #:pinpoint [pinpoint #f] #:chapter-first-page [chapter-first-page #f] #:parenthetical [parenthetical #f]
                             #:judge [judge #f] #:speaker [speaker #f] #:signal [signal #f] #:terminal [terminal "."])
  (((and/c string? declared-id?) exact-nonnegative-integer?) (#:pinpoint (or/c string? #f) #:chapter-first-page (or/c string? #f) #:parenthetical (or/c string? #f)
                                                              #:judge (or/c string? #f) #:speaker (or/c string? #f)
                                                              #:signal (or/c string? #f) #:terminal string?) . ->* . txexpr?)
  (define c-pinpoint (clean-param pinpoint))
  (define c-chapter-first-page (clean-param chapter-first-page))
  (define c-parenthetical (clean-param parenthetical))
  (define c-judge (clean-param judge))
  (define c-speaker (clean-param speaker))
  (define c-signal (clean-param signal))
  (define w (hash-ref work-metadata (clean-param id)))
  `(span [[class "bibliography-entry"] [data-citation-id ,(clean-param id)]]
         ,@(merge-successive-strings `(
                                       ,@(when-or-empty c-signal `(,c-signal " "))
                                       ,@(if (list? (hash-ref w 'short-form)) (hash-ref w 'short-form) `(,(hash-ref w 'short-form))) ", "
                                       (em "supra") ,(format " note ~a" back-ref)
                                       ,@(when-or-empty c-chapter-first-page `(", " ,c-chapter-first-page))
                                       ,@(when-or-empty c-parenthetical `(" (" ,c-parenthetical))
                                       ,@(when-or-empty c-pinpoint `(,(normalize-pinpoint c-pinpoint)))
                                       ,@(when-or-empty c-judge `(", " ,c-judge))
                                       ,@(when-or-empty c-parenthetical '(")"))
                                       ,@(when-or-empty c-speaker `(" (" ,c-speaker ")"))
                                       ,terminal))))

(module+ test
  (test-begin
   (declare-work #:id "secession" #:type "legal-case"
                 #:title "Reference re Secession of Quebec" #:citation "[1998] 2 SCR 217"
                 #:short-form "*Secession Reference*")
   (check-equal? (get-elements (cite-supra "secession" 2 #:pinpoint "219"))
                 `((em "Secession Reference") ", " (em "supra") " note 2 at 219."))
   (declare-work #:id "BC PPPA" #:type "bill" #:number "2"
                 #:title "Protection of Public Participation Act" #:legislative-body "4th Sess, 41st Leg, British Columbia"
                 #:year "2019" #:short-form "BC *PPPA*")
   (check-equal? (get-elements (cite-supra "BC PPPA" 1))
                 `("BC " (em "PPPA") ", " (em "supra") " note 1."))))

; Renders a full note-form of the work, which will possibly be replaced
; later by an ibid or supra if necessary.
(define/contract (cite id #:pinpoint [pinpoint #f] #:parenthetical [parenthetical #f] #:judge [judge #f] #:speaker [speaker #f]
                       #:signal [signal #f] #:terminal [terminal "."] #:format-authors-for-bibliography [bib-authors #f])
  (((and/c string? declared-id?)) (#:pinpoint (or/c string? #f) #:parenthetical (or/c string? #f)
                                   #:judge (or/c string? #f) #:speaker (or/c string? #f)
                                   #:signal (or/c string? #f) #:terminal string? #:format-authors-for-bibliography boolean?) . ->* . txexpr?)
  (define c-pinpoint (clean-param pinpoint))
  (define c-parenthetical (clean-param parenthetical))
  (define c-judge (clean-param judge))
  (define c-speaker (clean-param speaker))
  (define c-signal (clean-param signal))
  (define w (hash-ref work-metadata (clean-param id)))
  `(span [[class "bibliography-entry full-form-citation"]
          [data-citation-id ,(clean-param id)]
          [data-citation-pinpoint ,(if c-pinpoint c-pinpoint "false")]
          [data-citation-parenthetical ,(if c-parenthetical c-parenthetical "false")]
          [data-citation-judge ,(if c-judge c-judge "false")]
          [data-citation-speaker ,(if c-speaker c-speaker "false")]
          [data-citation-signal ,(if c-signal c-signal "false")]
          [data-citation-terminal ,terminal]
          ]
         ,@(merge-successive-strings
            `(
              ,@(when-or-empty c-signal `(,(format "~a " c-signal)))
              ,@(case (hash-ref w 'type)
                  [("article") (render-article-elements w c-pinpoint c-parenthetical bib-authors)]
                  [("chapter") (render-chapter-elements w c-pinpoint c-parenthetical bib-authors)]
                  [("book") (render-book-elements w c-pinpoint c-parenthetical bib-authors)]
                  [("thesis") (render-thesis-elements w c-pinpoint c-parenthetical bib-authors)]
                  [("proceedings") (render-proceedings-elements w c-pinpoint c-parenthetical bib-authors)]
                  [("unpublished") (render-unpublished-elements w c-pinpoint c-parenthetical bib-authors)]
                  [("legal-case") (render-legal-case-elements w c-pinpoint c-parenthetical c-judge)]
                  [("legal-case-US") (render-legal-case-US-elements w c-pinpoint c-parenthetical c-judge)]
                  [("bill") (render-bill-elements w c-pinpoint c-parenthetical)]
                  [("statute") (render-statute-elements w c-pinpoint c-parenthetical)]
                  [("regulation") (render-regulation-elements w c-pinpoint c-parenthetical)]
                  [("debate") (render-debate-elements w c-pinpoint c-speaker)]
                  [("magazine/news") (render-magazine/news-elements w c-pinpoint c-parenthetical bib-authors)]
                  [("custom") (render-custom-elements w c-pinpoint c-parenthetical c-judge)]
                  [else  (raise-user-error "No implementation for rendering this type of citation: " (hash-ref w 'type))])
              ,terminal))))

(define/contract (bib-entry id)
  ((and/c string? declared-id?) . -> . txexpr?)
  (cite id #:format-authors-for-bibliography #t))

(define/contract (bib-sort-value id)
  ((and/c string? declared-id?) . -> . string?)
  (define w (hash-ref work-metadata id))
  (define type (hash-ref w 'type))
  (case type
    [("article" "chapter" "book" "thesis" "proceedings" "unpublished" "magazine/news") (format-authors w #t)]
    [("debate") (hash-ref w 'proceedings)]
    [("custom") (hash-ref w 'custom-format)]
    [else (hash-ref w 'title)]))

(define/contract (short-form id)
  ((and/c string? declared-id?) . -> . txexpr-elements?)
  (hash-ref (get-work-by-id id) 'short-form))

(module+ test
  (test-begin
   (declare-work #:id "jordan" #:type "legal-case" #:title "R v Jordan" #:short-form "*Jordan*" #:citation "2016 SCC 27")
   (check-equal? (short-form "jordan") '((em "Jordan")))))
   ; TODO: Make this the behaviour instead. Legal cases should default to italics for short form if there is no
   ; formatting markup provided.
   ; (declare-work #:id "jordan" #:type "legal-case" #:title "R v Jordan" #:short-form "Jordan" #:citation "2016 SCC 27")
   ; (check-equal? (short-form "jordan") '((em "Jordan")))))

(define/contract (format-authors w [bibliography-formatted? #f])
  ((hash?) (boolean?) . ->* . string?)
  (define multiple-authors? (hash-ref w 'author2-given #f))
  (string-append
   (if (hash-ref w 'author-institutional #f)
       (hash-ref w 'author-institutional #f)
       (string-append
        (if bibliography-formatted? (hash-ref w 'author-family) (hash-ref w 'author-given))
        (if bibliography-formatted? ", " " ")
        (if bibliography-formatted? (hash-ref w 'author-given) (hash-ref w 'author-family))))
   (if (and (hash-ref w 'author2-family #f) (not (hash-ref w 'author3-family #f))) " & " "")
   (if (and (hash-ref w 'author2-family #f) (hash-ref w 'author3-family #f)) ", " "")
   (if (hash-ref w 'author2-given #f) (hash-ref w 'author2-given #f) "")
   (if (hash-ref w 'author2-family #f) (format " ~a" (hash-ref w 'author2-family #f)) "")
   (if (hash-ref w 'author3-family #f) " & " "")
   (if (hash-ref w 'author3-given #f) (hash-ref w 'author3-given #f) "")
   (if (hash-ref w 'author3-family #f) (format " ~a" (hash-ref w 'author3-family #f)) "")
   (if (string-is-affirmative? (hash-ref w 'etal? "")) " et al" "")
   (if (string-is-affirmative? (hash-ref w 'editors? "")) ", ed" "")
   (if (and (string-is-affirmative? (hash-ref w 'editors? "")) multiple-authors?) "s" "")))

(module+ test
  (check-equal? (format-authors (hash 'author-given "Sancho" 'author-family "McCann")) "Sancho McCann")
  (check-equal? (format-authors (hash 'author-given "Sancho" 'author-family "McCann"
                                      'author2-given "David G" 'author2-family "Lowe")) "Sancho McCann & David G Lowe")
  (check-equal? (format-authors (hash 'author-given "Natasha" 'author-family "Novac"
                                      'author2-given "Bailey" 'author2-family "Fox"
                                      'author3-given "Nora" 'author3-family "Parker")) "Natasha Novac, Bailey Fox & Nora Parker")
  (check-equal? (format-authors (hash 'author-given "Sancho" 'author-family "McCann" 'etal? "yes")) "Sancho McCann et al")
  (check-equal? (format-authors (hash 'author-given "Colleen M" 'author-family "Flood"
                                      'author2-given "Lorne" 'author2-family "Sossin" 'editors? "yes"))
                "Colleen M Flood & Lorne Sossin, eds")
  (check-equal? (format-authors (hash 'author-institutional "UNESCO")) "UNESCO"))

(define/contract (format-url url)
  (string? . -> . string?)
  (string-append
   ", online"
   (if (or (string-contains? url "youtu.be")
           (string-contains? url "youtube.com"))
       " (video)"
       "")
   ": <"
   (strip-http/https-protocol url)
   ">"))

(module+ test
  (check-equal? (format-url "https://youtu.be/ndLcvzA6S3I") ", online (video): <youtu.be/ndLcvzA6S3I>"))

(define (short-form-pre-placeholder id)
  `(span [[data-short-form-pre-placeholder ,id]]))

; -----------------------------------------------------------------------------------
; These are all the functions that do the citation layout.

(define/contract (render-article-elements w pinpoint parenthetical bib-authors)
  (hash? (or/c string? #f) (or/c string? #f) boolean? . -> . txexpr-elements?)
  (define title-elements (style-markedup-text (hash-ref w 'title)))
  (define fragmented
    `(,(format-authors w bib-authors)
      ", “"
      ,@(if (hash-ref w 'url) `((a [[href ,(hash-ref w 'url)]] ,@title-elements)) title-elements)
      "”"
      ,@(when-or-empty (hash-ref w 'comment-info) `(", " ,(hash-ref w 'comment-info) ", "))
      ,@(when-or-empty (hash-ref w 'year) `(" (" ,(hash-ref w 'year) ") "))
      ,(hash-ref w 'volume)
      ,@(when-or-empty (hash-ref w 'issue) `(":" ,(hash-ref w 'issue)))
      " "
      ,(hash-ref w 'journal)
      ,@(when-or-empty (hash-ref w 'forthcoming) `(" [forthcoming in " ,(hash-ref w 'forthcoming) "]"))
      ,@(when-or-empty (hash-ref w 'first-page) `(" " ,(hash-ref w 'first-page)))
      ,@(when-or-empty (and (not parenthetical) pinpoint) `(,(normalize-pinpoint pinpoint)))
      ,@(when-or-empty (and (hash-ref w 'display-url?) (hash-ref w 'url)) `(,(format-url (hash-ref w 'url))))
      ,(short-form-pre-placeholder (hash-ref w 'id))
      ,@(when-or-empty parenthetical `(" (" ,parenthetical))
      ,@(when-or-empty (and parenthetical pinpoint) `(,(normalize-pinpoint pinpoint)))
      ,@(when-or-empty parenthetical '(")"))))
  (merge-successive-strings fragmented))

(module+ test
  (test-begin
   (declare-work #:id "id1" #:type "article" #:author "Sancho McCann" #:title "Title" #:journal "Journal" #:volume "1" #:year "2018" #:short-form "McCann, \"Title\"")
   (check-equal? (get-elements (cite "id1")) '("Sancho McCann, “Title” (2018) 1 Journal" (span [[data-short-form-pre-placeholder "id1"]]) "."))
   (declare-work #:id "id2" #:type "article" #:author "Sancho McCann" #:title "Title 2" #:journal "Journal" #:volume "1" #:issue "2" #:pages "501--503" #:year "2018" #:short-form "McCann, \"Title 2\"")
   (check-equal? (get-elements (cite "id2")) '("Sancho McCann, “Title 2” (2018) 1:2 Journal 501" (span [[data-short-form-pre-placeholder "id2"]]) "."))
   (declare-work #:id "kamara" #:type "article" #:author "Alvin Kamara" #:title "Title" #:journal "Journal" #:volume "1" #:year "2018" #:url "https://www.nfl.com")
   (check-equal? (get-elements (cite "kamara")) '("Alvin Kamara, “" (a [[href "https://www.nfl.com"]] "Title") "” (2018) 1 Journal" (span [[data-short-form-pre-placeholder "kamara"]]) "."))
   (declare-work #:id "lorde" #:type "article" #:author "Audrey Lorde" #:title "Title" #:journal "Journal" #:volume "1" #:year "2018" #:url "https://www.nfl.com" #:display-url? #t)
   (check-equal? (get-elements (cite "lorde")) '("Audrey Lorde, “" (a [[href "https://www.nfl.com"]] "Title") "” (2018) 1 Journal, online: <www.nfl.com>" (span [[data-short-form-pre-placeholder "lorde"]]) "."))
   (declare-work #:id "hohfeld" #:type "article" #:author-given "Wesley Newcomb" #:author-family "Hohfeld"
                 #:title "The Relations between Equity and Law" #:year "1913" #:journal "Mich L Rev" #:volume "11" #:issue "8" #:first-page "537")
   (check-equal? (get-elements (cite "hohfeld"))
                 '("Wesley Newcomb Hohfeld, “The Relations between Equity and Law” (1913) 11:8 Mich L Rev 537" (span [[data-short-form-pre-placeholder "hohfeld"]]) "."))
   (check-equal? (get-elements (cite "hohfeld" #:pinpoint "page 538"))
                 '("Wesley Newcomb Hohfeld, “The Relations between Equity and Law” (1913) 11:8 Mich L Rev 537 at 538" (span [[data-short-form-pre-placeholder "hohfeld"]]) "."))
   (check-equal? (get-elements (cite "hohfeld" #:pinpoint "page 538" #:parenthetical "on the history of the Chancery Courts in England"))
                 '("Wesley Newcomb Hohfeld, “The Relations between Equity and Law” (1913) 11:8 Mich L Rev 537" (span [[data-short-form-pre-placeholder "hohfeld"]]) " (on the history of the Chancery Courts in England at 538)."))))

(define/contract (render-chapter-elements w pinpoint parenthetical bib-authors)
  (hash? (or/c string? #f) (or/c string? #f) boolean? . -> . txexpr-elements?)
  (define title-elements (style-markedup-text (hash-ref w 'title)))
  (define fragmented
    `(
      ,(format-authors w bib-authors)
      ", “"
      ,@(if (hash-ref w 'url) `((a [[href ,(hash-ref w 'url)]] ,@title-elements)) title-elements)
      "”"
      ,(short-form-pre-placeholder (hash-ref w 'id))
      " in "
      (span [[class "bibliography-entry full-form-citation"]
             [data-citation-id ,(hash-ref w 'in-book)]
             [data-citation-pinpoint "false"]
             [data-citation-chapter-first-page ,(hash-ref w 'first-page)]
             [data-citation-parenthetical "false"]
             [data-citation-judge "false"]
             [data-citation-speaker "false"]
             [data-citation-signal "false"]
             [data-citation-terminal ""]
             ]
            ,@(render-book-elements (get-work-by-id (hash-ref w 'in-book)) #f #f #f (hash-ref w 'first-page)))
      ,@(when-or-empty pinpoint `(,(normalize-pinpoint pinpoint)))
      ))
  (merge-successive-strings fragmented))

(module+ test
  (test-begin
   (declare-work #:id "Flood" #:type "book" #:title "Administrative Law in Context"
                 #:author-given "Colleen M" #:author-family "Flood" #:author2-given "Lorne" #:author2-family "Sossin" #:editors? "yes"
                 #:year "2018" #:edition "3rd" #:publisher "Emond" #:publisher-location "Toronto")
   (declare-work #:id "Wildeman" #:type "chapter" #:title "Making Sense of Reasonableness" #:author "Sheila Wildeman"
                 #:in-book "Flood" #:first-page "437")
   (check-equal? (get-elements (cite "Wildeman" #:pinpoint "443"))
                 '("Sheila Wildeman, “Making Sense of Reasonableness”"
                   (span [[data-short-form-pre-placeholder "Wildeman"]])
                   " in "
                   (span ((class "bibliography-entry full-form-citation")
                          (data-citation-id "Flood")
                          (data-citation-pinpoint "false")
                          (data-citation-chapter-first-page "437")
                          (data-citation-parenthetical "false")
                          (data-citation-judge "false")
                          (data-citation-speaker "false")
                          (data-citation-signal "false")
                          (data-citation-terminal ""))
                         "Colleen M Flood & Lorne Sossin, eds, "
                         (em "Administrative Law in Context") ", 3rd ed (Toronto: Emond, 2018) 437"
                         (span [[data-short-form-pre-placeholder "Flood"]]))
                   " at 443."))))

; chapter-first-page is a special element, only used when a book is cited as part of a chapter's
; citation
(define/contract (render-book-elements w pinpoint parenthetical bib-authors [chapter-first-page #f])
  (->* (hash? (or/c string? #f) (or/c string? #f) boolean?) ((or/c string? #f)) txexpr-elements?)
  (define title-elements (style-markedup-text (hash-ref w 'title)))
  ; TODO: Title style for books needs to flip if there is emphasis in the title.
  (define fragmented
    `(
      ,@(when-or-empty (or (hash-ref w 'author-family) (hash-ref w 'author-institutional)) `(,(format-authors w bib-authors) ", "))
      ,@(if (hash-ref w 'url) `((a [[href ,(hash-ref w 'url)]] (em ,@title-elements))) `((em ,@title-elements)))
      ,@(when-or-empty (hash-ref w 'edition) `(", " ,(hash-ref w 'edition) " ed"))
      " ("
      ,@(when-or-empty (hash-ref w 'publisher-location) `(,(hash-ref w 'publisher-location)))
      ,@(when-or-empty (and (hash-ref w 'publisher-location) (hash-ref w 'publisher)) '(": "))
      ,@(when-or-empty (hash-ref w 'publisher) `(,(hash-ref w 'publisher)))
      ,@(when-or-empty (or (hash-ref w 'publisher-location) (hash-ref w 'publisher)) '(", "))
      ,(hash-ref w 'year)
      ")"
      ,@(when-or-empty (and (not parenthetical) pinpoint) `(,(normalize-pinpoint pinpoint)))
      ,@(when-or-empty chapter-first-page `(" " ,chapter-first-page))
      ,(short-form-pre-placeholder (hash-ref w 'id))
      ,@(when-or-empty parenthetical `(" (" ,parenthetical))
      ,@(when-or-empty (and parenthetical pinpoint) `(,(normalize-pinpoint pinpoint)))
      ,@(when-or-empty parenthetical '(")"))))
  (merge-successive-strings fragmented))

(module+ test
  (test-begin
   (declare-work #:id "id3" #:type "book" #:author-given "Monique Mattei" #:author-family "Ferraro"
                 #:author2-given "Eoghan" #:author2-family "Casey"
                 #:title "Investigating Child Exploitation and Pornography: The Internet, the Law and Forensic Science"
                 #:publisher-location "Boston" #:publisher "Elsevier/Academic Press" #:year "2005")
   (check-equal? (get-elements (cite "id3"))
                 '("Monique Mattei Ferraro & Eoghan Casey, "
                   (em "Investigating Child Exploitation and Pornography: The Internet, the Law and Forensic Science")
                   " (Boston: Elsevier/Academic Press, 2005)" (span [[data-short-form-pre-placeholder "id3"]]) "."))
   (check-equal? (get-elements (cite "id3" #:pinpoint "page ix"))
              '("Monique Mattei Ferraro & Eoghan Casey, "
                (em "Investigating Child Exploitation and Pornography: The Internet, the Law and Forensic Science")
                " (Boston: Elsevier/Academic Press, 2005) at ix" (span [[data-short-form-pre-placeholder "id3"]]) "."))
   (check-equal? (get-elements (cite "id3" #:pinpoint "page ix" #:parenthetical "\"some quotation\""))
              '("Monique Mattei Ferraro & Eoghan Casey, "
                (em "Investigating Child Exploitation and Pornography: The Internet, the Law and Forensic Science")
                " (Boston: Elsevier/Academic Press, 2005)" (span [[data-short-form-pre-placeholder "id3"]]) " (\"some quotation\" at ix)."))))

(define/contract (render-thesis-elements w pinpoint parenthetical bib-authors)
  (hash? (or/c string? #f) (or/c string? #f) boolean? . -> . txexpr-elements?)
  (define title-elements (style-markedup-text (hash-ref w 'title)))
  (define fragmented
    `(
      ,(format-authors w bib-authors)
      ", "
      ,@(if (hash-ref w 'url) `((a [[href ,(hash-ref w 'url)]] (em ,@title-elements))) `((em ,@title-elements)))
      " ("
      ,(hash-ref w 'thesis-description) ", "
      ,(hash-ref w 'institution) ", "
      ,(hash-ref w 'year)
      ")"
      ,@(when-or-empty (and (not parenthetical) pinpoint) `(,(normalize-pinpoint pinpoint)))
      " [unpublished]"
      ,(short-form-pre-placeholder (hash-ref w 'id))
      ,@(when-or-empty parenthetical `(" (" ,parenthetical))
      ,@(when-or-empty (and parenthetical pinpoint) `(,(normalize-pinpoint pinpoint)))
      ,@(when-or-empty parenthetical '(")"))))
  (merge-successive-strings fragmented))

(module+ test
  (test-begin
   (declare-work #:id "id-thesis" #:type "thesis" #:author "Julie Desrosiers"
                 #:title "L'isolement, le retrait et l'arrêt d'agir dans les centre de réadaptation pour jeunes"
                 #:thesis-description "DCL Thesis" #:institution "McGill University Institute of Comparative Law" #:year "2005")
   (check-equal? (get-elements (cite "id-thesis"))
                 '("Julie Desrosiers, "
                   (em "L'isolement, le retrait et l'arrêt d'agir dans les centre de réadaptation pour jeunes")
                   " (DCL Thesis, McGill University Institute of Comparative Law, 2005) [unpublished]"
                   (span [[data-short-form-pre-placeholder "id-thesis"]]) "."))))

(define/contract (render-proceedings-elements w pinpoint parenthetical bib-authors)
  (hash? (or/c string? #f) (or/c string? #f) boolean? . -> . txexpr-elements?)
  (define title-elements (style-markedup-text (hash-ref w 'title)))
  (define fragmented
    `(
      ,(format-authors w bib-authors)
      ", “"
      ,@(if (hash-ref w 'url) `((a [[href ,(hash-ref w 'url)]] ,@title-elements)) title-elements)
      "” in "
      (em ,(hash-ref w 'proceedings))
      " ("
      ,@(when-or-empty (hash-ref w 'publisher-location) `(,(hash-ref w 'publisher-location)))
      ,@(when-or-empty (and (hash-ref w 'publisher-location) (hash-ref w 'publisher)) '(": "))
      ,@(when-or-empty (hash-ref w 'publisher) `(,(hash-ref w 'publisher)))
      ,@(when-or-empty (or (hash-ref w 'publisher-location) (hash-ref w 'publisher)) '(", "))
      ,(hash-ref w 'year)
      ")"
      ,@(when-or-empty (hash-ref w 'first-page) `(" " ,(hash-ref w 'first-page)))
      ,@(when-or-empty (and (not parenthetical) pinpoint) `(,(normalize-pinpoint pinpoint)))
      ,(short-form-pre-placeholder (hash-ref w 'id))
      ,@(when-or-empty parenthetical `(" (" ,parenthetical))
      ,@(when-or-empty (and parenthetical pinpoint) `(,(normalize-pinpoint pinpoint)))
      ,@(when-or-empty parenthetical '(")"))))
  (merge-successive-strings fragmented))

(module+ test
  (test-begin
   (declare-work #:id "id-proceedings" #:type "proceedings" #:author "Sancho McCann"
                 #:author2-given "David G" #:author2-family "Lowe"
                 #:title "Spatially-local coding for object recognition"
                 #:proceedings "11th Asian Conference on Computer Vision" #:year "2012"
                 #:first-page "204" #:publisher "Springer")
   (check-equal? (get-elements (cite "id-proceedings"))
                 '("Sancho McCann & David G Lowe, “Spatially-local coding for object recognition” in "
                   (em "11th Asian Conference on Computer Vision")
                   " (Springer, 2012) 204"
                   (span [[data-short-form-pre-placeholder "id-proceedings"]]) "."))))

(define/contract (render-unpublished-elements w pinpoint parenthetical bib-authors)
  (hash? (or/c string? #f) (or/c string? #f) boolean? . -> . txexpr-elements?)
  (define title-elements (style-markedup-text (hash-ref w 'title)))
  (merge-successive-strings
   `(
     ,(format-authors w bib-authors)
     ", “"
     ,@(if (hash-ref w 'url) `((a [[href ,(hash-ref w 'url)]] ,@title-elements)) title-elements)
     "” ("
     ,@(when-or-empty (hash-ref w 'description) `(,(hash-ref w 'description) ", "))
     ,(hash-ref w 'year)
     ")"
     ,@(when-or-empty (and (not parenthetical) pinpoint) `(,(normalize-pinpoint pinpoint)))
     " [unpublished]"
     ,(short-form-pre-placeholder (hash-ref w 'id))
     ,@(when-or-empty parenthetical `(" (" ,parenthetical))
     ,@(when-or-empty (and parenthetical pinpoint) `(,(normalize-pinpoint pinpoint)))
     ,@(when-or-empty parenthetical '(")")))))

(module+ test
  (test-begin
   (declare-work #:id "McCann" #:type "unpublished"
                 #:author "Sancho McCann" #:title "Atmospheric Sounding Visualization"
                 #:description "course report, Information Visualization, Department of Computer Science, University of British Columbia"
                 #:year "2006")
   (check-equal? (get-elements (cite "McCann"))
                 `("Sancho McCann, “Atmospheric Sounding Visualization” (course report, Information Visualization, Department of Computer Science, University of British Columbia, 2006) [unpublished]"
                   (span [[data-short-form-pre-placeholder "McCann"]]) "."))))


(define/contract (smush-jurisdiction jurisdiction)
  (string? . -> . string?)
  (define (all-caps? str)
    (regexp-match-exact? #rx"([A-Z ]*)" str))
  (if (all-caps? jurisdiction)
      (string-replace jurisdiction " " "")
      jurisdiction))

(module+ test
  (test-begin
   (check-equal? (smush-jurisdiction "ON CA") "ONCA")
   (check-equal? (smush-jurisdiction "ON Ct J (Gen Div)") "ON Ct J (Gen Div)")))

(define/contract (render-legal-case-elements w pinpoint parenthetical judge)
  (hash? (or/c string? #f) (or/c string? #f) (or/c string? #f) . -> . txexpr-elements?)
  (define url (hash-ref w 'url))
  (define title (hash-ref w 'title))
  (define fragmented
    `(
      (em ,(if url `(a [[href ,url]] ,title) title))
      ,@(when-or-empty (or (year-is-necessary? (hash-ref w 'citation))
                           (year-is-different? w)) `(" (" ,(hash-ref w 'year) ")"))
      ", "
      ,(hash-ref w 'citation)
      ,@(when-or-empty (and (hash-ref w 'jurisdiction) (not (hash-ref w 'parallel-citation))) `(" (" ,(smush-jurisdiction (hash-ref w 'jurisdiction)) ")"))
      ,@(when-or-empty (and (not parenthetical) pinpoint) `(,(normalize-pinpoint pinpoint)))
      ; Note that the pinpoint is attached to the main citation.
      ,@(when-or-empty (hash-ref w 'parallel-citation) `(", " ,(hash-ref w 'parallel-citation)))
      ,@(when-or-empty (and (hash-ref w 'jurisdiction) (hash-ref w 'parallel-citation)) `(" (" ,(smush-jurisdiction (hash-ref w 'jurisdiction)) ")"))
      ,@(when-or-empty (hash-ref w 'case-judge) `(", " ,(hash-ref w 'case-judge)))
      ,(short-form-pre-placeholder (hash-ref w 'id))
      ,@(when-or-empty parenthetical `(" (" ,parenthetical))
      ,@(when-or-empty (and parenthetical pinpoint) `(,(normalize-pinpoint pinpoint)))
      ,@(when-or-empty (and parenthetical judge) `(", " ,judge))
      ,@(when-or-empty parenthetical '(")"))))
  (merge-successive-strings fragmented))

(module+ test
  (test-begin
   (declare-work #:id "Fisher" #:type "legal-case"
                 #:title "Fisher v Fisher" #:citation "2008 ONCA 11" #:short-form "Fisher")
   (check-equal? (get-elements (cite "Fisher" #:pinpoint "paras 52--59"))
                 '((em "Fisher v Fisher") ", 2008 ONCA 11 at paras 52--59"
                                          (span [[data-short-form-pre-placeholder "Fisher"]]) "."))
   (declare-work #:id "Gordon" #:type "legal-case"
                 #:title "Gordon v Goertz" #:citation "[1996] 2 SCR 27" #:parallel-citation "134 DLR (4th) 321"
                 #:short-form "Gordon")
   (check-equal? (get-elements (cite "Gordon" #:pinpoint "para 13"))
                 '((em "Gordon v Goertz") ", [1996] 2 SCR 27 at para 13, 134 DLR (4th) 321"
                                          (span [[data-short-form-pre-placeholder "Gordon"]]) "."))
   (declare-work #:id "Neudorf" #:type "legal-case" #:title "Neudorf v Nettwerk Productions Ltd"
                 #:citation "[2000] 3 WWR 522" #:parallel-citation "3 CPR (4th) 129"
                 #:jurisdiction "BC SC" #:short-form "Neudorf")
   (check-equal? (get-elements (cite "Neudorf"))
                 '((em "Neudorf v Nettwerk Productions Ltd")
                   ", [2000] 3 WWR 522, 3 CPR (4th) 129 (BCSC)"
                   (span [[data-short-form-pre-placeholder "Neudorf"]]) "."))
   (declare-work #:id "Delrina" #:type "legal-case"
                 #:title "Delrina Corp v Triolet Systems Inc"
                 #:citation "58 OR (3d) 339" #:jurisdiction "ON CA"
                 #:year "2002" #:short-form "*Delrina*")
   (check-equal? (get-elements (cite "Delrina" #:pinpoint "paras 48--52"))
                 '((em "Delrina Corp v Triolet Systems Inc")
                   " (2002), 58 OR (3d) 339 (ONCA) at paras 48--52"
                    (span [[data-short-form-pre-placeholder "Delrina"]]) "."))

   ; See 3.6.2 for "cited to" rules; these are not context-free. They depend on whether
   ; the first occurrence had a pinpoint and whether the work is ever pinpointed later.
   ; However, it is clear from the examples at 1.3.7 that the requirements for cited-to
   ; are not well specified in the case of a parenthetical with a pinpoint in a citation's
   ; only use. Here, I adopt the more inclusive alternative: specify cited-to when there is a
   ; parallel citation with a parenthetical that includes a pinpoint, even if it is the only
   ; occurrence of the citation in the document. But really, cited-to should not be necessary,
   ; since McGill says you must always cite to the primary, first-listed citation.
   ; See 1.3.7 for parenthetical rules and this particular example.
   ; See 3.10 for judge rules.
   (check-exn exn:fail?
              (λ ()
                (declare-work #:id "Oakwood" #:type "legal-case"
                              #:title "Oakwood Development Ltd v St François Xavier (Municipality)"
                              #:citation "[1985] 2 SCR 164" #:parallel-citation "20 DLR (4th) 641" #:cited-to "DLR"
                              #:short-form "Oakwood"))) ; cited-to must be to the primary citation, if at all
   (declare-work #:id "Oakwood" #:type "legal-case"
                 #:title "Oakwood Development Ltd v St François Xavier (Municipality)"
                 #:citation "[1985] 2 SCR 164" #:parallel-citation "20 DLR (4th) 641"
                 #:case-judge "Wilson J" #:cited-to "SCR"
                 #:short-form "Oakwood")
   (check-equal? (get-elements (cite "Oakwood" #:pinpoint "174"
                                     #:parenthetical "\"[t]he failure of an administrative decision-maker\""))
                 '((em "Oakwood Development Ltd v St François Xavier (Municipality)")
                   ", [1985] 2 SCR 164, 20 DLR (4th) 641, Wilson J"
                   (span [[data-short-form-pre-placeholder "Oakwood"]])
                   " (\"[t]he failure of an administrative decision-maker\" at 174)."))
   (declare-work #:id "persons2" #:type "legal-case"
                 #:title "Edwards v Canada (AG)" #:year "1929" #:citation "[1930] AC 124"
                 #:parallel-citation "1929 UKPC 86" #:short-form "*Persons Case 2*")
   (check-equal? (get-elements (cite "persons2"))
                 `((em "Edwards v Canada (AG)") " (1929), [1930] AC 124, 1929 UKPC 86"
                                                (span [[data-short-form-pre-placeholder "persons2"]]) "."))))
; TODO: Add tests that check whether cited-to is properly being added to the short-forms.

(define/contract (render-legal-case-US-elements w pinpoint parenthetical judge)
  (hash? (or/c string? #f) (or/c string? #f) (or/c string? #f) . -> . txexpr-elements?)
  (define url (hash-ref w 'url))
  (define title (hash-ref w 'title))
  (define fragmented
    `(
      (em ,(if url `(a [[href ,url]] ,title) title))
      ", "
      ,(hash-ref w 'citation)
      ,@(when-or-empty pinpoint `(,(normalize-pinpoint pinpoint)))
      " ("
      ,@(when-or-empty (hash-ref w 'jurisdiction) `(,(hash-ref w 'jurisdiction) " "))
      ,(hash-ref w 'year)
      ")"
      ,@(when-or-empty (and (not parenthetical) judge) `(", " ,judge))
      ,(short-form-pre-placeholder (hash-ref w 'id))
      ,@(when-or-empty parenthetical `(" (" ,parenthetical))
      ,@(when-or-empty (and parenthetical judge) `(", " ,judge))
      ,@(when-or-empty parenthetical '(")"))))
  (merge-successive-strings fragmented))

(module+ test
  (test-begin
   (declare-work #:id "Texas Beef" #:type "legal-case-US"
                 #:title "Texas Beef Group v Winfrey" #:citation "11 F Supp (2d) 858"
                 #:url "https://law.justia.com/cases/federal/district-courts/FSupp2/11/858/2289177/"
                 #:jurisdiction "ND Tex" #:year "1998" #:short-form "Texas Beef")
   (check-equal? (get-elements (cite "Texas Beef"))
                 '((em (a [[href "https://law.justia.com/cases/federal/district-courts/FSupp2/11/858/2289177/"]] "Texas Beef Group v Winfrey"))
                   ", 11 F Supp (2d) 858 (ND Tex 1998)"
                   (span [[data-short-form-pre-placeholder "Texas Beef"]]) "."))))

(define/contract (render-bill-elements w pinpoint parenthetical)
  (hash? (or/c string? #f) (or/c string? #f) . -> . txexpr-elements?)
  (define url (hash-ref w 'url))
  (define title (hash-ref w 'title))
  (define fragmented
    `(
      "Bill "
      ,(hash-ref w 'number)
      ", "
      (em ,(if url `(a [[href ,url]] ,title) title)) ", "
      ,(hash-ref w 'legislative-body) ", "
      ,(hash-ref w 'year)
      ,@(when-or-empty pinpoint `(,(normalize-pinpoint pinpoint)))
      ,@(when-or-empty (hash-ref w 'bill-status) `(" (" ,(hash-ref w 'bill-status) ")"))
      ,@(when-or-empty (hash-ref w 'eventual-statute) `(", " ,(hash-ref w 'eventual-statute)))
      ,(short-form-pre-placeholder (hash-ref w 'id))))
  (merge-successive-strings fragmented))

(module+ test
  (test-begin
   (declare-work #:id "C-26" #:type "bill" #:number "C-26"
                 #:title "An Act to establish the Canada Border Services Agency"
                 #:legislative-body "1st Sess, 38th Parl" #:year "2005"
                 #:bill-status "as passed by the House of Commons 13 June 2005")
   (check-equal? (get-elements (cite "C-26" #:pinpoint "clause 5(1)(e)"))
                 `("Bill C-26, " (em "An Act to establish the Canada Border Services Agency")
                                 ", 1st Sess, 38th Parl, 2005, cl 5(1)(e) (as passed by the House of Commons 13 June 2005)"
                                 (span [[data-short-form-pre-placeholder "C-26"]]) "."))
   (declare-work #:id "Bill 59" #:type "bill" #:number "59"
                 #:title "An Act to amend the Civil Code as regards marriage"
                 #:legislative-body "1st Sess, 37th Leg, Quebec" #:year "2004"
                 #:bill-status "assented to 10 November 2004" #:eventual-statute "SQ 2004, c 23")
   (check-equal? (get-elements (cite "Bill 59"))
                 `("Bill 59, " (em "An Act to amend the Civil Code as regards marriage")
                               ", 1st Sess, 37th Leg, Quebec, 2004 (assented to 10 November 2004), SQ 2004, c 23"
                               (span [[data-short-form-pre-placeholder "Bill 59"]]) "."))))

(define/contract (render-statute-elements w pinpoint parenthetical)
  (hash? (or/c string? #f) (or/c string? #f) . -> . txexpr-elements?)
  (define url (hash-ref w 'url))
  (define title (hash-ref w 'title))
  (define fragmented
    `(
      (em ,(if url `(a [[href ,url]] ,title) title))
      ", "
      ,(hash-ref w 'volume) " "
      ,(hash-ref w 'year) ", "
      "c " ,(hash-ref w 'chapter)
      ,@(when-or-empty pinpoint `(,(normalize-pinpoint pinpoint)))
      ,@(when-or-empty parenthetical `(" (" ,parenthetical ")"))
      ,(short-form-pre-placeholder (hash-ref w 'id))))
  (merge-successive-strings fragmented))

(module+ test
  (test-begin
   (declare-work #:id "Criminal Code" #:type "statute" #:title "Criminal Code"
                 #:volume "RSC" #:year "1985" #:chapter "C-46")
   (check-equal? (get-elements (cite "Criminal Code" #:pinpoint "Section 745"))
                 `((em "Criminal Code") ", RSC 1985, c C-46, s 745"
                                        (span [[data-short-form-pre-placeholder "Criminal Code"]]) "."))
   (declare-work #:id "ITA" #:type "statute" #:title "Income Tax Act"
                 #:volume "RSC" #:year "1985" #:chapter "1 (5th Supp)")
   (check-equal? (get-elements (cite "ITA" #:pinpoint "Section 18(1)(m)(iv)(c)"))
                 `((em "Income Tax Act") ", RSC 1985, c 1 (5th Supp), s 18(1)(m)(iv)(c)"
                                         (span [[data-short-form-pre-placeholder "ITA"]]) "."))))

; TODO: Fill out the remainder of regulation formats from across Canada, dependent on the volume name.
(define/contract (format-canada-regulation w pinpoint parenthetical)
  (hash? (or/c string? #f) (or/c string? #f) . -> . string?)
  (first (merge-successive-strings (case (hash-ref w 'volume)
                                     [("CRC") `("CRC, c "
                                                ,(hash-ref w 'chapter)
                                                ,@(when-or-empty pinpoint `(,(normalize-pinpoint pinpoint)))
                                                " (" ,(hash-ref w 'year) ")")]
                                     [("SOR") `("SOR/"
                                                ; If the year is before 2000, only use the final two digits.
                                                ,(if (>= (string->number (hash-ref w 'year)) 2000)
                                                     (hash-ref w 'year)
                                                     (substring (hash-ref w 'year) 2))
                                                "-" ,(hash-ref w 'number)
                                                ,@(when-or-empty pinpoint `(,(normalize-pinpoint pinpoint))))]
                                     [("BC Reg") `("BC Reg "
                                                   ,(hash-ref w 'number) "/" ,(hash-ref w 'year)
                                                   ,@(when-or-empty pinpoint `(,(normalize-pinpoint pinpoint))))]
                                     [else (raise-user-error "Regulation volume not yet supported: " (hash-ref w 'volume))]))))

(define/contract (render-regulation-elements w pinpoint parenthetical)
  (hash? (or/c string? #f) (or/c string? #f) . -> . txexpr-elements?)
  (define url (hash-ref w 'url))
  (define title (hash-ref w 'title))
  (define fragmented
    `(
      (em ,(if url `(a [[href ,url]] ,title) title))
      ", "
      ,(format-canada-regulation w pinpoint parenthetical)
      ,(short-form-pre-placeholder (hash-ref w 'id))))
  (merge-successive-strings fragmented))

(module+ test
  (test-begin
   (declare-work #:id "BDRs" #:type "regulation" #:title "Broadcasting Distribution Regulations"
                 #:volume "SOR" #:year "1997" #:number "555")
   (check-equal? (get-elements (cite "BDRs" #:pinpoint "Section 2"))
                 `((em "Broadcasting Distribution Regulations") ", SOR/97-555, s 2"
                                                                (span [[data-short-form-pre-placeholder "BDRs"]]) "."))
   (declare-work #:id "LCLR-bugged" #:type "regulation" #:title "Liquor Control and Licensing Regulation"
                 #:volume "BC Reeg" #:year "2016" #:number "241" #:short-form "LCLR")
   (check-exn exn:fail? (λ () (cite "LCLR-bugged")))
   (declare-work #:id "LCLR" #:type "regulation" #:title "Liquor Control and Licensing Regulation"
                 #:volume "BC Reg" #:year "2016" #:number "241" #:short-form "Liquor Regulations")
   (check-equal? (get-elements (cite "LCLR" #:pinpoint "section 9"))
                 `((em "Liquor Control and Licensing Regulation") ", BC Reg 241/2016, s 9"
                                                                  (span [[data-short-form-pre-placeholder "LCLR"]]) "."))
   (declare-work #:id "MBRs" #:type "regulation" #:title "Migratory Birds Regulations"
                 #:volume "CRC" #:chapter "1035" #:year "1978")
   (check-equal? (get-elements (cite "MBRs" #:pinpoint "s 4"))
                 `((em "Migratory Birds Regulations") ", CRC, c 1035, s 4 (1978)"
                                                      (span [[data-short-form-pre-placeholder "MBRs"]]) "."))))

(define/contract (render-debate-elements w pinpoint speaker)
  (hash? (or/c string? #f) (or/c string? #f) . -> . txexpr-elements?)
  (define url (hash-ref w 'url))
  (define title (hash-ref w 'title))
  (define doc-string-elements
    `(,@(when-or-empty (and (hash-ref w 'jurisdiction) (not (equal? (hash-ref w 'jurisdiction) "Canada")))
                       `(,(hash-ref w 'jurisdiction) ", "))
      (em ,(hash-ref w 'proceedings)) ", "
      ,(hash-ref w 'legislative-body) ", "
      ,@(when-or-empty (hash-ref w 'volume) `(,(hash-ref w 'volume)))))
  (define fragmented
    `(
      ,@(when-or-empty title `("“" ,title "”, "))
      ,@(when-or-empty (hash-ref w 'reading) `(,(hash-ref w 'reading) ", "))
      ,@(if url `((a [[href ,url]] ,@doc-string-elements)) doc-string-elements)
      " (" ,(hash-ref w 'year) ")"
      ,@(when-or-empty pinpoint `(,(normalize-pinpoint pinpoint)))
      ,@(when-or-empty speaker `(" (" ,speaker ")"))
      ,(short-form-pre-placeholder (hash-ref w 'id))))
  (merge-successive-strings fragmented))

(module+ test
  (test-begin
   (declare-work #:id "debate-1" #:type "debate" #:jurisdiction "Canada"
                 #:proceedings "House of Commons Debates" #:legislative-body "37-1" #:volume "No 64"
                 #:date "17 May 2001" #:short-form "Debate: 17 May 2001")
   (check-equal? (get-elements (cite "debate-1" #:pinpoint "4175" #:speaker "Hon Elinor Caplan"))
                 `((em "House of Commons Debates") ", 37-1, No 64 (17 May 2001) at 4175 (Hon Elinor Caplan)"
                                                   (span [[data-short-form-pre-placeholder "debate-1"]]) "."))
   (declare-work #:id "debate-2" #:type "debate" #:jurisdiction "Quebec, National Assembly"
                 #:proceedings "Votes and Proceedings" #:legislative-body "39-1" #:volume "No 48"
                 #:date "18 June 2009" #:short-form "Debate: 18 June 2009")
   (check-equal? (get-elements (cite "debate-2" #:pinpoint "517"))
                 `("Quebec, National Assembly, " (em "Votes and Proceedings")
                                                 ", 39-1, No 48 (18 June 2009) at 517"
                                                 (span [[data-short-form-pre-placeholder "debate-2"]]) "."))
   (declare-work #:id "debate-3" #:type "debate" #:jurisdiction "Canada"
                 #:title "Bill C-8, An Act to amend the Copyright Act" #:reading "2nd reading"
                 #:proceedings "House of Commons Debates" #:legislative-body "41-2" #:volume "No 9"
                 #:date "28 October 2013" #:short-form "Copyright Debate")
   (check-equal? (get-elements (cite "debate-3" #:pinpoint "1504" #:speaker "Hon Steven Blaney"))
                 `("“Bill C-8, An Act to amend the Copyright Act”, 2nd reading, "
                   (em "House of Commons Debates")
                   ", 41-2, No 9 (28 October 2013) at 1504 (Hon Steven Blaney)"
                   (span [[data-short-form-pre-placeholder "debate-3"]]) "."))))

(define/contract (render-magazine/news-elements w pinpoint parenthetical bib-authors)
  (hash? (or/c string? #f) (or/c string? #f) boolean? . -> . txexpr-elements?)
  (define url (hash-ref w 'url))
  (define title-elements (style-markedup-text (hash-ref w 'title)))
  ; Note, title is the only required element.
  (define fragmented
    `(
      ,@(when-or-empty (hash-ref w 'author-family) `(,(format-authors w bib-authors) ", "))
      "“"
      ,@(if url `((a [[href ,url]] ,@title-elements)) title-elements) "”"
      ,@(when-or-empty (hash-ref w 'publication) `(", " (em ,(hash-ref w 'publication))))
      ,@(when-or-empty (hash-ref w 'volume) `(" " ,(hash-ref w 'volume)))
      ,@(when-or-empty (hash-ref w 'issue) `(":" ,(hash-ref w 'issue)))
      ,@(when-or-empty (hash-ref w 'year) `(" (" ,(hash-ref w 'year) ")"))
      ,@(when-or-empty (hash-ref w 'first-page) `(" " ,(hash-ref w 'first-page)))
      ,@(when-or-empty (and (not parenthetical) pinpoint) `(,(normalize-pinpoint pinpoint)))
      ,@(when-or-empty (and (hash-ref w 'display-url?) (hash-ref w 'url)) `(,(format-url (hash-ref w 'url))))
      ,(short-form-pre-placeholder (hash-ref w 'id))
      ,@(when-or-empty parenthetical `(" (" ,parenthetical))
      ,@(when-or-empty (and parenthetical pinpoint) `(,(normalize-pinpoint pinpoint)))
      ,@(when-or-empty parenthetical '(")"))))
  (merge-successive-strings fragmented))

(module+ test
  (test-begin
   (declare-work #:id "Clones" #:type "magazine/news"
                 #:title "The Case Against Clones" #:publication "The Economist" #:date "2 February 2013"
                 #:url "https://www.economist.com")
   (check-equal? (get-elements (cite "Clones"))
                 `("“" (a [[href "https://www.economist.com"]] "The Case Against Clones") "”, " (em "The Economist") " (2 February 2013)"
                       (span [[data-short-form-pre-placeholder "Clones"]]) "."))
   (declare-work #:id "Phelan" #:type "magazine/news" #:author "Benjamin Phelan"
                 #:title "Buried Truths" #:publication "Harper's Magazine" #:volume "309" #:issue "1855"
                 #:date "December 2004" #:first-page "70")
   (check-equal? (get-elements (cite "Phelan"))
                 `("Benjamin Phelan, “Buried Truths”, " (em "Harper's Magazine") " 309:1855 (December 2004) 70"
                                                        (span [[data-short-form-pre-placeholder "Phelan"]]) "."))))

(define/contract (render-custom-elements w pinpoint parenthetical judge)
  (hash? (or/c string? #f) (or/c string? #f) (or/c string? #f) . -> . txexpr-elements?)
  `(
    ,@(style-markedup-text (string-replace (hash-ref w 'custom-format) "[[pinpoint]]" (if pinpoint (normalize-pinpoint pinpoint) "")))
    ,(short-form-pre-placeholder (hash-ref w 'id))
    ,@(when-or-empty (and (hash-ref w 'display-url?) (hash-ref w 'url)) `(,(format-url (hash-ref w 'url))))
    ))

(module+ test
  (test-begin
   (declare-work #:id "custom" #:type "custom"
                 #:custom-format "Custom-formatted citation" #:short-form "Custom")
   (check-equal? (get-elements (cite "custom"))
                 `("Custom-formatted citation" (span [[data-short-form-pre-placeholder "custom"]]) "."))
   (declare-work
    #:id "charter" #:type "custom"
    #:custom-format "*Canadian Charter of Rights and Freedoms*[[pinpoint]], Part I of the *Constitution Act, 1982*, being Schedule B to the *Canada Act 1982* (UK), 1982, c 11"
    #:short-form "Charter")
   (check-equal? (get-elements (cite "charter"))
                 `((em "Canadian Charter of Rights and Freedoms")
                   ", Part I of the "
                   (em "Constitution Act, 1982")
                   ", being Schedule B to the "
                   (em "Canada Act 1982")
                   " (UK), 1982, c 11"
                   (span [[data-short-form-pre-placeholder "charter"]])
                   "."))
   (check-equal? (get-elements (cite "charter" #:pinpoint "Section 7"))
                 `((em "Canadian Charter of Rights and Freedoms")
                   ", s 7, Part I of the "
                   (em "Constitution Act, 1982")
                   ", being Schedule B to the "
                   (em "Canada Act 1982")
                   " (UK), 1982, c 11"
                   (span [[data-short-form-pre-placeholder "charter"]])
                   "."))))

(define/contract (short-form-differs-from-a-default? id)
  (string? . -> . boolean?)
  (define work (hash-ref work-metadata id))
  (define short-form (hash-ref work 'short-form))
  (or (default-short-form-undetermined? work) (not (equal? short-form (default-short-form work)))))

(module+ test
  (test-begin
   (declare-work #:id "newton" #:type "book" #:author "Cam Newton"
                 #:title "How to recover a fumble" #:short-form "Newton" #:year "2016")
   (check-false (short-form-differs-from-a-default? "newton"))
   (declare-work #:id "wm2" #:type "legal-case"
                 #:title "Overseas Tankship (UK) v Miller Steamship Co" #:year "1966"
                 #:citation "[1967] 1 AC 617" #:parallel-citation "[1966] 2 All ER 709"
                 #:short-form "*Wagon Mound No 2*")
   (check-true (short-form-differs-from-a-default? "wm2"))))

; Sweeps through the content, replacing any data-short-form-pre-placeholder with data-short-form-placeholder.
; You should do this only in the note context because if a work is just "cited" (i.e. rendered inline) not in a footnote or sidenote,
; then it isn't part of the reference-counting/back-reference (ibid/supra) system.
; This turns a citation's short-form placeholder into one that will actually be useable.
(define/contract (transform-short-form-placeholder tx)
  (txexpr? . -> . txexpr?)
  (if (attrs-have-key? tx 'data-short-form-pre-placeholder)
      (txexpr (get-tag tx) `[[data-short-form-placeholder ,(attr-ref tx 'data-short-form-pre-placeholder)]] empty)
      tx))

; Collects reference-count and first-reference info and transforms subsequent references into supra/ibid forms.
(define/contract (transform-full-cites-into-backrefs tx footnote-number)
  (txexpr? exact-nonnegative-integer? . -> . txexpr?)

  (define (extract-from-our-custom-data-attrs tx key)
    (define value (attr-ref tx key "false"))
    (if (equal? value "false") #f value))

  (if (and (attrs-have-key? tx 'class)
           (string-contains? (attr-ref tx 'class) "full-form-citation"))
      (let* ([id (attr-ref tx 'data-citation-id)]
             [first-cite (if (hash-has-key? first-place-cited id) (hash-ref first-place-cited id) #f)]
             [ibid (and first-cite (equal? (car most-recent-ibid-or-supra) id)
                        (or (equal? (- footnote-number 1) (cdr most-recent-ibid-or-supra))
                            (equal? footnote-number (cdr most-recent-ibid-or-supra))))])
        (when (and first-cite (not ibid)) (hash-set! short-form-used? id #t))
        (when (not (hash-has-key? first-place-cited id)) (hash-set! first-place-cited id footnote-number))
        (set! most-recent-ibid-or-supra (cons id footnote-number))
        ; If this work was previous cited, take its full-form citation and replace it with an ibid or supra.
        (if ibid (cite-ibid id
                            #:pinpoint (extract-from-our-custom-data-attrs tx 'data-citation-pinpoint)
                            #:chapter-first-page (extract-from-our-custom-data-attrs tx 'data-citation-chapter-first-page)
                            #:parenthetical (extract-from-our-custom-data-attrs tx 'data-citation-parenthetical)
                            #:judge (extract-from-our-custom-data-attrs tx 'data-citation-judge)
                            #:speaker (extract-from-our-custom-data-attrs tx 'data-citation-speaker)
                            #:signal (extract-from-our-custom-data-attrs tx 'data-citation-signal)
                            #:terminal (extract-from-our-custom-data-attrs tx 'data-citation-terminal))
            ; If ibid was not appropriate, but there is a first-cite, then supra must be required.
            (if first-cite (cite-supra id first-cite
                                       #:pinpoint (extract-from-our-custom-data-attrs tx 'data-citation-pinpoint)
                                       #:chapter-first-page (extract-from-our-custom-data-attrs tx 'data-citation-chapter-first-page)
                                       #:parenthetical (extract-from-our-custom-data-attrs tx 'data-citation-parenthetical)
                                       #:judge (extract-from-our-custom-data-attrs tx 'data-citation-judge)
                                       #:speaker (extract-from-our-custom-data-attrs tx 'data-citation-speaker)
                                       #:signal (extract-from-our-custom-data-attrs tx 'data-citation-signal)
                                       #:terminal (extract-from-our-custom-data-attrs tx 'data-citation-terminal))
                tx)))
      tx))

; This does the transformations that are needed for any cites found within a note context.
(define/contract (transform-cite-in-a-note tx footnote-number)
  (txexpr? exact-nonnegative-integer? . -> . txexpr?)
  (transform-short-form-placeholder (transform-full-cites-into-backrefs tx footnote-number)))

; To be called during your Pollen module's final decode.
; This sweeps through all short-form placeholders (which are empty spans before this point)
; and inserts the work's short-form if it was ever cited a second time.
(define/contract (show-necessary-short-forms tx)
  (txexpr? . -> . txexpr?)
  (define (short-form-needed? id)
    (and (hash-ref short-form-used? id #f)
         (short-form-differs-from-a-default? id)))
  (if (and (attrs-have-key? tx 'data-short-form-placeholder)
           (short-form-needed? (attr-ref tx 'data-short-form-placeholder)))
      (txexpr (get-tag tx) (get-attrs tx) `(" [" ,@(hash-ref (get-work-by-id (attr-ref tx 'data-short-form-placeholder)) 'short-form) "]"))
      tx))
