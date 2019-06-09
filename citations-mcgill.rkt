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
(define short-form-needed (make-hash))
(define unidentified-work-count 0)

; Accessor
(define/contract (get-work-by-id id)
  (string? . -> . hash?)
  (hash-ref work-metadata (clean-param id)))

(define (declared-id? id)
  (hash-has-key? work-metadata (clean-param id)))

; Citation system. Following the McGill Guide, with Chicago Manual of Style for any ambiguities.
; ----------------------------------------------------------------------------------------------

(define/contract (valid-work-type? type)
  (string? . -> . boolean?)
  (if (member type '("article" "thesis" "proceedings" "unpublished" "legal-case" "legal-case-US"
                               "bill" "statute" "debate" "book" "magazine/news"))
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

(define (pinpoint-requires-at? str) (or (string-prefix? (strip-at str) "para") (pinpoint-is-pages? (strip-at str))))
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

(define/contract (year-is-necessary? citation)
  (string? . -> . boolean?)
  ; Are the first alphanumeric characters in the citation a four-digit year? Ie. Is there a sequence of four consecutive
  ; digits before any letter?
  (not (regexp-match? #px"[[:digit:]]{4}" (first (regexp-match #px"\\S*[[:space:]]" citation)))))

(module+ test
  (check-true (year-is-necessary? "301 DLR (4th) 34"))
  (check-false (year-is-necessary? "[1977] 1 SCR 193"))
  (check-false (year-is-necessary? "2012 SCC 1")))

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
    [("thesis") (validate-thesis w)]
    [("proceedings") (validate-proceedings w)]
    [("unpublished") (validate-unpublished w)]
    [("legal-case") (validate-legal-case w)]
    [("legal-case-US") (validate-legal-case-US w)]
    [("bill") (validate-bill w)]
    [("statute") (validate-statute w)]
    [("debate") (validate-debate w)]
    [("book") (validate-book w)]
    [("magazine/news") (validate-magazine/news w)]
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
  (define (short-form-used? s)
    (ormap (λ (v) (equal? (hash-ref v 'short-form) s)) (hash-values work-metadata)))
  (when (short-form-used? (hash-ref w 'short-form))
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

(define (validate-article w)
  (validate-mandatory-elements "article" w '(title author-family author-given journal volume)))

(define (validate-book w)
  (validate-mandatory-elements "book" w '(title year)))

(define (validate-thesis w)
  (validate-mandatory-elements "thesis" w '(title author-family author-given institution thesis-description year)))

(define (validate-proceedings w)
  (validate-mandatory-elements "proceedings" w '(title author-family author-given proceedings year)))

(define (validate-unpublished w)
  (validate-mandatory-elements "unpublished" w '(title author-family author-given year)))

(define (validate-bill w)
  (validate-mandatory-elements "bill" w '(number title legislative-body year)))

(define (validate-statute w)
  (validate-mandatory-elements "statute" w '(title volume year chapter)))

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

(define (validate-legal-case w)
  (validate-mandatory-elements "legal-case" w '(title citation))
  (when (and (year-is-necessary? (hash-ref w 'citation)) (not (hash-ref w 'year)))
    (raise-user-error "Failed to declare year when year is not the first element of the first citation: " (hash-ref w 'citation))))

(define (validate-legal-case-US w)
  (validate-mandatory-elements "legal-case-US" w '(title citation year)))

(define/contract (make-short-form type author title)
  (valid-work-type? (or/c string? #f) string? . -> . txexpr?)
  (case type
    [("legal-case" "legal-case-US" "statute" "bill") `(em ,title)]
    [("magazine/news") `(span ,(when/splice author author ", “") ,title ,(when/splice author "”"))]
    [else `(span ,(when/splice author author ", “") ,title ,(when/splice author "”"))]))

(define (declare-work #:type type
                      #:id id
                      #:title [title #f]
                      #:author [author #f] ; a shortcut for simple "author-given author-family" names --- incompatible with author-given / author-family
                      #:author-given [author-given #f]
                      #:author-family [author-family #f]
                      #:author2-given [author2-given #f]
                      #:author2-family [author2-family #f]
                      #:author3-given [author3-given #f]
                      #:author3-family [author3-family #f]
                      #:journal [journal #f]
                      #:year [year #f] ; alias for "date" --- incompatible with date
                      #:date [date #f] ; alias for "year" --- incompatible with year
                      #:volume [volume #f]
                      #:publication [publication #f] ; for magazine/news
                      #:issue [issue #f]
                      #:citation [citation #f]
                      #:jurisdiction [jurisdiction #f]
                      #:institution [institution #f]
                      #:legislative-body [legislative-body #f]
                      #:number [number #f]
                      #:chapter [chapter #f] ; for statutes
                      #:reading [reading #f] ; for legislative debates
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
                      #:short-form [short-form #f])
  (define cleaned-id (clean-param id))
  (when (and author (or author-given author-family))
    (raise-user-error "You used #:author and either #:author-given or #:author-family. #:author is a substitute for the latter when the name is simple." `(,author ,author-given ,author-family)))
  (when (and year date)
    (raise-user-error "You specified both a year and a date. Use only one of these." `(,year ,date ,title)))
  (when (and pages first-page)
    (raise-user-error "You specified both pages and first-page. Use only one of these." `(,pages ,first-page)))
  (when (hash-has-key? work-metadata cleaned-id) (raise-user-error "duplicate id" cleaned-id))
  (define w (hash 'type type
                  'id cleaned-id
                  'title (clean-param title)
                  'author-given (if author (get-given-from-author author) (clean-param author-given))
                  'author-family (if author (get-family-from-author author) (clean-param author-family))
                  'author2-given (clean-param author2-given)
                  'author2-family (clean-param author2-family)
                  'author3-given (clean-param author3-given)
                  'author3-family (clean-param author3-family)
                  'journal (clean-param journal)
                  'publication (clean-param publication)
                  'year (if year year date)
                  'volume volume
                  'issue issue
                  'citation (clean-param citation)
                  'jurisdiction (clean-param jurisdiction)
                  'institution (clean-param institution)
                  'legislative-body (clean-param legislative-body)
                  'number (clean-param number)
                  'chapter (clean-param chapter)
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
                  'short-form (if short-form
                                  `(span ,(style-title (clean-param short-form)))
                                  (make-short-form type (if author (get-family-from-author author) (clean-param author-family)) title))))
  (validate-work-or-die w)
  (hash-set! work-metadata cleaned-id w))

; Just forward all arguments to declare-work, but this form does not accept
; an id. It will return a txexpr to be rendered in-place.
; TODO: Find a way to programmatically re-use these arguments.
(define (format-work #:type type
                     #:title [title #f]
                     #:author [author #f] ; a shortcut for simple "author-given author-family" names --- incompatible with author-given / author-family
                     #:author-given [author-given #f]
                     #:author-family [author-family #f]
                     #:author2-given [author2-given #f]
                     #:author2-family [author2-family #f]
                     #:author3-given [author3-given #f]
                     #:author3-family [author3-family #f]
                     #:journal [journal #f]
                     #:year [year #f] ; alias for "date" --- incompatible with date
                     #:date [date #f] ; alias for "year" --- incompatible with year
                     #:volume [volume #f]
                     #:publication [publication #f] ; for magazine/news
                     #:issue [issue #f]
                     #:citation [citation #f]
                     #:jurisdiction [jurisdiction #f]
                     #:institution [institution #f]
                     #:legislative-body [legislative-body #f]
                     #:number [number #f]
                     #:chapter [chapter #f] ; for statutes
                     #:reading [reading #f] ; for legislative debates
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
                     #:short-form [short-form #f])
  (define id (format "unidentified-work-~a" unidentified-work-count))
  (set! unidentified-work-count (+ 1 unidentified-work-count))
  (declare-work #:type type
                #:id id
                #:title title
                #:author author
                #:author-given author-given
                #:author-family author-family
                #:author2-given author2-given
                #:author2-family author2-family
                #:author3-given author3-given
                #:author3-family author3-family
                #:journal journal
                #:year year
                #:date date
                #:volume volume
                #:publication publication
                #:issue issue
                #:citation citation
                #:jurisdiction jurisdiction
                #:institution institution
                #:legislative-body legislative-body
                #:number number
                #:chapter chapter
                #:reading reading
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
                #:short-form short-form)
  (cite id))

(define/contract (style-title markedup-title)
  (string? . -> . txexpr?)
  (define italic-range (regexp-match-positions #rx"\\*.*\\*" markedup-title))
  (if italic-range
      (let* ([before (substring markedup-title 0 (car (car italic-range)))]
             [special-content (substring markedup-title (+ (car (car italic-range)) 1) (- (cdr (car italic-range)) 1))]
             [after (substring markedup-title (cdr (car italic-range)))])
        `(@ ,(when/splice (non-empty-string? before) before)
            (em ,special-content)
            ,(when/splice (non-empty-string? after) after)))
      `(@ ,markedup-title)))

(module+ test
  (check-equal? (style-title "title") (txexpr '@ empty '("title")))
  (check-equal? (style-title "title *with italics*") (txexpr '@ empty '((@ "title ") (em "with italics") (@))))
  (check-equal? (style-title "*italics* at start of title") (txexpr '@ empty '((@) (em "italics") (@ " at start of title"))))
  (check-equal? (style-title "*entire title italics*") (txexpr '@ empty '((@) (em "entire title italics") (@))))
  ; TODO (check-equal? (style-title "multiple *italics* sections *in* title") (txexpr '@ empty '("multiple " (em "italics") " sections " (em "in") " title")))
  )

(define/contract (cite-ibid id #:pinpoint [pinpoint #f] #:parenthetical [parenthetical #f] #:judge [judge #f] #:speaker [speaker #f] #:signal [signal #f])
  (((and/c string? declared-id?)) (#:pinpoint (or/c string? #f) #:parenthetical (or/c string? #f)
                                   #:judge (or/c string? #f) #:speaker (or/c string? #f) #:signal (or/c string? #f)) . ->* . txexpr?)
  (define c-pinpoint (clean-param pinpoint))
  (define c-parenthetical (clean-param parenthetical))
  (define c-judge (clean-param judge))
  (define c-speaker (clean-param speaker))
  (define c-signal (clean-param signal))
  (define w (hash-ref work-metadata (clean-param id)))
  `(span [[class "bibliography-entry"] [data-citation-id ,(clean-param id)]]
         ,(when/splice c-signal c-signal " ")
         ,(if c-signal `(em "ibid") `(em "Ibid"))
         ,(when/splice c-parenthetical " (" c-parenthetical)
         ,(when/splice c-pinpoint (normalize-pinpoint c-pinpoint))
         ,(when/splice c-judge ", " c-judge)
         ,(when/splice c-parenthetical ")")
         ,(when/splice c-speaker " (" c-speaker ")") ; Only relevant for debates (TODO: consider specializing back-reference forms).
         "."))

(define/contract (cite-supra id back-ref #:pinpoint [pinpoint #f] #:parenthetical [parenthetical #f] #:judge [judge #f] #:speaker [speaker #f] #:signal [signal #f])
  (((and/c string? declared-id?) exact-nonnegative-integer?) (#:pinpoint (or/c string? #f) #:parenthetical (or/c string? #f)
                                                              #:judge (or/c string? #f) #:speaker (or/c string? #f) #:signal (or/c string? #f)) . ->* . txexpr?)
  (define c-pinpoint (clean-param pinpoint))
  (define c-parenthetical (clean-param parenthetical))
  (define c-judge (clean-param judge))
  (define c-speaker (clean-param speaker))
  (define c-signal (clean-param signal))
  (define w (hash-ref work-metadata (clean-param id)))
  `(span [[class "bibliography-entry"] [data-citation-id ,(clean-param id)]]
         ,(when/splice c-signal c-signal " ")
         ,(hash-ref w 'short-form) ", "
         (em "supra") ,(format " note ~a" back-ref)
         ,(when/splice c-parenthetical " (" c-parenthetical)
         ,(when/splice c-pinpoint (normalize-pinpoint c-pinpoint))
         ,(when/splice c-judge ", " c-judge)
         ,(when/splice c-parenthetical ")")
         ,(when/splice c-speaker " (" c-speaker ")")
         "."))

; Renders a full note-form of the work, which will possibly be replaced
; later by an ibid or supra if necessary.
(define/contract (cite id #:pinpoint [pinpoint #f] #:parenthetical [parenthetical #f] #:judge [judge #f] #:speaker [speaker #f] #:signal [signal #f])
  (((and/c string? declared-id?)) (#:pinpoint (or/c string? #f) #:parenthetical (or/c string? #f)
                                   #:judge (or/c string? #f) #:speaker (or/c string? #f) #:signal (or/c string? #f)) . ->* . txexpr?)
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
          ]
         ,(when/splice c-signal (format "~a " c-signal))
         ,(case (hash-ref w 'type)
            [("article") (render-article w c-pinpoint c-parenthetical)]
            [("book") (render-book w c-pinpoint c-parenthetical)]
            [("thesis") (render-thesis w c-pinpoint c-parenthetical)]
            [("proceedings") (render-proceedings w c-pinpoint c-parenthetical)]
            [("unpublished") (render-unpublished w c-pinpoint c-parenthetical)]
            [("legal-case") (render-legal-case w c-pinpoint c-parenthetical c-judge)]
            [("legal-case-US") (render-legal-case-US w c-pinpoint c-parenthetical c-judge)]
            [("bill") (render-bill w c-pinpoint c-parenthetical)]
            [("statute") (render-statute w c-pinpoint c-parenthetical)]
            [("debate") (render-debate w c-pinpoint c-speaker)]
            [("magazine/news") (render-magazine/news w c-pinpoint c-parenthetical)]
            [else (raise-user-error "No implementation for rendering this type of citation: " (hash-ref w 'type))])))

(define/contract (format-authors w)
  (hash? . -> . txexpr?)
  `(@ ,(string-append (hash-ref w 'author-given)
                     " "
                     (hash-ref w 'author-family)
                     (if (and (hash-ref w 'author2-family #f) (not (hash-ref w 'author3-family #f))) " & " "")
                     (if (and (hash-ref w 'author2-family #f) (hash-ref w 'author3-family #f)) ", " "")
                     (if (hash-ref w 'author2-given #f) (hash-ref w 'author2-given #f) "")
                     (if (hash-ref w 'author2-family #f) (format " ~a" (hash-ref w 'author2-family #f)) "")
                     (if (hash-ref w 'author3-family #f) " & " "")
                     (if (hash-ref w 'author3-given #f) (hash-ref w 'author3-given #f) "")
                     (if (hash-ref w 'author3-family #f) (format " ~a" (hash-ref w 'author3-family #f)) ""))))

(module+ test
  (check-equal? (format-authors (hash 'author-given "Sancho" 'author-family "McCann")) '(@ "Sancho McCann"))
  (check-equal? (format-authors (hash 'author-given "Sancho" 'author-family "McCann"
                                      'author2-given "David G" 'author2-family "Lowe")) '(@ "Sancho McCann & David G Lowe"))
  (check-equal? (format-authors (hash 'author-given "Natasha" 'author-family "Novac"
                                      'author2-given "Bailey" 'author2-family "Fox"
                                      'author3-given "Nora" 'author3-family "Parker")) '(@ "Natasha Novac, Bailey Fox & Nora Parker")))


; -----------------------------------------------------------------------------------
; These are all the functions that do the citation layout.

(define/contract (render-article w pinpoint parenthetical)
  (hash? (or/c string? #f) (or/c string? #f) . -> . txexpr?)
  (define styled-title (style-title (hash-ref w 'title)))
  `(@
    ,(format-authors w)
    ", “"
    ,(if (hash-ref w 'url) `(a [[href ,(hash-ref w 'url)]] ,styled-title) styled-title)
    "”"
    ,(when/splice (hash-ref w 'comment-info) ", " (hash-ref w 'comment-info) ", ")
    ,(when/splice (hash-ref w 'year) " (" (hash-ref w 'year) ") ")
    ,(hash-ref w 'volume)
    ,(when/splice (hash-ref w 'issue) ":" (hash-ref w 'issue))
    " "
    (em ,(hash-ref w 'journal))
    " "
    ,(when/splice (hash-ref w 'forthcoming) " [forthcoming in " (hash-ref w 'forthcoming) "]")
    ,(when/splice (hash-ref w 'first-page) " " (hash-ref w 'first-page))
    (span [[data-short-form-pre-placeholder ,(format "~a" (hash-ref w 'id))]])
    ,(when/splice parenthetical " (" parenthetical)
    ,(when/splice pinpoint (normalize-pinpoint pinpoint))
    ,(when/splice parenthetical ")")
    "."))

; TODO
; (module+ test
;   (test-begin
;    (declare-work #:id "id1" #:type "article" #:author "Sancho McCann" #:title "Title" #:journal "Journal" #:volume "1")
;    (check-equal? (get-elements (cite "id1")) '(@ "Sancho McCann"))))

(define (render-book w pinpoint parenthetical)
  (define styled-title (style-title (hash-ref w 'title)))
  `(@
    ,(when/splice (hash-ref w 'author-family) (format-authors w) ", ")
    ,(if (hash-ref w 'url) `(a [[href ,(hash-ref w 'url)]] (em ,styled-title)) `(em ,styled-title))
    " "
    " ("
    ,(when/splice (hash-ref w 'publisher-location) (hash-ref w 'publisher-location))
    ,(when/splice (and (hash-ref w 'publisher-location) (hash-ref w 'publisher)) ": ")
    ,(when/splice (hash-ref w 'publisher) (hash-ref w 'publisher))
    ,(when/splice (or (hash-ref w 'publisher-location) (hash-ref w 'publisher)) ", ")
    ,(hash-ref w 'year)
    ")"
    (span [[data-short-form-pre-placeholder ,(format "~a" (hash-ref w 'id))]])
    ,(when/splice parenthetical " (" parenthetical)
    ,(when/splice pinpoint (normalize-pinpoint pinpoint))
    ,(when/splice parenthetical ")")
    "."))

(define (render-thesis w pinpoint parenthetical)
  (define styled-title (style-title (hash-ref w 'title)))
  `(@
    ,(format-authors w)
    ", "
    ,(if (hash-ref w 'url) `(a [[href ,(hash-ref w 'url)]] (em ,styled-title)) `(em ,styled-title))
    " ("
    ,(hash-ref w 'thesis-description) ", "
    ,(hash-ref w 'institution) ", "
    ,(hash-ref w 'year)
    ")"
    (span [[data-short-form-pre-placeholder ,(format "~a" (hash-ref w 'id))]])
    ,(when/splice parenthetical " (" parenthetical)
    ,(when/splice pinpoint (normalize-pinpoint pinpoint))
    ,(when/splice parenthetical ")")
    "."
    ))

(define (render-proceedings w pinpoint parenthetical)
  (define styled-title (style-title (hash-ref w 'title)))
  `(@
    ,(format-authors w)
    ", “"
    ,(if (hash-ref w 'url) `(a [[href ,(hash-ref w 'url)]] ,styled-title) styled-title)
    "” in "
    (em ,(hash-ref w 'proceedings))
    " ("
    ,(when/splice (hash-ref w 'publisher-location) (hash-ref w 'publisher-location))
    ,(when/splice (and (hash-ref w 'publisher-location) (hash-ref w 'publisher)) ": ")
    ,(when/splice (hash-ref w 'publisher) (hash-ref w 'publisher))
    ,(when/splice (or (hash-ref w 'publisher-location) (hash-ref w 'publisher)) ", ")
    ,(hash-ref w 'year)
    ")"
    ,(when/splice (hash-ref w 'first-page) " " (hash-ref w 'first-page))
    (span [[data-short-form-pre-placeholder ,(format "~a" (hash-ref w 'id))]])
    ,(when/splice parenthetical " (" parenthetical)
    ,(when/splice pinpoint (normalize-pinpoint pinpoint))
    ,(when/splice parenthetical ")")
    "."))

(define (render-unpublished w pinpoint parenthetical)
  (define styled-title (style-title (hash-ref w 'title)))
  `(@
    ,(format-authors w)
    ", “"
    ,(if (hash-ref w 'url) `(a [[href ,(hash-ref w 'url)]] ,styled-title) styled-title)
    "” ("
    ,(when/splice (hash-ref w 'description) (hash-ref w 'description) ", ")
    ,(hash-ref w 'year)
    ")"
    (span [[data-short-form-pre-placeholder ,(format "~a" (hash-ref w 'id))]])
    ,(when/splice parenthetical " (" parenthetical)
    ,(when/splice pinpoint (normalize-pinpoint pinpoint))
    ,(when/splice parenthetical ")")
    "."))

(define (render-legal-case w pinpoint parenthetical judge)
  (define url (hash-ref w 'url))
  (define title (hash-ref w 'title))
  `(@
    (em ,(if url `(a [[href ,url]] ,title) `(span ,title)))
    ,(when/splice (year-is-necessary? (hash-ref w 'citation)) " (" (hash-ref w 'year) ")")
    ", "
    ,(hash-ref w 'citation)
    ,(when/splice (and (not parenthetical) judge) ", " judge)
    (span [[data-short-form-pre-placeholder ,(format "~a" (hash-ref w 'id))]])
    ,(when/splice parenthetical " (" parenthetical)
    ,(when/splice pinpoint (normalize-pinpoint pinpoint))
    ,(when/splice (and parenthetical judge) ", " judge)
    ,(when/splice parenthetical ")")
    "."))

(define (render-legal-case-US w pinpoint parenthetical judge)
  (define url (hash-ref w 'url))
  (define title (hash-ref w 'title))
  `(@
    (em ,(if url `(a [[href ,url]] ,title) `(span ,title)))
    ", "
    ,(hash-ref w 'citation)
    ,(when/splice pinpoint (normalize-pinpoint pinpoint))
    " ("
    ,(when/splice (hash-ref w 'jurisdiction) (hash-ref w 'jurisdiction) " ")
    ,(hash-ref w 'year)
    ")"
    ,(when/splice (and (not parenthetical) judge) ", " judge)
    (span [[data-short-form-pre-placeholder ,(format "~a" (hash-ref w 'id))]])
    ,(when/splice parenthetical " (" parenthetical)
    ,(when/splice (and parenthetical judge) ", " judge)
    ,(when/splice parenthetical ")")
    "."))

(define (render-bill w pinpoint parenthetical)
  (define url (hash-ref w 'url))
  (define title (hash-ref w 'title))
  `(@
    ,(hash-ref w 'number) ", "
    (em ,(if url `(a [[href ,url]] ,title) `(span ,title))) ", "
    ,(hash-ref w 'legislative-body) ", "
    ,(hash-ref w 'year)
    ,(when/splice pinpoint (normalize-pinpoint pinpoint))
    (span [[data-short-form-pre-placeholder ,(format "~a" (hash-ref w 'id))]])
    "."))

(define (render-statute w pinpoint parenthetical)
  (define url (hash-ref w 'url))
  (define title (hash-ref w 'title))
  `(@
    (em ,(if url `(a [[href ,url]] ,title) `(span ,title))) ", "
    ,(hash-ref w 'volume) " "
    ,(hash-ref w 'year) ", "
    "c " ,(hash-ref w 'chapter)
    ,(when/splice pinpoint (normalize-pinpoint pinpoint))
    ,(when/splice parenthetical " (" parenthetical)
    ,(when/splice parenthetical  ")")
    (span [[data-short-form-pre-placeholder ,(format "~a" (hash-ref w 'id))]])
    "."))

(define (render-debate w pinpoint speaker)
  (define url (hash-ref w 'url))
  (define title (hash-ref w 'title))
  (define doc-string
    `(@ ,(hash-ref w 'jurisdiction) ", "
        (em ,(hash-ref w 'proceedings)) ", "
        ,(hash-ref w 'legislative-body) ", "
        ,(when/splice (hash-ref w 'volume) (hash-ref w 'volume))))
  `(@
    ,(when/splice title "“" title "”, ")
    ,(when/splice (hash-ref w 'reading) (hash-ref w 'reading) ", ")
    ,(if url `(a [[href ,url]] ,doc-string) `(span ,doc-string)) " "
    "(" ,(hash-ref w 'year) ")"
    ,(when/splice pinpoint (normalize-pinpoint pinpoint))
    ,(when/splice speaker " (" speaker ")")
    (span [[data-short-form-pre-placeholder ,(format "~a" (hash-ref w 'id))]])
    "."))

(define (render-magazine/news w pinpoint parenthetical)
  (define url (hash-ref w 'url))
  (define styled-title (style-title (hash-ref w 'title)))
  ; Note, title is the only required element.
  `(@
    ,(when/splice (hash-ref w 'author-family) (format-authors w) ", ")
    "“" ,(if url `(a [[href ,url]] ,styled-title) styled-title) "”"
    ,(when/splice (hash-ref w 'publication) ", " `(em ,(hash-ref w 'publication)))
    ,(when/splice (hash-ref w 'year) " (" (hash-ref w 'year) ")")
    (span [[data-short-form-pre-placeholder ,(format "~a" (hash-ref w 'id))]])
    ,(when/splice parenthetical " (" parenthetical)
    ,(when/splice pinpoint (normalize-pinpoint pinpoint))
    ,(when/splice parenthetical ")")
    "."))

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
    (define value (attr-ref tx key))
    (if (equal? value "false") #f value))

  (if (and (attrs-have-key? tx 'class)
           (string-contains? (attr-ref tx 'class) "full-form-citation"))
      (let* ([id (attr-ref tx 'data-citation-id)]
             [first-cite (if (hash-has-key? first-place-cited id) (hash-ref first-place-cited id) #f)]
             [ibid (and first-cite (equal? (car most-recent-ibid-or-supra) id) (equal? (- footnote-number 1) (cdr most-recent-ibid-or-supra)))])
        (when (and first-cite (not ibid)) (hash-set! short-form-needed id #t))
        (when (not (hash-has-key? first-place-cited id)) (hash-set! first-place-cited id footnote-number))
        (set! most-recent-ibid-or-supra (cons id footnote-number))
        ; If this work was previous cited, take its full-form citation and replace it with an ibid or supra.
        (if ibid (cite-ibid id
                            #:pinpoint (extract-from-our-custom-data-attrs tx 'data-citation-pinpoint)
                            #:parenthetical (extract-from-our-custom-data-attrs tx 'data-citation-parenthetical)
                            #:judge (extract-from-our-custom-data-attrs tx 'data-citation-judge)
                            #:speaker (extract-from-our-custom-data-attrs tx 'data-citation-speaker)
                            #:signal (extract-from-our-custom-data-attrs tx 'data-citation-signal))
            ; If ibid was not appropriate, but there is a first-cite, then supra must be required.
            (if first-cite (cite-supra id first-cite
                                       #:pinpoint (extract-from-our-custom-data-attrs tx 'data-citation-pinpoint)
                                       #:parenthetical (extract-from-our-custom-data-attrs tx 'data-citation-parenthetical)
                                       #:judge (extract-from-our-custom-data-attrs tx 'data-citation-judge)
                                       #:speaker (extract-from-our-custom-data-attrs tx 'data-citation-speaker)
                                       #:signal (extract-from-our-custom-data-attrs tx 'data-citation-signal))
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
    (and (hash-has-key? short-form-needed id)
         (hash-ref short-form-needed id)))
  (if (and (attrs-have-key? tx 'data-short-form-placeholder)
           (short-form-needed? (attr-ref tx 'data-short-form-placeholder)))
      (txexpr (get-tag tx) (get-attrs tx) `(" [" ,(hash-ref (get-work-by-id (attr-ref tx 'data-short-form-placeholder)) 'short-form) "]"))
      tx))
