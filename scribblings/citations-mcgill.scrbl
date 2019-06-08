#lang scribble/manual

@(require (for-label racket
                     txexpr
                     "../citations-mcgill.rkt"))

@title{McGill-style citations for Pollen}
@author{Sancho McCann}

@defmodule[pollen/citations-mcgill]

This module provides
@hyperlink["https://store.thomsonreuters.ca/product-detail/canadian-guide-to-uniform-legal-citation-9th-edition-manuel-canadien-de-la-reference-juridique-9e-edition-mcgill-guide-hc-print/"]{
 McGill-style citation management} for use with the Pollen
module. It lets you:

@itemlist[@item{declare works by providing their metadata (title, author, etc.)}
          @item{produce tagged x-expressions with the citations formatted according to the @emph{McGill guide}}
          @item{manage back-references to previous instances of a citation within a document}]

This requires the end-user/author to use
@code[#:lang "pollen"]|{◊declare-work}| within their Pollen
source code. It requires your Pollen project to call two
transformer functions (provided by this module). This is in order
that this module be able to manage reference tracking and
back-references for any citations that are rendered within
that footnote context.

@section{Example}

You can see this working at @url{
 https://github.com/sanchom/sanchom.github.io}. @filepath{
 down-the-foxhole.html.pm} is a good example of declaring the
works and using them later within notes. @filepath{
 pollen.rkt} demonstrates the required integration calls
described in @secref["integration"].

@section{User/author tags}

These are tags/functions that the ultimate author of a document will use in their source text.

@defproc[(declare-work [#:type type string? #f]
                       [#:title title (or/c string? #f) #f]
                       [#:author author (or/c string? #f) #f]
                       [#:author-given author-given (or/c string? #f) #f]
                       [#:author-family author-family (or/c string? #f) #f]
                       [#:author2-given author2-given (or/c string? #f) #f]
                       [#:author2-family author2-family (or/c string? #f) #f]
                       [#:author3-given author3-given (or/c string? #f) #f]
                       [#:author3-family author3-family (or/c string? #f) #f]
                       [#:journal journal (or/c string? #f) #f]
                       [#:year year (or/c string? #f) #f] ; alias for "date" --- incompatible with date
                       [#:date date (or/c string? #f) #f] ; alias for "year" --- incompatible with year
                       [#:volume volume (or/c string? #f)  #f]
                       [#:publication publication (or/c string? #f) #f] ; for magazine/news
                       [#:issue issue (or/c string? #f) #f]
                       [#:citation citation (or/c string? #f) #f]
                       [#:jurisdiction jurisdiction (or/c string? #f) #f]
                       [#:institution institution (or/c string? #f) #f]
                       [#:legislative-body legislative-body (or/c string? #f) #f]
                       [#:number number (or/c string? #f) #f]
                       [#:chapter chapter (or/c string? #f) #f] ; for statutes
                       [#:reading reading (or/c string? #f) #f] ; for legislative debates
                       [#:proceedings proceedings (or/c string? #f) #f]
                       [#:publisher publisher (or/c string? #f) #f]
                       [#:publisher-location publisher-location (or/c string? #f) #f]
                       [#:thesis-description thesis-description (or/c string? #f) #f]
                       [#:description description (or/c string? #f) #f]
                       [#:comment-info comment-info (or/c string? #f) #f]
                       [#:forthcoming forthcoming (or/c string? #f) #f]
                       [#:pages pages (or/c string? #f) #f] ; will extract the first-page from this; incompatible with first-page
                       [#:first-page first-page (or/c string? #f) #f]
                       [#:url url (or/c string? #f) #f]
                       [#:short-form short-form (or/c string? #f) #f]
                       [#:id id (or/c string? #f) #f]
                       [#:and-render? and-render? (or/c string? boolean?) #f]) (or/c void? txexpr?)]{

 If @racket[id] is specified, then this call returns
 @racket[void] and this work can be cited to later using
 @racket[cite].

 If @racket[id] is not specified, then the author will never
 be able to cite this work using @racket[cite]. The call to
 @racket[declare-work] will also render a formatted citation
 in-place, unless @racket[and-render?] equals @racket["no"].

 @racket[type] must be one of "article", "thesis",
 "proceedings", "unpublished", "legal-case", "legal-case-US",
 "bill", "statute", "debate", "book", "magazine/news".

 Depending on the @racket[type] of the work, the subsequent
 fields that are meaningful or mandatory will differ. For
 example, if @racket[type] is @racket["thesis"], then
 @racket[thesis-description] is actually meaningful. If
 @racket[type] is @racket["legal-case"], then
 @racket[citation] is mandatory. Some fields are incompatible
 with each other. For example, if @racket[author] is
 specified, then @racket[author-given] and
 @racket[author-family] must not also be specified (and vice
 versa). These various restrictions are best presented
 through examples.

 [TODO: document what fields are meaningful for each type of work.]

}

@defproc[(cite [id string?]
               [#:pinpoint pinpoint (or/c string? #f) #f]
               [#:parenthetical parenthetical (or/c string? #f) #f]
               [#:judge judge (or/c string? #f) #f]
               [#:speaker speaker (or/c string? #f) #f]
               [#:signal signal (or/c string? #f) #f])
         txexpr?]{

 Produces a @racket[txexpr] with a formatted citation. The
 content of the citation is that which was provided via
 @racket[declare-work], supplemented by the specific
 arguments in the call to @racket[cite].

 @racket[pinpoint] needs to be something like "page 6", "at
 para 1", "clause 7", etc.

 @racket[parenthetical] is a short quote or explanation no
 longer than one sentence.

 @racket[judge] is the name of the judge.

 @racket[speaker] is the name of the speaker in the case of
 a legislative debate or other transcript.

 @racket[signal] is an introductory signal to prepend to the
 citation ("See", "See also", "See generally", etc.)

 Only when a call to @racket[cite] is within your Pollen
 system's note-tag context will the resulting citation be
 fully managed by this citation system. If you call
 @racket[cite] just inline in a paragraph, this citation
 system will just leave you a formatted citation in place.
 Such a citation will not affect reference counting and will
 not be subject to transformation into back-references.

}

@section[#:tag "integration"]{Integration with your Pollen tags}

This citation system assumes/requires that you define some
sort of a "note" tag within which citations are treated
specially (that is, managed by this citation system).

You can name that tag whatever you want. For example, if you
name that tag @racket[note], the end-user/author would need
to write:

@codeblock|{
 ◊note{Here is a cited work within a note context. ◊cite["id-string"]}
}|

All citations that the end-user/author places within your
@racket[note] context will be managed by this citation
system, but only if do the following extra work within your
note tag and during the final decode.

@bold{First}, your @racket[note] tag is responsible for
counting the footnote number. One way of doing this is
through a global variable in your @filepath{pollen.rkt} that
is mutated upon each call to @racket[note].

@bold{Second}, transform the entire content of the
@racket[note] with the following code:

@racketblock[
 (define transformed-content
   (decode-elements content
                    #:txexpr-proc (λ (x) (transform-cite-in-a-note x footnote-number))))
 ]

The resulting @racket[transformed-content] is what you
should actually show the user (in a sidenote, footnote,
inline pop-up, or however you've decided to use your
@racket[note] tag).

@bold{Third}, during the final decode, provide the
@racket[show-necessary-short-forms] function as an argument
to @racket[decode] via @racket[#:txexpr-proc]:

@racketblock[
 (decode ...
  #:txexpr-proc (compose1 custom-hyphenation show-necessary-short-forms)
  )
 ]
