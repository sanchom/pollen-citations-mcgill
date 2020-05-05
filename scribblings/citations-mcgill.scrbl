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
module. pollen-citations-mcgill lets you:

@itemlist[@item{declare works by providing their metadata (title, author, etc.)}
          @item{produce tagged x-expressions with the citations formatted according to the @emph{McGill Guide}}
          @item{manage back-references to previous instances of a citation within a document (via @emph{ibid} and @emph{supra})}]

This requires the end-user/author to use
@code[#:lang "pollen"]|{◊declare-work}| within their Pollen
source code. It requires your Pollen project (in your @filepath{pollen.rkt} file, usually) to call two
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

I'm also using it for the project at @url{
 https://github.com/sanchom/associate}.

There is also an online demo that you can play with at @url{https://citations-demo.herokuapp.com/}.

@section{User/author tags}

These are tags/functions that the ultimate author of a
document will use in their source text.

You need to provide them through your @filepath{pollen.rkt}
like this:

@racketblock[
 (provide declare-work)
 (provide cite)
 ]

@defproc[(declare-work [#:type type string?]
                       [#:id id string?]
                       [#:title title (or/c string? #f) #f]
                       [#:author author (or/c string? #f) #f]
                       [#:author-institutional author-insitutional (or/c string? #f) #f]
                       [#:author-given author-given (or/c string? #f) #f]
                       [#:author-family author-family (or/c string? #f) #f]
                       [#:author2-given author2-given (or/c string? #f) #f]
                       [#:author2-family author2-family (or/c string? #f) #f]
                       [#:author3-given author3-given (or/c string? #f) #f]
                       [#:author3-family author3-family (or/c string? #f) #f]
                       [#:editors? editors? string? ""]
                       [#:etal? edit? string? ""]
                       [#:in-book in-book (or/c string? #f) #f]
                       [#:journal journal (or/c string? #f) #f]
                       [#:edition edition (or/c string? #f) #f]
                       [#:year year (or/c string? #f) #f]
                       [#:date date (or/c string? #f) #f]
                       [#:volume volume (or/c string? #f) #f]
                       [#:issue issue (or/c string? #f) #f]
                       [#:publication publication (or/c string? #f) #f]
                       [#:citation citation (or/c string? #f) #f]
                       [#:parallel-citation parallel-citation (or/c string? #f) #f]
                       [#:jurisdiction jurisdiction (or/c string? #f) #f]
                       [#:case-judge case-judge (or/c string? #f) #f]
                       [#:institution institution (or/c string? #f) #f]
                       [#:legislative-body legislative-body (or/c string? #f) #f]
                       [#:number number (or/c string? #f) #f]
                       [#:chapter chapter (or/c string? #f) #f]
                       [#:reading reading (or/c string? #f) #f]
                       [#:bill-status bill-status (or/c string? #f) #f]
                       [#:eventual-statute eventual-statute (or/c string? #f) #f]
                       [#:proceedings proceedings (or/c string? #f) #f]
                       [#:publisher publisher (or/c string? #f) #f]
                       [#:publisher-location publisher-location (or/c string? #f) #f]
                       [#:thesis-description thesis-description (or/c string? #f) #f]
                       [#:description description (or/c string? #f) #f]
                       [#:comment-info comment-info (or/c string? #f) #f]
                       [#:forthcoming forthcoming (or/c string? #f) #f]
                       [#:pages pages (or/c string? #f) #f]
                       [#:first-page first-page (or/c string? #f) #f]
                       [#:url url (or/c string? #f) #f]
                       [#:display-url? display-url? string? ""]
                       [#:custom-format custom-format (or/c string? #f) #f]
                       [#:short-form short-form (or/c string? #f) #f]
                       [#:cited-to cited-to (or/c string? #f) #f]) void?]{

 The @racket[id] is the string that users/authors can use to
 cite this work using @racket[cite].

 @racket[type] must be one of "article", "chapter", "book", "thesis",
 "proceedings", "unpublished", "legal-case", "legal-case-US",
 "bill", "statute", "regulation", "debate", "book", "magazine/news", or "custom".

 Depending on the @racket[type] of the work, the subsequent
 fields that are meaningful or mandatory will differ. For
 example, if @racket[type] is @racket["thesis"], then
 @racket[thesis-description] is actually meaningful. If
 @racket[type] is @racket["legal-case"], then
 @racket[citation] is mandatory. Some fields are incompatible
 with each other. For example, if @racket[author] is
 specified, then @racket[author-given] and
 @racket[author-family] are automatically extracted and
 must not also be specified (and vice
 versa). These various restrictions are best presented
 through examples, which are presented next.

 And if none of these are adequate,
 you can always fall back to a custom citation format, but
 this requires you to lay out the citation yourself using the
 @racket[custom-format] argument. This takes a string
 optionally marked up with italicized sections (by
 surrounding them with asterisks).

 @bold{Specifying authors}

 The way to declare an author or authors is the same across all document types.
 You can declare an author's given and family name explicitly:

 @codeblock|{
 ◊declare-work[#:id "Mills" #:type "article"
               #:author-given "Aaron" #:author-family "Mills"
               ...]
 }|

 Or, for the first author, if they only have two parts to their name, you can use
 @code{#:author} as shorthand. The system will assume the first name is the given
 name and that the second name is the family name.
 
 @codeblock|{
 ◊declare-work[#:id "Mills" #:type "article"
               #:author "Aaron Mills"
               ...]
 }|

 For additional authors (up to three), you have to list them like this:

 @codeblock|{
 ◊declare-work[#:id "FSP" #:type "magazine/news"
               #:author "Jennifer Daskal"
               #:author2-given "Danielle Keats" #:author2-family "Citron"
               #:author3-given "Nathaniel" #:author3-family "Gleicher"
               ...]
 }|

 If there are more than three authors, @emph{McGill} style only lists the first,
 followed by @emph{et al}. You can tell the citation system to use @emph{et al}
 like this:

 @codeblock|{
 ◊declare-work[#:id "Lazer" #:type "article"
               #:author-given "David MJ"
               #:author-family "Lazer" #:etal? "yes"
               ...]
 }|

 If the string argument to @code{#:etal} is any kind of affirmative string
 (e.g. @code{"yes"}, @code{"T"}, @code{"t"}, @code{"#t"}, @code{"YeS"}),
 the system will interpret it as a "yes",
 otherwise, it will interpret it as a "no".

 If the people you have listed using the author arguments are actually editors,
 use the @code{#:editors?} argument, and provide an affirmative string.

 In place of a person, you can also specify an institutional author:

 @codeblock|{
 ◊declare-work[#:id "FB" #:type "magazine/news"
               #:author-institutional "Facebook"
               ...]
 }|
 
 @bold{article}

 Author(s), a title, and a journal name are mandatory.

 E.g.:

 @codeblock|{
 ◊declare-work[#:id "DiResta" #:type "article"
               #:author "Renee DiResta"
               #:title "Computational Propaganda: If You Make it Trend, You Make it True"
               #:journal "The Yale Review"
               #:volume "106" #:issue "4"
               #:first-page "12" #:year "2018"]
 }|

 @bold{book}

 Title and year are mandatory.

 E.g.:

 @codeblock|{
 ◊declare-work[#:id "Bilewicz" #:type "book"
               #:author "Michal Bilewicz"
               #:author2-given "Aleksandra" #:author2-family "Cichocka"
               #:author3-given "Wiktor" #:author3-family "Soral"
               #:editors? "yes"
               #:title "The Psychology of Conspiracy"
               #:year "2015" #:publisher "Routledge"
               #:publisher-location "New York"]
 }|

 @bold{chapter}

 Title, in-book (a reference to a book that has also been declared), and first-page are mandatory.

 E.g.:

 @codeblock|{
 ◊declare-work[#:id "Douglas: Chapter" #:type "chapter"
               #:author-given "Karen M" #:author-family "Douglas"
               #:etal? "yes"
               #:title "The Social, Political, Environmental, and
                        Health-Related Consequences of Conspiracy Theories"
               #:in-book "Bilewicz" #:first-page "183"]
 }|

 @bold{legal-case}

 This is for citing Canadian legal cases. The title (style of cause) and citation
 elements are mandatory. If the year is not the first thing to appear in the citation
 string, you must also explicitly include the year.

 E.g. (showing a simple citation using a neutral citation, and a specified short-form;
 in short-forms, the asterisks signify that it is to be rendered italicized):

 @codeblock|{
 ◊declare-work[#:id "Maltz" #:type "legal-case"
               #:title "Maltz v Witterick"
               #:citation "2016 FC 524" #:short-form "*Maltz*"]
 }|
 
 E.g. (showing a parallel citation and explicit jurisdiction information because the
 citations don't reveal the jurisdiction and court):

 @codeblock|{
 ◊declare-work[#:id "Delrina ONCJ" #:type "legal-case"
               #:title "Delrina Corp v Triolet Systems Inc"
               #:citation "[1993] OJ No 319"
               #:parallel-citation "47 CPR (3d) 1"
               #:jurisdiction "ON Ct J (Gen Div)"]
 }|

 E.g. (explicitly declaring the year because it isn't the first part of the citation):

 @codeblock|{
 ◊declare-work[#:id "Delrina" #:type "legal-case"
               #:title "Delrina Corp v Triolet Systems Inc"
               #:citation "58 OR (3d) 339"
               #:jurisdiction "ON CA"
               #:year "2002"
               #:short-form "*Delrina*"]
 }|

 The system will automatically format the jurisdiction according to McGill rules, removing the
 space between all-uppercase jurisdiction strings.

 @bold{legal-case-US}

 The title (style of cause), citation, and year elements are mandatory. Jurisdiction will also
 usually be provided unless citing the Supreme Court through the ◊emph{United States Reports}.

 E.g.:

 @codeblock|{
 ◊declare-work[#:id "Titanic" #:type "legal-case-US"
               #:title "Lindsay v The Wrecked and Abandoned Vessel RMS Titanic"
               #:citation "52 USPQ (2d) 1609"
               #:year "1999" #:jurisdiction "SDNY"
               #:short-form "*Lindsay v RMS Titanic*"]
 }|

 @bold{magazine/news}

 The only mandatory element for this is the title. This citation format
 can also be used for general website citation.

 E.g.:
 
 @codeblock|{
 ◊declare-work[#:id "Serhan" #:type "magazine/news"
               #:title "The Case Against Waging 'war' on the Coronavirus"
               #:author "Yasmeen Serhan"
               #:date "31 March 2020"
               #:publication "The Atlantic"]
 }|

 It's common to specify a URL when you are citing an online news source or website.
 If you want the URL to be displayed in the citation as per a standard McGill URL
 reference, use the @code{#:display-url?} element. If you provide a URL and don't
 use @code{#:display-url?}, then the title will merely be turned into a hyperlink.
 This might be all you want if you're rendering the citation on a blog.

 @codeblock|{
 ◊declare-work[#:id "FSP" #:type "magazine/news"
               #:author "Jennifer Daskal"
               #:author2-given "Danielle Keats" #:author2-family "Citron"
               #:author3-given "Nathaniel" #:author3-family "Gleicher"
               #:publication "New America"
               #:title "Free Speech Project: Confronting Viral Disinformation"
               #:date "26 March 2020"
               #:url "https://youtu.be/a6hFwYxUSxM" #:display-url? "yes"]
 }|

 @bold{statute}

 This is a citation to a published statute (as opposed to a bill). You can use this
 to cite to either Annual Statutes of Canada or Revised Statutes. The title, volume, year,
 and chapter are mandatory elements. Volume is the name of the statute publication (e.g. RSC,
 SC, RSBC).

 E.g.:

 @codeblock|{
 ◊declare-work[#:id "CC" #:type "statute"
               #:title "Criminal Code"
               #:volume "RSC" #:year "1985" #:chapter "C-46"]

 ◊declare-work[#:id "C-25" #:type "statute"
               #:title "An Act to amend the Criminal Code, the Youth Criminal Justice Act
                        and other Acts and to make consequential amendments to other Acts"
               #:volume "SC" #:year "2019" #:chapter "25"]
 }|

 @bold{bill}

 The bill number, title, legislative-body, and year are mandatory elements.

 E.g. (note, this example also shows a user-specified short-form with a mix of italics
 and roman text):

 @codeblock|{
 ◊declare-work[#:id "BC PPPA" #:type "bill"
               #:number "2"
               #:title "Protection of Public Participation Act"
               #:legislative-body "4th Sess, 41st Leg, British Columbia"
               #:year "2019"
               #:url "https://www.leg.bc.ca/content/data%20-%20ldp/Pages/41st4th/1st_read/gov02-1.htm"
               #:short-form "BC *PPPA*"]
 }|

 @bold{regulation}

 @bold{debate}

 @bold{thesis}

 @bold{proceedings}

 @bold{unpublished}

 @bold{custom}

 The @code{custom} type lets you specify a citation that doesn't fit neatly into any of the provided
 categories. This citation system will still manage back-references, using @emph{ibid}, @emph{supra},
 and short-forms. It's just that you will specify the layout of the initial citation.

 E.g.:

 @codeblock|{
 ◊declare-work[#:id "Charter" #:type "custom"
               #:custom-format "*Canadian Charter of Rights and Freedoms*[[pinpoint]],
                                Part I of the *Constitution Act, 1982*, being Schedule B to the
                                *Canada Act 1982* (UK), 1982, c 11"
               #:short-form "*Charter*"]
 }|

 In the custom format, surround text that should appear italicized in asterisks. Indicate the
 position for pinpoints using the @code{[[pinpoint]]} markup.

}

@defproc[(cite [id string?]
               [#:pinpoint pinpoint (or/c string? #f) #f]
               [#:parenthetical parenthetical (or/c string? #f) #f]
               [#:judge judge (or/c string? #f) #f]
               [#:speaker speaker (or/c string? #f) #f]
               [#:signal signal (or/c string? #f) #f]
               [#:terminal terminal string? "."])
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

 You can change the @racket[terminal] to end the citation in
 something other than a "." A common alternative is ";" in
 order to join multiple citations together in a single
 footnote signal as described at McGill 1.3.4.

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
system, but only if you do the following extra work within your
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
