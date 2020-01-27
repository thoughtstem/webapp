#lang scribble/manual
@require[@for-label[webapp
                    racket/base]]

@title{webapp}
@author{thoughtstem}

@defmodule[webapp]

@defmodule[webapp/models/utils]

This reprovides all of @racket[deta] but adds a few new utilities and changes the semantics of a few things (documented below).

@defform[(define-schema name ([info ...] ...))]{
  Same as Deta's, but simpler syntax.  Captures schema details for the purposes of automatic code generation (e.g. scaffolds, predefined functions, etc).
}

@defproc[(insert-one! [model entity?])
         (or/c false/c entity?)]{
   
  A wrapper around Deta's version.  But you do not need to supply a database connection.
}

@defproc[(delete-one! [model entity?])
         (or/c false/c entity?)]{
   
  A wrapper around Deta's version.  But you do not need to supply a database connection.
}

@defproc[(update-one! [model entity?])
         (or/c false/c entity?)]{
   
  A wrapper around Deta's version.  But you do not need to supply a database connection.
}

@defproc[(update! [model entity?] ...)
         (listof entity?)]{
   
  A wrapper around Deta's version.  But you do not need to supply a database connection.
}

@defproc[(delete! [model entity?] ...)
         (listof entity?)]{
   
  A wrapper around Deta's version.  But you do not need to supply a database connection.
}

@defform[(all entity-name)]{
  Selects * from the table implied by @racket[entity-name].  Packages them into Deta structs implied by @racket[entity-name]

  @codeblock{
    (all course)
  }
}

@defproc[(get [model entity?] [field-name symbol?])
         any/c]{

  Dynamic getter if you know the name of a table
   
  @codeblock{
    (define c (first (all course)))
    (get c 'name)
  }
}

@defproc[(get-type [model entity?])
         symbol?]{

  Gets the type of a model.
   
  @codeblock{
    (define c (first (all course)))
    (get-type c)
  }

  Returns @racket['course]
}

@defproc[(get-fields [model entity?])
         (listof symbol?)]{

  Gets the field names (column names) of a model.
   
  @codeblock{
    (define c (first (all course)))
    (get-fields c)
  }

  Returns @racket['(id name description ...)] etc.
}

@defproc[(get-values [model entity?])
         (listof any/c)]{

  Gets the field values of a model.
   
  @codeblock{
    (define c (first (all course)))
    (get-values c)
  }

  Returns @racket['(1 "Becoming Unicorn" "Become the Unicorn you feel you are" ...)] etc.
}
