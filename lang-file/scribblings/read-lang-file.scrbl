#lang scribble/manual

@(require scribble/eval
          (for-label racket/base
                     racket/bool
                     racket/contract/base
                     lang-file/read-lang-file
                     ))

@title{read-lang-file}

source code: @url{https://github.com/AlexKnauth/lang-file}

@defmodule[lang-file/read-lang-file]

@defproc[(read-lang-file [path-string path-string?]) syntax?]{
Reads a @hash-lang[] file into a @racket[module] syntax object.
}

@defproc[(lang-file? [path-string path-string?]) boolean?]{
Returns true if @racket[path-string] points to a valid @hash-lang[]
file, false otherwise.
}

@defproc[(lang-file-lang [path-string path-string?]) (or/c false? string?)]{
Returns a string containing the the language of a @hash-lang[] file.
}

