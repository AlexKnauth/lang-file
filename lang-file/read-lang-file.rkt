#lang racket/base

(require racket/contract/base)
(provide (contract-out
          [read-lang-file (-> path-string? syntax?)])
         lang-file?
         lang-file-lang
         )

(require (only-in racket/list last)
         (only-in racket/port call-with-input-string peeking-input-port)
         (only-in racket/string string-trim)
         (only-in syntax/modread with-module-reading-parameterization)
         )

(module+ test
  (require rackunit
           racket/runtime-path))

;; read-lang-file : Path-String -> Syntax
(define (read-lang-file path-string)
  (define port (open-input-file path-string))
  (port-count-lines! port)
  (begin0
    (with-module-reading-parameterization 
     (lambda () 
       (read-syntax (object-name port) port)))
    (close-input-port port)))

;; private value eq? to itself
(define read-language-fail (gensym 'read-language-fail))

;; lang-file? : Path-String -> Boolean
(define (lang-file? path-string)
  (cond
    [(file-exists? path-string)
     (define port (open-input-file path-string))
     (port-count-lines! port)
     (begin0
       (not (eq? (read-language port (λ () read-language-fail)) read-language-fail))
       (close-input-port port))]
    [else #false]))

;; lang-file-lang : Path-String -> (U False String)
(define (lang-file-lang path-string)
  (cond
    [(file-exists? path-string)
     (define port (open-input-file path-string))
     (port-count-lines! port)
     (begin0
       (read-lang port)
       (close-input-port port))]
    [else #false]))

;; read-lang : Input-Port -> (U False String)
(define (read-lang port)
  (define port* (peeking-input-port port))
  (port-count-lines! port*)
  (and
   (with-handlers ([exn:fail:read? (λ (e) #false)])
     (not (eq? (read-language port* (λ () read-language-fail)) read-language-fail)))
   (let* ([end (file-position port*)]
          [str (read-string end port)]
          [hash-lang-positions (regexp-match-positions* "#lang|#!" str)]
          [start (cdr (last hash-lang-positions))])
     (string-trim (substring str start)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define-runtime-path read-lang-file.rkt "read-lang-file.rkt")
  (define-runtime-path scribblings "scribblings")
  (check-true (lang-file? read-lang-file.rkt))
  (check-false (lang-file? scribblings))
  (check-pred syntax? (read-lang-file read-lang-file.rkt))

  (test-case "read-lang"
    (define (read-lang-from-strings . strs)
      (call-with-input-string (apply string-append strs) read-lang))

    ;; -- basic #langs
    (check-equal? (read-lang-from-strings
                   "#lang racket/base\n"
                   "(+ 1 2")
                  "racket/base")
    (check-equal? (read-lang-from-strings
                   "#lang racket/base\n"
                   "(+ 1 2)")
                  "racket/base")
    (check-equal? (read-lang-from-strings
                   "\n\n#lang racket\n")
                  "racket")
    (check-equal? (read-lang-from-strings
                   "#lang scribble/manual\n"
                   "some text for scribble\n")
                  "scribble/manual")
    (check-equal? (read-lang-from-strings
                   "   #lang typed/racket")
                  "typed/racket")
    
    ;; -- confusing #langs
    (check-equal? (read-lang-from-strings
                   "#!racket")
                  "racket")
    (check-equal? (read-lang-from-strings
                   ";; extra comment\n"
                   "#lang racket/base \n"
                   "(+ 1 2)")
                  "racket/base")
    (check-equal? (read-lang-from-strings
                   "#| another kind of comment \n"
                   "|#\n"
                   "#lang scribble/html")
                  "scribble/html")
    (check-equal? (read-lang-from-strings
                   "#lang at-exp racket")
                  "at-exp racket")
    (check-equal? (read-lang-from-strings
                   "#|#lang scribble/manual|# #!racket")
                  "racket")

    ;; -- failures
    (check-false (read-lang-from-strings
                  "#lang #| bad comment |# racket"))
    (check-false (read-lang-from-strings
                  "#lang     racket\n"
                  ";; has too many spaces\n"))
    )
  )
