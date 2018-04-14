#lang racket/base

(require racket/contract/base)
(provide (contract-out
          [read-lang-file (-> path-string? syntax?)]
          [read-lang-module (-> input-port? syntax?)])
         lang-file?
         lang-file-lang
         )

(require (only-in racket/list last)
         (only-in racket/path path-only)
         (only-in racket/port call-with-input-string peeking-input-port)
         (only-in racket/string string-trim)
         (only-in syntax/modread with-module-reading-parameterization)
         )

(module+ test
  (require rackunit
           racket/runtime-path))

;; read-lang-file : Path-String -> Syntax
(define (read-lang-file path-string)
  (call-with-input-file*/dir path-string read-lang-module))

;; read-lang-module : Input-Port -> Syntax
(define (read-lang-module port)
  (port-count-lines! port)
  (with-module-reading-parameterization
   (lambda ()
     (read-syntax (object-name port) port))))

;; private value eq? to itself
(define read-language-fail (gensym 'read-language-fail))
(define (read-language-fail? v)
  (eq? v read-language-fail))

;; lang-file? : Path-String -> Boolean
(define (lang-file? path-string)
  (cond
    [(file-exists? path-string)
     (call-with-input-file*/dir
       path-string
       (lambda (port)
         (port-count-lines! port)
         (not (read-language-fail? (read-language port (λ () read-language-fail))))))]
    [else #false]))

;; lang-file-lang : Path-String -> (U False String)
(define (lang-file-lang path-string)
  (cond
    [(file-exists? path-string)
     (call-with-input-file*/dir path-string read-lang)]
    [else #false]))

;; read-lang : Input-Port -> (U False String)
(define (read-lang port)
  (port-count-lines! port)
  (define port* (peeking-input-port port))
  (port-count-lines! port*)
  (and
   (with-handlers ([exn:fail:read? (λ (e) #false)])
     (not (read-language-fail? (read-language port* (λ () read-language-fail)))))
   (let* ([end (file-position port*)]
          [str (read-string end port)]
          [hash-lang-positions (regexp-match-positions* "#lang|#!" str)]
          [start (cdr (last hash-lang-positions))])
     (string-trim (substring str start)))))

;; call-with-input-file*/dir : PathString [InputPort -> Any] #:mode (U 'binary 'text) -> Any
(define (call-with-input-file*/dir path proc #:mode [mode 'binary])
  (call-with-input-file*
    path
    (lambda (port)
      (parameterize ([current-directory (or (path-only path) (current-directory))])
        (proc port)))
    #:mode mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define-check (check-stx-datum stx expected-datum)
    (check-pred syntax? stx)
    (check-equal? (syntax->datum stx) expected-datum))

  (define-runtime-path read-lang-file.rkt "read-lang-file.rkt")
  (define-runtime-path interp "test/interp.ss")
  (define-runtime-path tuvalu.rkt "test/tuvalu.rkt")

  (check-true (lang-file? read-lang-file.rkt))
  (check-false (lang-file? interp))
  (check-true (lang-file? tuvalu.rkt))
  (check-pred syntax? (read-lang-file read-lang-file.rkt))
  (check-pred syntax? (read-lang-file tuvalu.rkt))

  (test-case "read-lang-module"
    (define (read-mod . strs)
      (call-with-input-string (apply string-append strs) read-lang-module))

    (check-stx-datum (read-mod
                      "#lang racket/base\n"
                      "(+ 1 2)")
                     '(module anonymous-module racket/base
                        (#%module-begin (+ 1 2))))
    (check-stx-datum (read-mod
                      "#lang at-exp racket/base\n"
                      "(+ 1 2)\n"
                      "@+[3 4]\n"
                      "@string-append{five five}\n"
                      "(define six (number->string 6))\n"
                      "(define seven (symbol->string 'seven))\n"
                      "@string-append{The numbers @six and @|seven|.}")
                     '(module anonymous-module racket/base
                        (#%module-begin
                         (+ 1 2)
                         (+ 3 4)
                         (string-append "five five")
                         (define six (number->string 6))
                         (define seven (symbol->string 'seven))
                         (string-append "The numbers " six " and " seven "."))))
    )

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
