#lang racket

;;; Punto de entrada del intérprete de LET

(require "env.rkt"
         "vals.rkt"
         "parser.rkt"
         "desugar.rkt"
         "interp.rkt")

(provide
 (contract-out
  [run  (-> string? expval?)]
  [repl (-> void?)]))

;; El árbol que sale del parser pasa por desugar antes de llegar a value-of:
;; los constructos de azúcar se traducen al núcleo y value-of nunca los ve.
(define (run str)
  (value-of (desugar (parse str)) (empty-env)))

(define (repl)
  (display "==> ")
  (define line (read-line))
  (unless (or (eof-object? line) (string=? (string-trim line) ""))
    (with-handlers ([exn:fail? (λ (e) (displayln (exn-message e)))])
      (println (run line)))
    (repl)))

(module+ main
  (repl))

