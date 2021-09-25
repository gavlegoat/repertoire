;;; This file keeps defines functions for working with an entire opening
;;; repertoire

#lang racket

(provide repertoire%)

(require json)
(require "chess.rkt")

;; Convert a move (in pair representation) to a string representing the
;; start and end squares, for example ((4 . 1) . (4 . 3)) is converted to
;; "e2-e4".
(define/contract (move->jsexpr move)
  (-> (cons/c (cons/c integer? integer?) (cons/c integer? integer?)) jsexpr?)
  (let ([from-sq (car move)]
        [to-sq (cdr move)])
    (string-append (pair->algebraic from-sq) "-" (pair->algebraic to-sq))))

;; Convert a move in string format to a move in pair format. For example
;; "e2-e4" becomes ((4 . 1) . (4 . 3))
(define/contract (jsexpr->move js)
  (-> jsexpr? (cons/c (cons/c integer? integer?) (cons/c integer? integer?)))
  (let ([from-sq (substring js 0 2)]
        [to-sq (substring js 3)])
    (cons (algebraic->pair from-sq) (algebraic->pair to-sq))))

;; A repertoire entry holds all of the information associated with a single
;; position in a repertoire.
(define repertoire-entry%
  (class object%
    (init [annotation ""])
    (init [moves '()])
    ; A JSON string representing an entry. This is ignored if its value is #f,
    ; otherwise the other two initializers are ignored.
    (init [json #f])
    
    ; The textual notes associated with the position
    (define notes annotation)
    ; A list of moves that may be made.
    (define next-moves moves)

    (if json
        (begin
          (set! notes (hash-ref json 'annotation))
          (set! next-moves
                (map jsexpr->move (hash-ref json 'next-moves))))
        '())
    
    ; Convert this entry into a JSON object for writing out
    (define/public (get-json)
      (hash 'annotation notes 'next-moves (map move->jsexpr next-moves)))

    (define/public (get-annotation)
      notes)
    
    (super-new)))

;; A repertoire consists of a map from board positions to repertoire entries.
;; We will use FEN strings to encode positions, but because we don't care
;; about move numbers or repetititions for this purpose, we will omit
;; those fields. That is, for the starting position we will use
;; "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -".
;; NOTE: It is important that the castling rights are indicated in the order
;; KQkq because the key-value store is based on string equality.
(define repertoire%
  (class object%
    ; An association list of entries for this repertoire
    (init [entries '()])
    ; A filename to read a repertoire from. If the filename is #f it is ignored,
    ; otherwise entries is ignored.
    (init [filename #f])

    (define table (make-immutable-hash entries))

    (if filename
        (let ([in (open-input-file filename)])
          (define instr (port->string in #:close? #t))
          (let ([json-table (string->jsexpr instr)])
            (set! table (make-immutable-hash
                         (hash-map
                          json-table
                          (lambda (k v)
                            (cons (symbol->string k)
                                  (new repertoire-entry% [json v]))))))))
        '())

    ; Write this repertoire to a file.
    (define/public (write-to-file filename)
      (let ([es (make-immutable-hash
                 (hash-map (lambda (k v) (cons k (send v get-json))) table))]
            [out (open-output-file filename)])
        (display (jsexpr->string es) out)
        (close-output-port out)))        

    (define/public (get-annotation pos)
      (if (hash-has-key? table pos)
          (send (hash-ref table pos) get-annotation)
          ""))
    
    (super-new)))