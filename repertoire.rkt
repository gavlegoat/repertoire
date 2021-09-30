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
  (-> move? jsexpr?)
  (let ([from-sq (car move)]
        [to-sq (cdr move)])
    (string-append (pair->algebraic from-sq) "-" (pair->algebraic to-sq))))

;; Convert a move in string format to a move in pair format. For example
;; "e2-e4" becomes ((4 . 1) . (4 . 3))
(define/contract (jsexpr->move js)
  (-> jsexpr? move?)
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

    (define/public (get-next-moves)
      next-moves)

    (define/public (set-annotation str)
      (set! notes str))

    (define/public (add-next-move move)
      (if (member move next-moves)
          (void)
          (set! next-moves (cons move next-moves))))

    (define/public (set-next-moves moves)
      (set! next-moves moves))
    
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

    (define table (make-hash entries))

    (if filename
        (let ([in (open-input-file filename)])
          (define instr (port->string in #:close? #t))
          (let ([json-table (string->jsexpr instr)])
            (set! table (make-hash
                         (hash-map
                          json-table
                          (lambda (k v)
                            (cons (symbol->string k)
                                  (new repertoire-entry% [json v]))))))))
        (void))

    ; Write this repertoire to a file.
    (define/public (write-to-file filename)
      (let ([es (make-hash
                 (hash-map table (lambda (k v) (cons (string->symbol k)
                                                     (send v get-json)))))]
            [out (open-output-file filename #:exists 'replace)])
        (display (jsexpr->string es) out)
        (close-output-port out)))        

    (define/public (get-annotation pos)
      (if (hash-has-key? table pos)
          (send (hash-ref table pos) get-annotation)
          ""))

    (define/public (get-next-moves pos)
      (if (hash-has-key? table pos)
          (send (hash-ref table pos) get-next-moves)
          '()))

    (define/public (set-annotation pos str)
      (if (hash-has-key? table pos)
          (send (hash-ref table pos) set-annotation str)
          (hash-set! table pos (new repertoire-entry% [annotation str]))))

    (define/public (set-next-moves pos moves)
      (if (hash-has-key? table pos)
          (send (hash-ref table pos) set-next-moves moves)
          (hash-set! table pos (new repertoire-entry% [moves moves]))))

    (define/public (add-move pos move)
      (if (hash-has-key? table pos)
          (send (hash-ref table pos) add-next-move move)
          (hash-set! table pos (new repertoire-entry% [moves (list move)]))))

    (define/public (has-position pos)
      (hash-has-key? table pos)) 
    
    (super-new)))

(define/contract repertoire+c%
  (class/c
   [write-to-file (->m string? void?)]
   [get-annotation (->m string? string?)]
   [get-next-moves (->m string? (listof move?))]
   [set-annotation (->m string? string? void?)]
   [set-next-moves (->m string? (listof move?) void?)]
   [add-move (->m string? move? void?)]
   [has-position (->m string? boolean?)])
  repertoire%)