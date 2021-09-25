;;; This file handles the rules of chess to ensure that only legal moves can be
;;; made.

#lang racket

(provide position%
         piece?
         color?
         (contract-out
          [get-legal-moves
           (-> (is-a?/c position%) symbol? (cons/c integer? integer?)
               (listof (cons/c integer? integer?)))]
          [get-color
           (-> piece? color?)]
          [algebraic->pair
           (-> string? (cons/c integer? integer?))]
          [pair->algebraic
           (-> (cons/c integer? integer?) string?)]))

(require "util.rkt")

(define piece?
  (or/c 'white-rook
        'white-knight
        'white-bishop
        'white-queen
        'white-king
        'white-pawn
        'black-rook
        'black-knight
        'black-bishop
        'black-queen
        'black-king
        'black-pawn))

(define color?
  (or/c 'white 'black))

;; Convert algebraic notation to pair notation.
(define (algebraic->pair alg)
  (if (equal? alg "-")
      '()
      (let ([lst (string->list alg)])
        (cons (- (char->integer (car lst)) 97)
              (- (char->integer (cadr lst)) 49)))))

;; Convert a square in pair notation to algebraic notation.
(define (pair->algebraic pair)
  (let ([file (integer->char (+ (car pair) 97))]
        [rank (integer->char (+ (cdr pair) 49))])
    (string file rank)))

(define (get-color piece)
  (switch piece
          ['white-rook 'white]
          ['white-knight 'white]
          ['white-bishop 'white]
          ['white-queen 'white]
          ['white-king 'white]
          ['white-pawn 'white]
          ['black-rook 'black]
          ['black-knight 'black]
          ['black-bishop 'black]
          ['black-queen 'black]
          ['black-king 'black]
          ['black-pawn 'black]))

;; A position is represented by a map from squares to pieces, where a square
;; is a pair of integers and piece is one of the following symbols:
;; 'white-pawn   'black-pawn
;; 'white-rook   'black-rook
;; 'white-knight 'black-knight
;; 'white-bishop 'black-bishop
;; 'white-queen  'black-queen
;; 'white-king   'black-king
;; The coordinates start from a1 and are 0-indexed. For example e4 is (4 . 3).
;; The starting position, for example, is represented as
;; (make-hash '(((0 . 0) . white-rook)
;;              ((0 . 1) . white-pawn)
;;              ((1 . 0) . white-knight) ...))
  
;; Convert a FEN-encoded position into a hash table of square-piece pairs
(define (interpret-fen fen)
  ; Interpret one character from a FEN string. The accumulator is a pair
  ; (f . l) where f is the number of the current file and l is the list
  ; of pieces seen so far.
  (define (fold-fun r char acc)
    (if (char-numeric? char)
        (cons (+ (car acc) (- (char->integer char) 48)) (cdr acc))
        (let ([symb (switch char
                       [#\r 'black-rook]
                       [#\n 'black-knight]
                       [#\b 'black-bishop]
                       [#\q 'black-queen]
                       [#\k 'black-king]
                       [#\p 'black-pawn]
                       [#\R 'white-rook]
                       [#\N 'white-knight]
                       [#\B 'white-bishop]
                       [#\Q 'white-queen]
                       [#\K 'white-king]
                       [#\P 'white-pawn]
                       [else (raise "Unrecognized piece")])])
          (cons (+ (car acc) 1)
                (cons (cons (cons (car acc) r) symb) (cdr acc))))))
  ; Handle a single rank from the FEN string.
  (define (interpret-rank str r)
    (cdr (foldl (lambda (c a) (fold-fun r c a))
                (cons 0 '()) (string->list str))))
  (let ([ranks (string-split fen "/")])
    (make-immutable-hash (append-map interpret-rank ranks (range 7 -1 -1)))))

;; A position on a chessboard.
(define position%
  (class object%
    (init [fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"])
    ;; The position of the pieces on the board, as a hashtable explained above.
    (init-field [board-position #f])
    ;; Whose turn it is.
    (init-field [to-move #f])
    ;; The square where an en passant capture can end (i.e., the 6th or 3rd
    ;; rank). If en passant is not possible, this is '().
    (init-field [ep-sq #f])
    ;; An association list describing where castling is possible.
    (init-field [castling-rights #f])

    ;; We only use the fen string if no other initialization fields are
    ;; provided.
    (if (or board-position to-move ep-sq castling-rights)
        '()
        (let ([parts (string-split fen)])
          (let ([pos (car parts)]
                [side (cadr parts)]
                [castle (caddr parts)]
                [ep (cadddr parts)])
            (set! board-position (interpret-fen pos))
            (set! to-move (if (equal? side "w") 'white 'black))
            (set! ep-sq (if (equal? ep "-") '() (algebraic->pair ep)))
            (set! castling-rights
                  (make-immutable-hash
                   (list (cons '(white . kingside) (string-contains? castle "K"))
                         (cons '(white . queenside) (string-contains? castle "Q"))
                         (cons '(black . kingside) (string-contains? castle "k"))
                         (cons '(black . queenside) (string-contains? castle "q"))))))))
        
    ;; The last state, used for unmaking moves.
    (define last-board-pos board-position)
    (define last-ep-sq ep-sq)
    (define last-castling castling-rights)

    ;; Check whether there is a piece at a given square.
    (define/public (has-piece-at? sq)
      (hash-has-key? board-position sq))

    ;; Get the piece at a given square. This method assumes that square does
    ;; have a piece.
    (define/public (get-piece-at sq)
      (hash-ref board-position sq))

    ;; Get the square where en passant can happen.
    (define/public (get-ep-sq) ep-sq)

    ;; Get the side who's turn it is (either 'white or 'black).
    (define/public (get-to-move) to-move)

    ;; Get the algebraic notation for the given move assuming it maps the
    ;; previous position to the current position.
    (define/public (get-algebraic move)
      (let* ([piece (hash-ref last-board-pos (car move))]
             [piece-code (switch piece
                                 ['white-rook "R"]
                                 ['black-rook "R"]
                                 ['white-knight "N"]
                                 ['black-knight "N"]
                                 ['white-bishop "B"]
                                 ['black-bishop "B"]
                                 ['white-queen "Q"]
                                 ['black-queen "Q"]
                                 ['white-king "K"]
                                 ['black-king "K"]
                                 [else ""])]
             [end-sq (pair->algebraic (cdr move))])
        (cond
          [(and (equal? piece-code "K") (= (- (cadr move) (caar move)) 2))
           ; Castle kingside
           "O-O"]
          [(and (equal? piece-code "K") (= (- (caar move) (cadr move)) 3))
           ; Castle queenside
           "O-O-O"]
          [(and (equal? piece-code "")
                (equal? (cdr move) last-ep-sq))
           ; En passant
           (let ([start-file (integer->char (+ (caar move) 97))])
             (string-append (list->string (list start-file))
                            "x"
                            (pair->algebraic (cdr move))))]
          [(equal? piece-code "")
           ; Pawn move -- There can be no ambiguity in pawn moves.
           (if (hash-has-key? last-board-pos (cdr move))
               (string-append
                (list->string (list (integer->char (+ (caar move) 97))))
                "x"
                (pair->algebraic (cdr move)))
               (pair->algebraic (cdr move)))]
          [else
           ; This is the regular case.
           ; Get a list of the locatinos of other pieces of the same kind that
           ; can attack the target square.
           (let ([other-pieces
                  (let* ([last-pos (new position%
                                        [board-position last-board-pos]
                                        [to-move 'white]
                                        [ep-sq last-ep-sq]
                                        [castling-rights last-castling])]
                         [sqs (switch piece-code
                                     ["R" (rook-moves last-pos
                                                      (get-color piece)
                                                      (cdr move)
                                                      #:allow-self-capture #t)]
                                     ["N" (knight-moves last-pos
                                                        (get-color piece)
                                                        (cdr move)
                                                        #:allow-self-capture #t)]
                                     ["B" (bishop-moves last-pos
                                                        (get-color piece)
                                                        (cdr move)
                                                        #:allow-self-capture #t)]
                                     ["Q" (queen-moves last-pos
                                                       (get-color piece)
                                                       (cdr move)
                                                       #:allow-self-capture #t)]
                                     ["K" (king-moves last-pos
                                                      (get-color piece)
                                                      (cdr move)
                                                      #:allow-self-capture #t)])])
                    (filter
                     (lambda (sq) (if (hash-has-key? last-board-pos sq)
                                      (and (equal? (hash-ref last-board-pos sq)
                                                   piece)
                                           (not (equal? sq (car move))))
                                      #f))
                     sqs))]
                 [capture-string
                  (if (hash-has-key? last-board-pos (cdr move)) "x" "")])
             (if (null? other-pieces)
                 ; There is no ambiguity
                 (string-append piece-code
                                capture-string
                                (pair->algebraic (cdr move)))
                 (if (ormap (lambda (p) (equal? (car p) (caar move)))
                            other-pieces)
                     ; File is not enough to distinguish
                     (if (ormap (lambda (p) (equal? (cdr p) (cdar move)))
                                other-pieces)
                         ; Rank is not enough to distinguish
                         (string-append piece-code
                                        (pair->algebraic (car move))
                                        capture-string
                                        (pair->algebraic (cdr move)))
                         ; Rank is enough to distinguish
                         (string-append
                          piece-code
                          (substring (pair->algebraic (car move)) 1 2)
                          capture-string
                          (pair->algebraic (cdr move))))
                     ; File is enough to distinguish
                     (string-append piece-code
                                    (substring (pair->algebraic (car move)) 0 1)
                                    capture-string
                                    (pair->algebraic (cdr move))))))])))

    ; NOTE: it is possible (though rare) for situations to come up where both
    ; file and the rank are needed to distinguish different moves. For example,
    ; in the position below Qdd4 is ambiguous between the queens on d2 and d6
    ; while Q2d4 is ambiguous between d2 and f2. Therefore we would need to
    ; return Qd2d4.
    ; - - - - - - - -
    ; - - - - - - - -
    ; - - - Q - - - -
    ; - - - - - - - -
    ; - - - x - - - -
    ; - - - - - - - -
    ; - - - Q - Q - -
    ; - - - - - - - -
    
    ;; Determine whether the given color can castle queenside (including looking
    ;; checks).
    (define/public (queenside-castle? color)
      (if (hash-ref castling-rights (cons color 'queenside))
          (let ([other (if (equal? color 'white) 'black 'white)]
                [from-sq (if (equal? color 'white) (cons 4 0) (cons 4 7))])
            (not (or (is-attacked other from-sq)
                     (is-attacked other (cons (- (car from-sq) 1)
                                              (cdr from-sq)))
                     (is-attacked other (cons (- (car from-sq) 2)
                                              (cdr from-sq)))
                     (hash-has-key? board-position
                                    (cons (- (car from-sq) 1) (cdr from-sq)))
                     (hash-has-key? board-position
                                    (cons (- (car from-sq) 2) (cdr from-sq)))
                     (hash-has-key? board-position
                                    (cons (- (car from-sq) 3) (cdr from-sq))))))
          #f))

    ;; Determine whether the given color can castle kingside (including looking
    ;; checks).
    (define/public (kingside-castle? color)
      (if (hash-ref castling-rights (cons color 'kingside))
          (let ([other (if (equal? color 'white) 'black 'white)]
                [from-sq (if (equal? color 'white) (cons 4 0) (cons 4 7))])
            (not (or (is-attacked other from-sq)
                     (is-attacked other (cons (+ (car from-sq) 1)
                                              (cdr from-sq)))
                     (is-attacked other (cons (+ (car from-sq) 2)
                                              (cdr from-sq)))
                     (hash-has-key? board-position
                                    (cons (+ (car from-sq) 1) (cdr from-sq)))
                     (hash-has-key? board-position
                                    (cons (+ (car from-sq) 2) (cdr from-sq))))))
          #f))


    ;; Get a list of all positions at which a given piece appears.
    (define/public (find-piece piece)
      (apply append
             (for*/list ([i (in-range 0 8)]
                         [j (in-range 0 8)])
               (if (and (hash-has-key? board-position (cons i j))
                        (equal? (hash-ref board-position (cons i j)) piece))
                   (list (cons i j))
                   '()))))

    ;; Determine whether the given piece is at the given square.
    (define/public (piece-at? piece sq)
      (and (hash-has-key? board-position sq)
           (equal? (hash-ref board-position sq) piece)))
    
    ;; Determine whether the given sq is attacked by pieces of the given color
    ;; in the given position. Note that this function doesn't handle en passant.
    (define/private (is-attacked color sq)
      (define (check-sqs piece sqs)
        (ormap (lambda (x) (piece-at? piece x)) sqs))
      (define (add-sq s1 s2)
        (let ([r (cons (+ (car s1) (car s2)) (+ (cdr s1) (cdr s2)))])
          (if (and (< (car r) 8) (< (cdr r) 8) (>= (car r) 0) (>= (cdr r) 0))
              (list r)
              '())))
      (let* ([other (if (equal? color 'white) 'black 'white)]
             [knight
              (let ([sqs (knight-moves this other sq)])
                (if (equal? color 'white)
                    (check-sqs 'white-knight sqs)
                    (check-sqs 'black-knight sqs)))]
             [rook
              (let ([sqs (rook-moves this other sq)])
                (if (equal? color 'white)
                    (or (check-sqs 'white-rook sqs)
                        (check-sqs 'white-queen sqs))
                    (or (check-sqs 'black-rook sqs)
                        (check-sqs 'black-queen sqs))))]
             [bishop
              (let ([sqs (bishop-moves this other sq)])
                (if (equal? color 'white)
                    (or (check-sqs 'white-bishop sqs)
                        (check-sqs 'white-queen sqs))
                    (or (check-sqs 'black-bishop sqs)
                        (check-sqs 'black-queen sqs))))]
             [king
              (let ([sqs (filter (lambda (s) (and (>= (car s) 0) (>= (cdr s) 0)
                                                  (< (car s) 8) (< (cdr s) 8)))
                                 (map (lambda (o)
                                        (cons (+ (car sq) (car o))
                                              (+ (cdr sq) (cdr o))))
                                      '((-1 . -1) (-1 . 0) (-1 . 1) (0 . -1)
                                        (0 . 1) (1 . -1) (1 . 0) (1 . 1))))])
                (if (equal? color 'white)
                    (check-sqs 'white-king sqs)
                    (check-sqs 'black-king sqs)))]
             [pawn
              (if (equal? color 'white)
                  (cond
                    [(= (car sq) 0)
                     (piece-at? 'white-pawn (cons 1 (- (cdr sq) 1)))]
                    [(= (car sq) 7)
                     (piece-at? 'white-pawn (cons 6 (- (cdr sq) 1)))]
                    [else
                     (or (piece-at? 'white-pawn (cons (- (car sq) 1)
                                                      (- (cdr sq) 1)))
                         (piece-at? 'white-pawn (cons (+ (car sq) 1)
                                                      (- (cdr sq) 1))))])
                  (cond
                    [(= (car sq) 0)
                     (piece-at? 'black-pawn (cons 1 (+ (cdr sq) 1)))]
                    [(= (car sq) 7)
                     (piece-at? 'black-pawn (cons 6 (+ (cdr sq) 1)))]
                    [else
                     (or (piece-at? 'black-pawn (cons (- (car sq) 1)
                                                      (+ (cdr sq) 1)))
                         (piece-at? 'black-pawn (cons (+ (car sq) 1)
                                                      (+ (cdr sq) 1))))]))])
        (or knight rook bishop pawn king)))
    
    ;; Determine whether color is in check.
    (define/public (in-check color)
      (if (equal? color 'white)
          (is-attacked 'black (car (find-piece 'white-king)))
          (is-attacked 'white (car (find-piece 'black-king)))))

    ;; Make a move: update the piece map, castling rights and en passant rights.
    (define/public (make-move from-sq to-sq)
      (set! last-board-pos board-position)
      (set! last-ep-sq ep-sq)
      (set! last-castling castling-rights)
      (set! to-move (if (equal? to-move 'white) 'black 'white))
      (cond
        ; En passant
        [(equal? to-sq ep-sq)
         (let* ([piece (hash-ref board-position from-sq)]
                [p1 (hash-remove board-position from-sq)]
                [p2 (hash-remove p1 (cons (car to-sq) (cdr from-sq)))]
                [p3 (hash-set p2 to-sq piece)])
           (set! board-position p3)
           (set! ep-sq '()))]
        ; Castling
        [(and (or (equal? (get-piece-at from-sq) 'white-king)
                  (equal? (get-piece-at from-sq) 'black-king))
              (= (abs (- (car from-sq) (car to-sq))) 2))
         (begin
           (set! ep-sq '())
           (if (equal? (hash-ref board-position from-sq) 'white-king)
               (let* ([c1 (hash-set castling-rights '(white . kingside) #f)]
                      [c2 (hash-set c1 '(white . queenside) #f)])
                 (set! castling-rights c2)
                 (if (> (car to-sq) (car from-sq))
                     (let* ([p1 (hash-remove board-position '(4 . 0))]
                            [p2 (hash-remove p1 '(7 . 0))]
                            [p3 (hash-set p2 '(6 . 0) 'white-king)]
                            [p4 (hash-set p3 '(5 . 0) 'white-rook)])
                       (set! board-position p4))
                     (let* ([p1 (hash-remove board-position '(4 . 0))]
                            [p2 (hash-remove p1 '(0 . 0))]
                            [p3 (hash-set p2 '(2 . 0) 'white-king)]
                            [p4 (hash-set p3 '(3 . 0) 'white-rook)])
                       (set! board-position p4))))
               (let* ([c1 (hash-set castling-rights '(black . kingside) #f)]
                      [c2 (hash-set c1 '(black . queenside) #f)])
                 (set! castling-rights c2)
                 (if (> (car to-sq) (car from-sq))
                     (let* ([p1 (hash-remove board-position '(4 . 7))]
                            [p2 (hash-remove p1 '(7 . 7))]
                            [p3 (hash-set p2 '(6 . 7) 'black-king)]
                            [p4 (hash-set p3 '(5 . 7) 'black-rook)])
                       (set! board-position p4))
                     (let* ([p1 (hash-remove board-position '(4 . 7))]
                            [p2 (hash-remove p1 '(0 . 7))]
                            [p3 (hash-set p2 '(2 . 7) 'black-king)]
                            [p4 (hash-set p3 '(3 . 7) 'black-rook)])
                       (set! board-position p4))))))]
        ; Normal moves
        [else
         (let ([piece (hash-ref board-position from-sq)])
           (set! ep-sq
                 (cond [(and (equal? piece 'white-pawn)
                             (= (- (cdr to-sq) (cdr from-sq)) 2))
                        (cons (car from-sq) (+ (cdr from-sq) 1))]
                       [(and (equal? piece 'black-pawn)
                             (= (- (cdr from-sq) (cdr to-sq)) 2))
                        (cons (car from-sq) (- (cdr from-sq) 1))]
                       [else '()]))
           (cond [(equal? piece 'white-king)
                  (let* ([c1 (hash-set castling-rights '(white . kingside) #f)]
                         [c2 (hash-set c1 '(white . queenside) #f)])
                    (set! castling-rights c2))]
                 [(equal? piece 'black-king)
                  (let* ([c1 (hash-set castling-rights '(black . kingside) #f)]
                         [c2 (hash-set c1 '(black . queenside) #f)])
                    (set! castling-rights c2))]
                 [(and (equal? piece 'white-rook) (equal? from-sq '(0 . 0)))
                  (set! castling-rights
                        (hash-set castling-rights '(white . queenside) #f))]
                 [(and (equal? piece 'white-rook) (equal? from-sq '(0 . 7)))
                  (set! castling-rights
                        (hash-set castling-rights '(white . kingside) #f))]
                 [(and (equal? piece 'black-rook) (equal? from-sq '(7 . 0)))
                  (set! castling-rights
                        (hash-set castling-rights '(black . queenside) #f))]
                 [(and (equal? piece 'black-rook) (equal? from-sq '(7 . 7)))
                  (set! castling-rights
                        (hash-set castling-rights '(black . kingside) #f))]
                 [else '()])
           (let* ([p1 (hash-remove board-position from-sq)]
                  [p2 (hash-set p1 to-sq piece)])
             (set! board-position p2)))]))

    ;; Undo the last move. We assume this move is never called more than once
    ;; between calls to make-move. That is, we are only responsible for saving
    ;; one previous position, not the whole history.
    (define/public (unmake-move)
      (set! to-move (if (equal? to-move 'white) 'black 'white))
      (set! board-position last-board-pos)
      (set! ep-sq last-ep-sq)
      (set! castling-rights last-castling))

    (define/public (get-fen)
      (define (label-square i j)
          (if (hash-has-key? board-position (cons i j))
              (switch (hash-ref board-position (cons i j))
                      ['black-rook   #\r]
                      ['black-knight #\n]
                      ['black-bishop #\b]
                      ['black-queen  #\q]
                      ['black-king   #\k]
                      ['black-pawn   #\p]
                      ['white-rook   #\R]
                      ['white-knight #\N]
                      ['white-bishop #\B]
                      ['white-queen  #\Q]
                      ['white-king   #\K]
                      ['white-pawn   #\P])
              #\1))
      (define (compress lst len)
        (if (null? lst)
            '()
            (if (equal? (car lst) #\1)
                (compress (cdr lst) (+ len 1))
                (if (> len 0)
                    (cons (integer->char (+ len 48)) (compress lst 0))
                    (cons (car lst) (compress (cdr lst) 0))))))
      ; First get an expanded string where consecutive empty spaces are not
      ; compressed
      (let ([expanded (string-join (map (lambda (i)
                                          (list->string
                                           (map (lambda (j)
                                                  (label-square j (- 7 i)))
                                                (range 8))))
                                        (range 8))
                                   "/")])
        ; Now compress consecutive 1's
        (let ([fen-pos (list->string (compress (string->list expanded) 0))]
              [to-play (if (equal? to-move 'white) "w" "b")]
              [castling
               (string-append
                (if (hash-ref castling-rights '(white . kingside)) "K" "")
                (if (hash-ref castling-rights '(white . queenside)) "Q" "")
                (if (hash-ref castling-rights '(black . kingside)) "k" "")
                (if (hash-ref castling-rights '(black . queenside)) "q" ""))]
              [ep-square (if (null? ep-sq) "-" (pair->algebraic ep-sq))])
          (string-join
           (list fen-pos
                 to-play
                 (if (non-empty-string? castling)
                     castling
                     "-")
                 ep-square) " "))))

    (super-new)))

;; A contract defining the interface of position%
(define/contract position+c%
  (class/c
   [has-piece-at? (->m (cons/c integer? integer?) boolean?)]
   [get-piece-at (->m (cons/c integer? integer?) piece?)]
   [get-ep-sq (->m (or/c (cons/c integer? integer?) null?))]
   [get-to-move (->m color?)]
   [get-algebraic (->m (cons/c (cons/c integer? integer?)
                               (cons/c integer? integer?)) string?)]
   [queenside-castle? (->m color? boolean?)]
   [kingside-castle? (->m color? boolean?)]
   [find-piece (->m piece? (listof (cons/c integer? integer?)))]
   [piece-at? (->m piece? (cons/c integer? integer?) boolean?)]
   [unmake-move (->m void?)]
   [get-fen (->m string?)])
  position%)

;; Determine whether the given piece has the given color.
(define/contract (same-color? color piece)
  (-> color? piece? boolean?)
  (switch piece
          ['black-rook   (equal? color 'black)]
          ['black-knight (equal? color 'black)]
          ['black-bishop (equal? color 'black)]
          ['black-queen  (equal? color 'black)]
          ['black-king   (equal? color 'black)]
          ['black-pawn   (equal? color 'black)]
          ['white-rook   (equal? color 'white)]
          ['white-knight (equal? color 'white)]
          ['white-bishop (equal? color 'white)]
          ['white-queen  (equal? color 'white)]
          ['white-king   (equal? color 'white)]
          ['white-pawn   (equal? color 'white)]
          [else (raise (string-append "Unknown piece symbol"
                                      (symbol->string piece)))]))

;; Generate all pseudolegal rook moves starting from the given square in the
;; given position.
(define/contract (rook-moves pos color from-sq #:allow-self-capture [self #f])
  (->* ((is-a?/c position%) color? (cons/c integer? integer?))
       (#:allow-self-capture boolean?)
       (listof (cons/c integer? integer?)))
  (let ([right
         (for/list ([i (in-range (+ 1 (car from-sq)) 8)])
           #:break (and (send pos has-piece-at? (cons i (cdr from-sq)))
                        (same-color? color (send pos get-piece-at
                                                 (cons i (cdr from-sq))))
                        (not self))
           #:final (and (send pos has-piece-at? (cons i (cdr from-sq)))
                        (or
                         (not (same-color? color (send pos get-piece-at
                                                       (cons i (cdr from-sq)))))
                         self))
           (cons i (cdr from-sq)))]
        [left
         (for/list ([i (in-range (- (car from-sq) 1) -1 -1)])
           #:break (and (send pos has-piece-at? (cons i (cdr from-sq)))
                        (same-color? color (send pos get-piece-at
                                                 (cons i (cdr from-sq))))
                        (not self))
           #:final (and (send pos has-piece-at? (cons i (cdr from-sq)))
                        (or
                         (not (same-color? color (send pos get-piece-at
                                                       (cons i (cdr from-sq)))))
                         self))
           (cons i (cdr from-sq)))]
        [up
         (for/list ([i (in-range (+ 1 (cdr from-sq)) 8)])
           #:break (and (send pos has-piece-at? (cons (car from-sq) i))
                        (same-color? color (send pos get-piece-at
                                                 (cons (car from-sq) i)))
                        (not self))
           #:final (and (send pos has-piece-at? (cons (car from-sq) i))
                        (or
                         (not (same-color? color (send pos get-piece-at
                                                       (cons (car from-sq) i))))
                         self))
           (cons (car from-sq) i))]
        [down
         (for/list ([i (in-range (- (cdr from-sq) 1) -1 -1)])
           #:break (and (send pos has-piece-at? (cons (car from-sq) i))
                        (same-color? color (send pos get-piece-at
                                                 (cons (car from-sq) i)))
                        (not self))
           #:final (and (send pos has-piece-at? (cons (car from-sq) i))
                        (or
                         (not (same-color? color (send pos get-piece-at
                                                       (cons (car from-sq) i))))
                         self))
           (cons (car from-sq) i))])
    (append left right up down)))

;; Generate pseudolegal knight moves.
(define/contract (knight-moves pos color from-sq #:allow-self-capture [self #f])
  (->* ((is-a?/c position%) color? (cons/c integer? integer?))
       (#:allow-self-capture boolean?)
       (listof (cons/c integer? integer?)))
  (define (map-fun offset)
    (let ([dest (cons (+ (car from-sq) (car offset))
                      (+ (cdr from-sq) (cdr offset)))])
      (if (or (< (car dest) 0) (< (cdr dest) 0)
              (> (car dest) 7) (> (cdr dest) 7)
              (and (send pos has-piece-at? dest)
                   (same-color? color (send pos get-piece-at dest))
                   (not self)))
          '()
          (list dest))))
  (append-map map-fun '((1 . -2) (-1 . -2) (-2 . -1) (-2 . 1)
                        (-1 . 2) (1 . 2) (2 . 1) (2 . -1))))

;; Generate pseudolegal bishop moves.
(define/contract (bishop-moves pos color from-sq #:allow-self-capture [self #f])
  (->* ((is-a?/c position%) color? (cons/c integer? integer?))
       (#:allow-self-capture boolean?)
       (listof (cons/c integer? integer?)))
  (let ([upright
         (for/list ([i (in-range (+ 1 (car from-sq)) 8)]
                    [j (in-range (+ 1 (cdr from-sq)) 8)])
           #:break (and (send pos has-piece-at? (cons i j))
                        (same-color? color (send pos get-piece-at (cons i j)))
                        (not self))
           #:final (and (send pos has-piece-at? (cons i j))
                        (or
                         (not (same-color? color
                                           (send pos get-piece-at (cons i j))))
                         self))
           (cons i j))]
        [upleft
         (for/list ([i (in-range (- (car from-sq) 1) -1 -1)]
                    [j (in-range (+ 1 (cdr from-sq)) 8)])
           #:break (and (send pos has-piece-at? (cons i j))
                        (same-color? color (send pos get-piece-at (cons i j)))
                        (not self))
           #:final (and (send pos has-piece-at? (cons i j))
                        (or
                         (not (same-color? color
                                           (send pos get-piece-at (cons i j))))
                         self))
           (cons i j))]
        [downright
         (for/list ([i (in-range (+ 1 (car from-sq)) 8)]
                    [j (in-range (- (cdr from-sq) 1) -1 -1)])
           #:break (and (send pos has-piece-at? (cons i j))
                        (same-color? color (send pos get-piece-at (cons i j)))
                        (not self))
           #:final (and (send pos has-piece-at? (cons i j))
                        (or
                         (not (same-color? color
                                           (send pos get-piece-at (cons i j))))
                         self))
           (cons i j))]
        [downleft
         (for/list ([i (in-range (- (car from-sq) 1) -1 -1)]
                    [j (in-range (- (cdr from-sq) 1) -1 -1)])
           #:break (and (send pos has-piece-at? (cons i j))
                        (same-color? color (send pos get-piece-at (cons i j)))
                        (not self))
           #:final (and (send pos has-piece-at? (cons i j))
                        (or
                         (not (same-color? color
                                           (send pos get-piece-at (cons i j))))
                         self))
                        (cons i j))])
    (append upright upleft downright downleft)))

;; Generate pseudolegal queen moves.
(define/contract (queen-moves pos color from-sq #:allow-self-capture [self #f])
  (->* ((is-a?/c position%) color? (cons/c integer? integer?))
       (#:allow-self-capture boolean?)
       (listof (cons/c integer? integer?)))
  (append (rook-moves pos color from-sq #:allow-self-capture self)
          (bishop-moves pos color from-sq #:allow-self-capture self)))

;; Generate pseudolegal king moves.
(define/contract (king-moves pos color from-sq #:allow-self-capture [self #f])
  (->* ((is-a?/c position%) color? (cons/c integer? integer?))
       (#:allow-self-capture boolean?)
       (listof (cons/c integer? integer?)))
  (define (map-fun offset)
    (let ([dest (cons (+ (car from-sq) (car offset))
                      (+ (cdr from-sq) (cdr offset)))])
      (if (or (< (car dest) 0) (< (cdr dest) 0)
              (> (car dest) 7) (> (cdr dest) 7)
              (and (send pos has-piece-at? dest)
                   (same-color? color (send pos get-piece-at dest))
                   (not self)))
          '()
          (list dest))))
  (let ([queenside-castle
         (if (send pos queenside-castle? color)
             (list (cons (- (car from-sq) 2) (cdr from-sq)))
             '())]
        [kingside-castle
         (if (send pos kingside-castle? color)
             (list (cons (+ (car from-sq) 2) (cdr from-sq)))
             '())])
    (append queenside-castle kingside-castle
            (append-map map-fun '((-1 . -1) (-1 . 0) (-1 . 1) (0 . -1)
                                  (0 . 1) (1 . -1) (1 . 0) (1 . 1))))))

;; Generate pseudolegal pawn moves.
(define/contract (pawn-moves pos color from-sq)
  (-> (is-a?/c position%) color? (cons/c integer? integer?)
      (listof (cons/c integer? integer?)))
  (let ([forward
         (if (equal? color 'white)
             (if (send pos has-piece-at?
                       (cons (car from-sq) (+ (cdr from-sq) 1)))
                 '()
                 (cons (cons (car from-sq) (+ (cdr from-sq) 1))
                       (if (and (= (cdr from-sq) 1)
                                (not (send pos has-piece-at?
                                           (cons (car from-sq) 3))))
                           (list (cons (car from-sq) 3))
                           '())))
             (if (send pos has-piece-at?
                       (cons (car from-sq) (- (cdr from-sq) 1)))
                 '()
                 (cons (cons (car from-sq) (- (cdr from-sq) 1))
                       (if (and (= (cdr from-sq) 6)
                                (not (send pos has-piece-at?
                                           (cons (car from-sq) 4))))
                           (list (cons (car from-sq) 4))
                           '()))))]
        [capture
         (let* ([adv (if (equal? color 'white)
                         (lambda (x) (+ x 1))
                         (lambda (x) (- x 1)))]
                [sqs (cond [(zero? (car from-sq))
                            (list (cons 1 (adv (cdr from-sq))))]
                           [(= (car from-sq) 7)
                            (list (cons 6 (adv (cdr from-sq))))]
                           [else (list (cons (- (car from-sq) 1)
                                             (adv (cdr from-sq)))
                                       (cons (+ (car from-sq) 1)
                                             (adv (cdr from-sq))))])])
           (filter
            (lambda (sq)
              (and (send pos has-piece-at? sq)
                   (not (same-color? color (send pos get-piece-at sq)))))
            sqs))]
        [en-passant
         (let ([ep (send pos get-ep-sq)])
           (if (null? ep)
               '()
               (if (and (= (cdr ep) (if (equal? color 'white)
                                        (+ (cdr from-sq) 1)
                                        (- (cdr from-sq) 1)))
                        (or (= (car ep) (+ (car from-sq) 1))
                            (= (car ep) (- (car from-sq) 1))))
                   (list ep)
                   '())))])
    (append forward capture en-passant)))

;; Get a list of all the locations at which a given piece appears.
(define/contract (find-piece pos piece)
  (-> (is-a?/c position%) piece? (listof (cons/c integer? integer?)))
  (append
   (for*/list ([i (in-range 8)]
               [j (in-range 8)]
               #:when (and (send pos has-piece-at? (cons i j))
                           (equal? (send pos get-piece-at (cons i j)) piece)))
     (cons i j))))

;; Get a list of all legal moves from the given position and piece.
(define (get-legal-moves pos piece from-sq)
  (define (look-for-check move)
    (send pos make-move from-sq move)
    (let* ([color (get-color piece)]
           [res (send pos in-check color)])
      (send pos unmake-move)
      (not res)))
  (let ([pseudo
         (switch piece
                 ['black-rook   (rook-moves pos 'black from-sq)]
                 ['black-knight (knight-moves pos 'black from-sq)]
                 ['black-bishop (bishop-moves pos 'black from-sq)]
                 ['black-queen  (queen-moves pos 'black from-sq)]
                 ['black-king   (king-moves pos 'black from-sq)]
                 ['black-pawn   (pawn-moves pos 'black from-sq)]
                 ['white-rook   (rook-moves pos 'white from-sq)]
                 ['white-knight (knight-moves pos 'white from-sq)]
                 ['white-bishop (bishop-moves pos 'white from-sq)]
                 ['white-queen  (queen-moves pos 'white from-sq)]
                 ['white-king   (king-moves pos 'white from-sq)]
                 ['white-pawn   (pawn-moves pos 'white from-sq)]
                 [else (string-append (raise "Unknown piece symbol")
                                    (symbol->string piece))])])
    (filter look-for-check pseudo)))

;; Determine whether a given move is legal.
(define/contract (is-legal-move piece from-sq to-sq)
  (-> piece? (cons/c integer? integer?) (cons/c integer? integer?) boolean?)
  (member to-sq (get-legal-moves piece from-sq)))

;; Convert a full FEN string to a position
(define/contract (fen->position fen)
  (-> string? (is-a?/c position%))
  (let* ([parts (string-split fen)]
         [to-move (if (equal? (cadr parts) "w") 'white 'black)]
         [castle
          (list
           (cons '(white . kingside) (string-contains? (caddr parts) "K"))
           (cons '(white . queenside) (string-contains? (caddr parts) "Q"))
           (cons '(black . kingside) (string-contains? (caddr parts) "k"))
           (cons '(black . queenside) (string-contains? (caddr parts) "q")))]
         [en-passant (algebraic->pair (cadddr parts))])
    (new position%
         [position (car parts)]
         [en-passant en-passant]
         [castling castle]
         [side-to-move to-move])))