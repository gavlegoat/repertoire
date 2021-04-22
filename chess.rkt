#lang racket

(provide position%)

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
        (let ([symb (cond
                       [(equal? char #\r) 'black-rook]
                       [(equal? char #\n) 'black-knight]
                       [(equal? char #\b) 'black-bishop]
                       [(equal? char #\q) 'black-queen]
                       [(equal? char #\k) 'black-king]
                       [(equal? char #\p) 'black-pawn]
                       [(equal? char #\R) 'white-rook]
                       [(equal? char #\N) 'white-knight]
                       [(equal? char #\B) 'white-bishop]
                       [(equal? char #\Q) 'white-queen]
                       [(equal? char #\K) 'white-king]
                       [(equal? char #\P) 'white-pawn]
                       [else (raise "Unrecognized piece")])])
          (cons (+ (car acc) 1)
                (cons (cons (cons (car acc) r) symb) (cdr acc))))))
  ; Handle a single rank from the FEN string.
  (define (interpret-rank str r)
    (cdr (foldl (lambda (c a) (fold-fun r c a))
                (cons 0 '()) (string->list str))))
  (let ([ranks (string-split fen "/")])
    (make-hash (append-map interpret-rank ranks (range 7 -1 -1)))))

;; A position on a chessboard.
(define position%
  (class object%
    (init [position "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"])
    (init [to-move 'white])
    (init [castling '(((white . kingside) . #t) ((white . queenside) . #t)
                      ((black . kingside) . #t) ((black . queenside) . #t))])
    (init [en-passant '()])

    (define board-position (interpret-fen position))
    (define ep-sq en-passant)
    
    (define/public (has-piece-at? sq)
      (hash-has-key? board-position sq))

    (define/public (get-piece-at sq)
      (hash-ref board-position sq))

    (define/public (get-ep-sq) ep-sq)
    
    (super-new)))

;; Determine whether the given piece has the given color.
(define (same-color? color piece)
  (cond [(equal? piece 'black-rook)   (equal? color 'black)]
        [(equal? piece 'black-knight) (equal? color 'black)]
        [(equal? piece 'black-bishop) (equal? color 'black)]
        [(equal? piece 'black-queen)  (equal? color 'black)]
        [(equal? piece 'black-king)   (equal? color 'black)]
        [(equal? piece 'black-pawn)   (equal? color 'black)]
        [(equal? piece 'white-rook)   (equal? color 'white)]
        [(equal? piece 'white-knight) (equal? color 'white)]
        [(equal? piece 'white-bishop) (equal? color 'white)]
        [(equal? piece 'white-queen)  (equal? color 'white)]
        [(equal? piece 'white-king)   (equal? color 'white)]
        [(equal? piece 'white-pawn)   (equal? color 'white)]
        [else (raise "Unknown piece symbol")]))

;; Generate all pseudolegal rook moves starting from the given square in the
;; given position.
(define (rook-moves pos color from-sq)
  (let ([right
         (for/list ([i (in-range (+ 1 (car from-sq)) 8)])
           #:break (and (send pos has-piece-at? (cons i (cdr from-sq)))
                        (same-color? color (send pos get-piece-at
                                                 (cons i (cdr from-sq)))))
           #:final (and (send pos has-piece-at? (cons i (cdr from-sq)))
                        (not (same-color? color (send pos get-piece-at
                                                      (cons i (cdr from-sq))))))
           (cons i (cdr from-sq)))]
        [left
         (for/list ([i (in-range 0 (car from-sq))])
           #:break (and (send pos has-piece-at? (cons i (cdr from-sq)))
                        (same-color? color (send pos get-piece-at
                                                 (cons i (cdr from-sq)))))
           #:final (and (send pos has-piece-at? (cons i (cdr from-sq)))
                        (not (same-color? color (send pos get-piece-at
                                                      (cons i (cdr from-sq))))))
           (cons i (cdr from-sq)))]
        [up
         (for/list ([i (in-range (+ 1 (cdr from-sq)) 8)])
           #:break (and (send pos has-piece-at? (cons (car from-sq) i))
                        (same-color? color (send pos get-piece-at
                                                 (cons (car from-sq) i))))
           #:final (and (send pos has-piece-at? (cons (car from-sq) i))
                        (not (same-color? color (send pos get-piece-at
                                                      (cons (car from-sq) i)))))
           (cons (car from-sq) i))]
        [down
         (for/list ([i (in-range 0 (cdr from-sq))])
           #:break (and (send pos has-piece-at? (cons (car from-sq) i))
                        (same-color? color (send pos get-piece-at
                                                 (cons (car from-sq) i))))
           #:final (and (send pos has-piece-at? (cons (car from-sq) i))
                        (not (same-color? color (send pos get-piece-at
                                                      (cons (car from-sq) i)))))
           (cons (car from-sq) i))])
    (append left right up down)))

;; Generate pseudolegal knight moves.
(define (knight-moves pos color from-sq)
  (define (map-fun offset)
    (let ([dest (cons (+ (car from-sq) (car offset))
                      (+ (cdr from-sq) (cdr offset)))])
      (if (or (< (car dest) 0) (< (cdr dest) 0)
              (> (car dest) 7) (> (cdr dest) 7)
              (and (send pos has-piece-at? dest)
                   (same-color? color (send pos get-piece-at dest))))
          '()
          (list dest))))
  (append-map map-fun '((1 . -2) (-1 . -2) (-2 . -1) (-2 . 1)
                        (-1 . 2) (1 . 2) (2 . 1) (2 . -1))))

;; Generate pseudolegal bishop moves.
(define (bishop-moves pos color from-sq)
  (let ([upright
         (for/list ([i (in-range (+ 1 (car from-sq)) 8)]
                    [j (in-range (+ 1 (cdr from-sq)) 8)])
           #:break (and (send pos has-piece-at? (cons i j))
                        (same-color? color (send pos get-piece-at (cons i j))))
           #:final (and (send pos has-piece-at? (cons i j))
                        (not (same-color? color
                                          (send pos get-piece-at (cons i j)))))
           (cons i j))]
        [upleft
         (for/list ([i (in-range (- (car from-sq) 1) -1 -1)]
                    [j (in-range (+ 1 (cdr from-sq)) 8)])
           #:break (and (send pos has-piece-at? (cons i j))
                        (same-color? color (send pos get-piece-at (cons i j))))
           #:final (and (send pos has-piece-at? (cons i j))
                        (not (same-color? color
                                          (send pos get-piece-at (cons i j)))))
           (cons i j))]
        [downright
         (for/list ([i (in-range (+ 1 (car from-sq)) 8)]
                    [j (in-range (- (cdr from-sq) 1) -1 -1)])
           #:break (and (send pos has-piece-at? (cons i j))
                        (same-color? color (send pos get-piece-at (cons i j))))
           #:final (and (send pos has-piece-at? (cons i j))
                        (not (same-color? color
                                          (send pos get-piece-at (cons i j)))))
           (cons i j))]
        [downleft
         (for/list ([i (in-range (- (car from-sq) 1) -1 -1)]
                    [j (in-range (- (car from-sq) 1) -1 -1)])
           #:break (and (send pos has-piece-at? (cons i j))
                        (same-color? color (send pos get-piece-at (cons i j))))
           #:final (and (send pos has-piece-at? (cons i j))
                        (not (same-color? color
                                          (send pos get-piece-at (cons i j)))))
           (cons i j))])
    (append upright upleft downright downleft)))

;; Generate pseudolegal queen moves.
(define (queen-moves pos color from-sq)
  (append (rook-moves pos color from-sq) (bishop-moves pos color from-sq)))

;; Generate pseudolegal king moves.
; TODO: Castling
(define (king-moves pos color from-sq)
  (define (map-fun offset)
    (let ([dest (cons (+ (car from-sq) (car offset))
                      (+ (cdr from-sq) (cdr offset)))])
      (if (or (< (car dest) 0) (< (cdr dest) 0)
              (> (car dest) 7) (> (cdr dest) 7)
              (and (send pos has-piece-at? dest)
                   (same-color? color (send pos get-piece-at dest))))
          '()
          (list dest))))
  (append-map map-fun '((-1 . -1) (-1 . 0) (-1 . 1) (0 . -1)
                        (0 . 1) (1 . -1) (1 . 0) (1 . 1))))

;; Generate pseudolegal pawn moves.
(define (pawn-moves pos color from-sq)
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
(define (find-piece pos piece)
  (append
   (for*/list ([i (in-range 8)]
               [j (in-range 8)]
               #:when (and (send pos has-piece-at? (cons i j))
                           (equal? (send pos get-piece-at (cons i j)) piece)))
     (cons i j))))

;; Determine whether the given king is in check in the given position.
; TODO
(define (is-check pos color)
  '())

;; Get a list of all legal moves from the given position and piece.
; TODO: Check, castling
(define (get-legal-moves pos piece from-sq)
  (let ([pseudo
         (cond [(equal? piece 'black-rook)   (rook-moves pos 'black from-sq)]
               [(equal? piece 'black-knight) (knight-moves pos 'black from-sq)]
               [(equal? piece 'black-bishop) (bishop-moves pos 'black from-sq)]
               [(equal? piece 'black-queen)  (queen-moves pos 'black from-sq)]
               [(equal? piece 'black-king)   (king-moves pos 'black from-sq)]
               [(equal? piece 'black-pawn)   (pawn-moves pos 'black from-sq)]
               [(equal? piece 'white-rook)   (rook-moves pos 'white from-sq)]
               [(equal? piece 'white-knight) (knight-moves pos 'white from-sq)]
               [(equal? piece 'white-bishop) (bishop-moves pos 'white from-sq)]
               [(equal? piece 'white-queen)  (queen-moves pos 'white from-sq)]
               [(equal? piece 'white-king)   (king-moves pos 'white from-sq)]
               [(equal? piece 'white-pawn)   (pawn-moves pos 'white from-sq)]
               [else (raise "Unknown piece symbol")])])
    pseudo))

;; Determine whether a given move is legal.
(define (is-legal-move piece from-sq to-sq)
  (member to-sq (get-legal-moves piece from-sq)))