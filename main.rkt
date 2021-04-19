#lang racket/gui

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

(define (piece-image-fn size piece)
  (let ([basename
         (cond
           [(equal? piece 'black-rook)   "Chess_rdt.png"]
           [(equal? piece 'black-knight) "Chess_ndt.png"]
           [(equal? piece 'black-bishop) "Chess_bdt.png"]
           [(equal? piece 'black-queen)  "Chess_qdt.png"]
           [(equal? piece 'black-king)   "Chess_kdt.png"]
           [(equal? piece 'black-pawn)   "Chess_pdt.png"]
           [(equal? piece 'white-rook)   "Chess_rlt.png"]
           [(equal? piece 'white-knight) "Chess_nlt.png"]
           [(equal? piece 'white-bishop) "Chess_blt.png"]
           [(equal? piece 'white-queen)  "Chess_qlt.png"]
           [(equal? piece 'white-king)   "Chess_klt.png"]
           [(equal? piece 'white-pawn)   "Chess_plt.png"]
           [else (raise "Unrecognized piece symbol")])])
    (string-append "img/" (number->string size) "px/" basename)))

;; A GUI object representing a chess position.
(define chess-board%
  (class canvas%
    (inherit get-dc get-width get-height)
    ; The starting position, given as a FEN string (default: starting position).
    (init [position "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"])
    ; The side who's turn it is, either 'white or 'black (default: 'white).
    (init [to-move 'white])

    (define side-to-move to-move)
    (define board-position (interpret-fen position))
    (define square-size 0)
    (define image-size 0)
    (define start-x 0)
    (define start-y 0)
    (define highlight-square '())
    
    (define/private (handle-click event)
      (let* ([x (- (send event get-x) start-x)]
             [y (- (send event get-y) start-y)]
             [xsq (quotient x square-size)]
             [ysq (- 7 (quotient y square-size))]
             [xc (+ (* xsq square-size) (quotient square-size 2))]
             [yc (+ (* (- 7 ysq) square-size) (quotient square-size 2))])
        (if (and (hash-has-key? board-position (cons xsq ysq))
                 (<= (abs (- x xc)) (quotient image-size 2))
                 (<= (abs (- y yc)) (quotient image-size 2)))
            (begin
              (set! highlight-square (cons xsq ysq))
              (send this refresh))
            '())))

    (define/private (handle-release event)
      (set! highlight-square '())
      (send this refresh))

    (define/private (handle-dragging event)
      '())
    
    (define/override (on-event event)
      (cond
        [(send event button-down? 'left) (handle-click event)]
        [(send event button-up? 'left) (handle-release event)]
        [(send event dragging?) (handle-dragging event)]
        [else '()]))
    
    (define/private (draw-piece i j)
      (define dc (get-dc))
      (define width (get-width))
      (define height (get-height))
      (let ([sx (+ start-x (* i square-size))]
            [sy (- height (+ start-y (* j square-size)) square-size)]
            [color (cond [(and (not (null? highlight-square))
                               (= i (car highlight-square))
                               (= j (cdr highlight-square)))
                          (make-color 255 255 0)]
                         [(zero? (remainder (+ i j) 2)) (make-color 118 150 86)]
                         [else (make-color 238 238 210)])])
        (send dc set-brush color 'solid)
        (send dc set-pen "white" 1 'transparent)
        (send dc draw-rectangle sx sy square-size square-size)
        (if (hash-has-key? board-position (cons i j))
            (let* ([x (- (+ sx (quotient square-size 2))
                         (quotient image-size 2) 1)]
                   [y (- (+ sy (quotient square-size 2))
                         (quotient image-size 2) 1)]
                   [img (read-bitmap
                         (piece-image-fn
                          image-size
                          (hash-ref board-position (cons i j)))
                         'png/alpha)]
                   [scale-factor (/ square-size 50)])
              (send dc draw-bitmap img x y))
            '())))
    
    ;; Draw the current board position.
    (define/override (on-paint)
      (define dc (get-dc))
      (define width (get-width))
      (define height (get-height))
      (define (choose-image-size square-size)
        (cond [(>= square-size 80) 75]
              [(>= square-size 65) 60]
              [else 45]))
      (let* ([total-size (min width height)]
             [sq-size (quotient total-size 8)]
             [st-x (quotient (- width (* sq-size 8)) 2)]
             [st-y (quotient (- height (* sq-size 8)) 2)]
             [size (choose-image-size square-size)])
        (set! square-size sq-size)
        (set! start-x st-x)
        (set! start-y st-y)
        (set! image-size size)
        
        (for ([i 8])
          (for ([j 8])
            (draw-piece i j)))))
    
    (super-new)))

;;; Main code

(define (main)
  (define frame (new frame% [label "Repertoire Explorer"]))
  (new chess-board%
       [parent frame]
       [min-width 400]
       [min-height 400])
  (send frame show #t))
