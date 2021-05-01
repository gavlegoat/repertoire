#lang racket/gui

(require "chess.rkt")

;; Choose a filename based on the piece type and size.
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
    (inherit get-dc get-width get-height refresh)
    (init [position "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"])
    (init [castling '(((white . kingside) . #t) ((white . queenside) . #t)
                      ((black . kingside) . #t) ((black . queenside) . #t))])
    (init [en-passant '()])
    (init [side-to-move 'white])

    (define board-position (new position% [position position]
                                          [castling castling]
                                          [en-passant en-passant]
                                          [side-to-move side-to-move]))
    (define square-size 0)
    (define image-size 0)
    (define start-x 0)
    (define start-y 0)
    (define selected-square '())
    (define drag-x 0)
    (define drag-y 0)
    (define legal-moves '())

    (define/private (get-sq event)
      (let* ([x (- (send event get-x) start-x)]
             [y (- (send event get-y) start-y)]
             [xsq (quotient x square-size)]
             [ysq (- 7 (quotient y square-size))])
        (cons xsq ysq)))

    (define/private (place-piece event)
      (let* ([sq (get-sq event)])
        (if (member sq legal-moves)
            (send board-position make-move selected-square sq)
            '())
        (set! selected-square '())
        (set! legal-moves '())
        (refresh)))
    
    (define/private (handle-click event)
      (if (null? selected-square)
          (let* ([x (- (send event get-x) start-x)]
                 [y (- (send event get-y) start-y)]
                 [sq (get-sq event)]
                 [xc (+ (* (car sq) square-size) (quotient square-size 2))]
                 [yc (+ (* (- 7 (cdr sq)) square-size)
                        (quotient square-size 2))])
            (if (and (send board-position has-piece-at? sq)
                     (<= (abs (- x xc)) (quotient image-size 2))
                     (<= (abs (- y yc)) (quotient image-size 2))
                     (equal? (get-piece-color (send board-position
                                                    get-piece-at sq))
                             (send board-position get-to-move)))
                (begin
                  (set! selected-square sq)
                  (set! drag-x (send event get-x))
                  (set! drag-y (send event get-y))
                  (set! legal-moves (get-legal-moves
                                     board-position
                                     (send board-position get-piece-at
                                           selected-square)
                                     selected-square))
                  (refresh))
                '()))
          (place-piece event)))

    (define/private (handle-release event)
      (let* ([sq (get-sq event)])
        (if (equal? sq selected-square)
            (let* ([x (- (send event get-x) start-x)]
                 [y (- (send event get-y) start-y)]
                 [xc (+ (* (car sq) square-size) (quotient square-size 2))]
                 [yc (+ (* (- 7 (cdr sq)) square-size)
                        (quotient square-size 2))])
              (set! drag-x xc)
              (set! drag-y yc)
              (refresh))
            (place-piece event))))

    (define/private (handle-dragging event)
      (if (null? selected-square)
          '()
          (begin
            (set! drag-x (send event get-x))
            (set! drag-y (send event get-y))
            (refresh))))
    
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
      (let* ([sx (+ start-x (* i square-size))]
             [sy (- height (+ start-y (* j square-size)) square-size)]
             [selected (and (not (null? selected-square))
                            (= i (car selected-square))
                            (= j (cdr selected-square)))]
             [color (cond [selected (make-color 255 255 0)]
                          [(zero? (remainder (+ i j) 2))
                           (make-color 118 150 86)]
                          [else (make-color 238 238 210)])])
        (send dc set-brush color 'solid)
        (send dc set-pen "white" 1 'transparent)
        (send dc draw-rectangle sx sy square-size square-size)
        (if (send board-position has-piece-at? (cons i j))
            (let* ([x (- (+ sx (quotient square-size 2))
                         (quotient image-size 2) 1)]
                   [y (- (+ sy (quotient square-size 2))
                         (quotient image-size 2) 1)]
                   [img (read-bitmap
                         (piece-image-fn
                          image-size
                          (send board-position get-piece-at (cons i j)))
                         'png/alpha)]
                   [scale-factor (/ square-size 50)])
              (if selected
                  (send dc draw-bitmap img (- drag-x (quotient image-size 2))
                        (- drag-y (quotient image-size 2)))
                  (send dc draw-bitmap img x y)))
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
            (draw-piece i j)))
        (define (draw-dot sq)
          (let ([x (+ st-x (* (car sq) sq-size) (quotient sq-size 2))]
                [y (- height (+ st-y (* (cdr sq) sq-size)
                                (quotient sq-size 2)))])
            (send dc draw-ellipse (- x 5) (- y 5) 10 10)))
        (if (not (null? selected-square))
            (begin
              (send dc set-brush (make-color 0 255 0 0.5) 'solid)
              (send dc set-pen "white" 1 'transparent)
              (map draw-dot legal-moves)
              (draw-piece (car selected-square) (cdr selected-square)))
            '())))
    
    (super-new)))

;;; Main code

(define (main)
  (define frame (new frame% [label "Repertoire Explorer"]))
  (new chess-board%
       [parent frame]
       [min-width 400]
       [min-height 400])
  (send frame show #t))
