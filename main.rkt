#lang racket/gui

(require framework)

(require "util.rkt")
(require "chess.rkt")
(require "repertoire.rkt")

;; Choose a filename based on the piece type and size.
(define (piece-image-fn size piece)
  (let ([basename
         (switch piece
           ['black-rook   "Chess_rdt.png"]
           ['black-knight "Chess_ndt.png"]
           ['black-bishop "Chess_bdt.png"]
           ['black-queen  "Chess_qdt.png"]
           ['black-king   "Chess_kdt.png"]
           ['black-pawn   "Chess_pdt.png"]
           ['white-rook   "Chess_rlt.png"]
           ['white-knight "Chess_nlt.png"]
           ['white-bishop "Chess_blt.png"]
           ['white-queen  "Chess_qlt.png"]
           ['white-king   "Chess_klt.png"]
           ['white-pawn   "Chess_plt.png"]
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

    ; The current position on the board
    (define board-position (new position% [position position]
                                          [castling castling]
                                          [en-passant en-passant]
                                          [side-to-move side-to-move]))
    ; The size of one square of the chess board.
    (define square-size 0)
    ; The size of the images of the pieces.
    (define image-size 0)
    ; The x value of the upper left corner of the clicked square
    (define start-x 0)
    ; The y value of the upper left corner of the clicked square
    (define start-y 0)
    ; The square which is currently highlighted (or '() for none)
    (define selected-square '())
    ; The x coordinate of the cursor while dragging
    (define drag-x 0)
    ; The y coordinate of the cursor while dragging
    (define drag-y 0)
    ; A list of moves that are legal in the current position.
    (define legal-moves '())
    ; A callback to be called whenever a piece is moved
    (define move-callback (lambda (move) '()))

    (define/public (set-move-callback f)
      (set! move-callback f))

    (define/public (get-position-fen)
      (send board-position get-fen))
    
    ; Given a mouse event, determine which square of the board
    ; the mouse is in.
    (define/private (get-sq event)
      (let* ([x (- (send event get-x) start-x)]
             [y (- (send event get-y) start-y)]
             [xsq (quotient x square-size)]
             [ysq (- 7 (quotient y square-size))])
        (cons xsq ysq)))

    ; Move a piece from selected-square to the square currently under
    ; the cursor if that is a legal move.
    (define/private (place-piece event)
      (let* ([sq (get-sq event)])
        (if (member sq legal-moves)
            (begin
              (send board-position make-move selected-square sq)
              (move-callback (cons selected-square sq)))
            '()))
      (set! selected-square '())
      (set! legal-moves '())
      (refresh))

    ; Determine what to do when the mouse is clicked.
    (define/private (handle-click event)
      (if (null? selected-square)
          ; If no square is currently selected
          (let* ([x (- (send event get-x) start-x)]
                 [y (- (send event get-y) start-y)]
                 [sq (get-sq event)]
                 [xc (+ (* (car sq) square-size) (quotient square-size 2))]
                 [yc (+ (* (- 7 (cdr sq)) square-size)
                        (quotient square-size 2))])
            ; If there is a piece at the square under the cursor
            (if (and (send board-position has-piece-at? sq)
                     (<= (abs (- x xc)) (quotient image-size 2))
                     (<= (abs (- y yc)) (quotient image-size 2))
                     (equal? (get-piece-color (send board-position
                                                    get-piece-at sq))
                             (send board-position get-to-move)))
                (begin
                  ; Highlight that square and prepare dragging variables.
                  (set! selected-square sq)
                  (set! drag-x (send event get-x))
                  (set! drag-y (send event get-y))
                  (set! legal-moves (get-legal-moves
                                     board-position
                                     (send board-position get-piece-at
                                           selected-square)
                                     selected-square))
                  (refresh))
                ; If there is not a piece on the square, do nothing
                '()))
          ; If the selected square is not none, then move a piece
          ; This indicates that the user has clicked a start square
          ; and end square.
          (place-piece event)))

    ; Determine what to do on a mouse release event.
    (define/private (handle-release event)
      (let* ([sq (get-sq event)])
        ; If the mouse is released in the same square where it was clicked
        (if (equal? sq selected-square)
            ; Simply reset the dragging variables.
            (let* ([x (- (send event get-x) start-x)]
                   [y (- (send event get-y) start-y)]
                   [xc (+ (* (car sq) square-size) (quotient square-size 2))]
                   [yc (+ (* (- 7 (cdr sq)) square-size)
                          (quotient square-size 2))])
              (set! drag-x xc)
              (set! drag-y yc)
              (refresh))
            ; Otherwise move a piece. This indcates that the user clicked and
            ; dragged a piece to move it.
            (place-piece event))))

    ; Determine what to do when the mouse is being dragged.
    (define/private (handle-dragging event)
      (if (null? selected-square)
          ; If the selected square is none, then there's no need to do anything
          '()
          ; Otherwise update the dragging variables.
          (begin
            (set! drag-x (send event get-x))
            (set! drag-y (send event get-y))
            (refresh))))

    ; Determine what to do with an arbitrary mouse event.
    (define/override (on-event event)
      (cond
        [(send event button-down? 'left) (handle-click event)]
        [(send event button-up? 'left) (handle-release event)]
        [(send event dragging?) (handle-dragging event)]
        [else '()]))

    ; Draw the appropriate piece at the given square.
    (define/private (draw-piece i j)
      (define dc (get-dc))
      (define width (get-width))
      (define height (get-height))
      (let* ([sx (+ start-x (* i square-size))]
             [sy (- height (+ start-y (* j square-size)) square-size)]
             ; Determine whether this square is the selected square
             [selected (and (not (null? selected-square))
                            (= i (car selected-square))
                            (= j (cdr selected-square)))]
             ; Choose a background color for this square.
             [color (cond [selected (make-color 255 255 0)]
                          [(zero? (remainder (+ i j) 2))
                           (make-color 118 150 86)]
                          [else (make-color 238 238 210)])])
        ; Draw the square background
        (send dc set-brush color 'solid)
        (send dc set-pen "white" 1 'transparent)
        (send dc draw-rectangle sx sy square-size square-size)
        ; If there is a piece at this square
        (if (send board-position has-piece-at? (cons i j))
            (let* ([x (- (+ sx (quotient square-size 2))
                         (quotient image-size 2) 1)]
                   [y (- (+ sy (quotient square-size 2))
                         (quotient image-size 2) 1)]
                   ; Get the appropriate piece image.
                   [img (read-bitmap
                         (piece-image-fn
                          image-size
                          (send board-position get-piece-at (cons i j)))
                         'png/alpha)]
                   [scale-factor (/ square-size 50)])
              ; If this square is selected
              (if selected
                  ; Draw the piece at this square at the current drag
                  ; coordinates (so that the piece moves with the cursor).
                  (send dc draw-bitmap img (- drag-x (quotient image-size 2))
                        (- drag-y (quotient image-size 2)))
                  ; Otherwise draw that piece on the chosen square.
                  (send dc draw-bitmap img x y)))
            ; If there is not a piece at this square, there's nothing to do.
            '())))
    
    ; Draw the current board position.
    (define/override (on-paint)
      (define dc (get-dc))
      (define width (get-width))
      (define height (get-height))
      ; Choose an appropriate image size given the size of each square.
      (define (choose-image-size square-size)
        (cond [(>= square-size 80) 75]
              [(>= square-size 65) 60]
              [else 45]))
      ; Compute several sizes of the current window and update internal
      ; variables appropriately.
      (let* ([total-size (min width height)]
             [sq-size (quotient total-size 8)]
             [st-x (quotient (- width (* sq-size 8)) 2)]
             [st-y (quotient (- height (* sq-size 8)) 2)]
             [size (choose-image-size square-size)])
        (set! square-size sq-size)
        (set! start-x st-x)
        (set! start-y st-y)
        (set! image-size size)

        ; Draw each square of the chess board
        (for ([i 8])
          (for ([j 8])
            (draw-piece i j)))
        ; This function draws a dot on the indicated square.
        (define (draw-dot sq)
          (let ([x (+ st-x (* (car sq) sq-size) (quotient sq-size 2))]
                [y (- height (+ st-y (* (cdr sq) sq-size)
                                (quotient sq-size 2)))])
            (send dc draw-ellipse (- x 5) (- y 5) 10 10)))
        ; If the user is currently in the process of moving a piece, draw a
        ; light, partially transparent dot on each square that the piece can
        ; legally move to.
        (if (not (null? selected-square))
            (begin
              (send dc set-brush (make-color 0 255 0 0.5) 'solid)
              (send dc set-pen "white" 1 'transparent)
              (map draw-dot legal-moves)
              (draw-piece (car selected-square) (cdr selected-square)))
            '())))
    
    (super-new)))

;; The controller class coordinates all of the GUI elements and the repertoire.
(define controller%
  (class object%
    ; A chess-board% object for interacting with positions
    (define panel #f)
    ; A text% object for interacting with annotations
    (define editor #f)
    ; A repertoire% object for storing data
    (define repertoire #f)

    (define/public (set-panel pl)
      (set! panel pl))

    (define/public (set-editor ed)
      (set! editor ed))

    (define/public (set-repertoire r)
      (set! repertoire r))

    ;; Whenever a move is made on the chessboard, update the annotation panel
    ;; as necessary.
    (define/public (update-annotation)
      (let ([pos (send panel get-position-fen)])
        (let ([text (send repertoire get-annotation pos)])
          (send editor erase)
          (send editor insert text 0))))
    
    (super-new)))

;;; Main code

(define (main)
  (define frame (new frame% [label "Repertoire Explorer"]))
  (define main-panel (new panel:horizontal-dragable% [parent frame]))
  (define controller (new controller%))
  (define editor (new text%))
  (send controller set-editor editor)
  (define panel (new chess-board%
                     [parent main-panel]
                     [min-width 400]
                     [min-height 400]))
  (send controller set-panel panel)
  (send panel set-move-callback
        (lambda (move) (send controller update-annotation)))
  (send controller set-repertoire (new repertoire% [filename "test.json"]))
  (send controller update-annotation)
  (define editor-canvas (new editor-canvas%
                             [parent main-panel]
                             [editor editor]
                             [min-width 400]))
  (send frame show #t))
