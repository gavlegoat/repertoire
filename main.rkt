#lang racket/gui

(require framework)

(require "util.rkt")
(require "chess.rkt")
(require "repertoire.rkt")

;; Choose a filename based on the piece type and size.
(define/contract (piece-image-fn size piece)
  (-> integer? piece? string?)
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
    (init [fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"])

    ; The current position on the board
    (define board-position (new position% [fen fen]))
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

    (define/public (get-board-position)
      board-position)
    
    (define/public (set-move-callback f)
      (set! move-callback f))

    (define/public (get-position-fen)
      (send board-position get-fen))

    (define/public (set-position fen)
      (set! selected-square '())
      (set! legal-moves '())
      (set! board-position (new position% [fen fen]))
      (refresh))

    (define/public (get-move-algebraic move)
      (send board-position get-algebraic move))
    
    ; Given a mouse event, determine which square of the board
    ; the mouse is in.
    (define/private (get-sq event)
      (let* ([x (- (send event get-x) start-x)]
             [y (- (send event get-y) start-y)]
             [xsq (quotient x square-size)]
             [ysq (- 7 (quotient y square-size))])
        (cons xsq ysq)))

    (define/public (make-move move)
      (send board-position make-move (car move) (cdr move))
      (move-callback move)
      (set! selected-square '())
      (set! legal-moves '())
      (refresh))
    
    ; Move a piece from selected-square to the square currently under
    ; the cursor if that is a legal move.
    (define/private (place-piece event)
      (let* ([sq (get-sq event)])
        (if (member sq legal-moves)
            (make-move (cons selected-square sq))
            (begin
              (set! selected-square '())
              (set! legal-moves '())
              (refresh)))))

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
                     (equal? (get-color (send board-position
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
            (let* ([xc (+ start-x (* (car sq) square-size)
                          (quotient square-size 2))]
                   [yc (+ start-y (* (- 7 (cdr sq)) square-size)
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
        [else (void)]))

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

    ;; If the user has clicked a square to start a move, forget that square.
    (define/public (reset-mouse)
      (set! selected-square '())
      (set! legal-moves '())
      (refresh))
    
    (super-new)))

(define/contract chess-board+c%
  (class/c
   [get-board-position (->m (is-a?/c position%))]
   [get-position-fen (->m string?)]
   [set-position (->m string? void?)]
   [get-move-algebraic (->m square? string?)]
   [make-move (->m move? void?)]
   [reset-mouse (->m void?)])
  chess-board%)

;; The navigation panel allows the user to change the position on the board in
;; ways other than by moving the pieces (for example, returning to a previous
;; position).
;; The panel layout is:
;; +-------------------------+
;; |     Start    Back       |
;; |   History    Next moves |
;; +-------------------------+
(define navigation-panel%
  (class object%
    (init controller)
    (init parent)

    (define panel (new vertical-panel%
                       [parent parent]
                       [stretchable-height #f]))
    (define button-panel (new horizontal-panel%
                              [parent panel]))
    
    (define ctrl controller)
    
    (define start (new button%
                       [parent button-panel]
                       [label "<<"]
                       [callback (lambda (b e)
                                   (send ctrl set-start-position))]))
    
    (define back (new button%
                      [parent button-panel]
                      [label "<"]
                      [callback (lambda (b e)
                                  (send ctrl set-previous-position))]))

    ; History and next moves
    (define move-panel (new horizontal-panel%
                            [parent panel]))
    (define history-panel (new vertical-panel%
                               [parent move-panel]
                               [style '(auto-vscroll)]
                               [min-height 200]))

    (define next-moves-panel (new vertical-panel%
                                  [parent move-panel]
                                  [style '(auto-vscroll)]))
    
    ; Add history to history-panel and make clickable
    (define/public (populate-history-panel moves)
      ; Clear the current contents of the panel
      (let ([children (send history-panel get-children)])
        (map (lambda (c) (send history-panel delete-child c)) children))
      ; For each pair of moves, add a horizontal panel with two buttons
      (define (add-row ms ind)
        (define (make-row-panel idx)
          (let ([p (new horizontal-panel%
                        [parent history-panel]
                        [alignment '(left center)]
                        [stretchable-height #f])])
            (new message% [label (~a idx #\.)] [parent p])
            p))
        (cond [(null? ms) '()]
              [(null? (cdr ms))
               (let ([p (make-row-panel ind)])
                 (new button%
                      [parent p]
                      [label (car ms)]
                      [callback (lambda (b e)
                                  (send ctrl return-to (car ms)))]))]
              [else
               (let ([p (make-row-panel ind)])
                 (new button%
                      [parent p]
                      [label (car ms)]
                      [callback (lambda (b e)
                                  (send ctrl return-to (car ms)))])
                 (new button%
                      [parent p]
                      [label (cadr ms)]
                      [callback (lambda (b e)
                                  (send ctrl return-to (cadr ms)))]))
               (add-row (cddr ms) (+ ind 1))]))
      (add-row (reverse moves) 1))

    ;; Add a button to the navigation panel for each move from the current
    ;; position which is included in the repertoire.
    (define/public (populate-next-moves moves labels)
      (let ([children (send next-moves-panel get-children)])
        (map (lambda (c) (send next-moves-panel delete-child c)) children))
      (for ([m (map cons moves labels)])
        (new button%
             [parent next-moves-panel]
             [label (cdr m)]
             [callback (lambda (b e)
                         (send ctrl advance-board-position (car m))
                         (send ctrl make-move (car m)))])))
    
    (super-new)))

(define/contract navigation-panel+c%
  (class/c
   [populate-history-panel (->m (listof string?) void?)]
   [populate-next-moves
    (->m (listof move?) (listof string?) void?)])
  navigation-panel%)

;; The controller class coordinates all of the GUI elements and the repertoire.
(define controller%
  (class object%
    ; A chess-board% object for interacting with positions
    (define panel #f)
    ; A text% object for interacting with annotations
    (define editor #f)
    ; A navigation-panel% object for interacting with the position.
    (define navigation #f)
    ; A repertoire% object for storing data
    (define repertoire #f)
    ; History is a list of positions which have been set up on the board.
    ; The first element of the list is the current board position.
    (define history '("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"))
    ; move-history holds a string representation of each move.
    (define move-history '())
    ; filename is the name of the file associated with the current repertoire,
    ; if one exists
    (define filename #f)
    
    (define/public (set-panel pl)
      (set! panel pl))

    (define/public (set-editor ed)
      (set! editor ed))

    (define/public (set-navigator nav)
      (set! navigation nav))

    (define/public (set-repertoire r)
      (set! repertoire r))

    (define/public (get-move-history)
      move-history)

    ;; Update the given position in the repertoire with the current contents of
    ;; the editor.
    (define/private (update-repertoire-annotation pos)
      (send repertoire set-annotation pos (send editor get-text)))
    
    ;; Send all changes which have been made in the text panel to the
    ;; repertoire. If the current position is not in the repertoire, add it and
    ;; add a link from the previous position.
    ;; NOTE: At the point where this is called, the text editor has not been
    ;; changed to the new position, but the board panel has.
    (define/private (update-repertoire-last move)
      ; Note that this function should never be called when (cdr history) is
      ; null.
      (let ([last-pos (cadr history)])
        (update-repertoire-annotation last-pos)
        ; Update the move mapping.
        (send repertoire add-move last-pos move)))
    
    ;; Whenever a move is made on the chessboard, update the annotation panel
    ;; as necessary.
    (define/public (update-annotation)
      (send navigation populate-history-panel move-history)
      (let ([pos (send panel get-position-fen)]
            [board (send panel get-board-position)])
        (let ([text (send repertoire get-annotation pos)]
              [moves (send repertoire get-next-moves pos)])
          (send editor erase)
          (send editor insert text 0)
          (define (get-label move)
            (send board make-move (car move) (cdr move))
            (let ([alg (send board get-algebraic move)])
              (send board unmake-move)
              alg))
          (let ([labels (map get-label moves)])
            (send navigation populate-next-moves moves labels)))))

    ;; When a move is made on the board, we add it to the position history.
    (define/public (make-move move)
      (let ([pos (send panel get-position-fen)])
        (set! history (cons pos history))
        (set! move-history
              (cons (send panel get-move-algebraic move) move-history))
        (update-repertoire-last move)
        (update-annotation)))

    ;; Return to the last position in the history (if one is available)
    (define/public (set-previous-position)
      (update-repertoire-annotation (send panel get-position-fen))
      (let ([cur (car history)])
        (set! history (cdr history))
        (if (null? move-history)
            '()
            (set! move-history (cdr move-history)))
        (if (null? history)
            (set! history (list cur))
            (send panel set-position (car history)))
        (update-annotation)))
    
    ;; Load an arbitrary position.
    (define/public (set-position fen)
      (update-repertoire-annotation (send panel get-position-fen))
      (send panel set-position fen)
      ; If the position is manually set, we should also clear the history
      (set! history (list fen))
      (set! move-history '())
      (update-annotation))

    ;; Load the start position.
    (define/public (set-start-position)
      (set-position "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"))

    ;; Return to the position after one of the moves in move-history.
    (define/public (return-to move)
      (update-repertoire-annotation (send panel get-position-fen))
      (define (helper hist m-hist)
        (if (equal? (car m-hist) move)
            (begin
              (set-position (car hist))
              (set! history hist)
              (set! move-history m-hist)
              (update-annotation))
            (helper (cdr hist) (cdr m-hist))))
      (helper history move-history))

    ;; Make a move on the board. Note that this is different from make-move,
    ;; which assumes a moves has already been made and then updates all other
    ;; information accordingly.
    (define/public (advance-board-position move)
      (let ([pos (send panel get-board-position)])
        (send pos make-move (car move) (cdr move))
        (send panel reset-mouse)))

    ;; Save the repertoire at the given filename
    (define/private (save-to-file fn)
      (update-repertoire-annotation (car history))
      (send repertoire write-to-file fn)
      (set! filename fn))
    
    ;; Save the repertoire. If there is a filename associated with the current
    ;; repertoire, we save it there, otherwise open a dialog to ask the user
    ;; where to save.
    (define/public (save-repertoire)
      (if filename
          (save-to-file filename)
          (save-repertoire-as)))

    ;; Open a dialog to ask the user where to save the repertoire.
    (define/public (save-repertoire-as)
      (let ([fn (get-file "Save...")])
        (if fn
            (save-to-file fn)
            ; If the user cancels the dialog, do nothing
            (void))))

    ;; Open a dialog to select a file then open a repertoire from that file.
    (define/public (open-repertoire)
      (let ([fn (get-file "Open...")])
        (if fn
            (begin
              (set-repertoire (new repertoire% [filename fn]))
              (update-annotation)
              (set! filename fn))
            (void))))

    (super-new)))

(define/contract controller+c%
  (class/c
   [set-panel (->m (is-a?/c panel%) void?)]
   [set-editor (->m (is-a?/c text%) void?)]
   [set-navigator (->m (is-a?/c navigation-panel%) void?)]
   [set-repertoire (->m (is-a?/c repertoire%) void?)]
   [get-move-history (->m (listof string?))]
   [update-annotation (->m void?)]
   [advance-board-position (->m move? void?)]
   [make-move (->m move? void?)]
   [set-previous-position (->m void?)]
   [set-position (->m string? void?)]
   [set-start-position (->m void?)]
   [return-to (->m string? void?)]
   [save-repertoire (->m void?)]
   [save-repertoire-as (->m void?)]
   [open-repertoire (->m void?)])
  controller%)

;;; Main code

;; The window set up is like this:
;; +------------+------------+
;; |            |            |
;; |            |            |
;; |   Board    |            |
;; |            |    Notes   |
;; |            |            |
;; +------------+            |
;; | Navigation |            |
;; +------------+------------+
;; Where the navigation panel shows a history of moves played and back buttons.

(define (main)
  (define frame (new frame% [label "Repertoire Explorer"]))
  ; The controller synchronizes the GUI elements and repertoire
  (define controller (new controller%))
  (define menu-bar (new menu-bar% [parent frame]))
  (define file (new menu% [label "&File"] [parent menu-bar]))
  ; TODO: Menu item callbacks
  (new menu-item%
       [label "&Save"]
       [parent file]
       [shortcut #\s]
       [shortcut-prefix '(ctl)]
       [callback (lambda (i e) (send controller save-repertoire))])
  (new menu-item%
       [label "Save &As..."]
       [parent file]
       [shortcut #\s]
       [shortcut-prefix '(ctl shift)]
       [callback (lambda (i e) (send controller save-repertoire-as))])
  (new menu-item%
       [label "&Open..."]
       [parent file]
       [shortcut #\o]
       [shortcut-prefix '(ctl)]
       [callback (lambda (i e) (send controller open-repertoire))])
  ; The main panel allows the user to resize the two sides of the application 
  (define main-panel (new panel:horizontal-dragable% [parent frame]))
  ; The editor holds the repertoire notes on a position and allows them to be
  ; edited.
  (define editor (new text%))
  (send controller set-editor editor)
  ; The left panel allows the user to resize the board and navigation areas.
  (define left-panel (new panel:vertical-dragable% [parent main-panel]))
  (define panel (new chess-board%
                     [parent left-panel]
                     [min-width 400]
                     [min-height 400]))
  (define navigation
    (new navigation-panel% [controller controller] [parent left-panel]))
  (send controller set-panel panel)
  (send controller set-navigator navigation)
  ; Add a callback to the chessboard which tells the controller when a piece is
  ; moved.
  (send panel set-move-callback
        (lambda (move) (send controller make-move move)))
  (send controller set-repertoire (new repertoire%))
  (send controller update-annotation)
  ; The editor canvas is necessary to display the editor.
  (define editor-canvas (new editor-canvas%
                             [parent main-panel]
                             [editor editor]
                             [min-width 400]))
  (send frame show #t))
