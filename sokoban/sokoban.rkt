#lang racket/base
(require racket/class
         racket/vector
         racket/unit
         racket/logging
         racket/gui/base
         pict
         "./sokoban-lib.rkt")

(provide game@)

(define game@
  (unit
    (import)
    (export)

    (start-game)))

(define (board-at b x y)
  (vector-ref (vector-ref b y) x))

(define (set-board-at! b x y what)
  (define row (vector-ref b y))
  (vector-set! row x what))

(define (copy-board b)
  (for/vector ([r b])
    (vector-copy r)))

;;;; board-pict creates a pict from the board, by looking using tile-lookup
(define (board-pict board)
  (for/fold ([total (blank 0)])
            ([row board])
    (vl-append total
               (for/fold ([start (blank 0)])
                         ([col row])
                 (htl-append start (tile-lookup col))))))

;;;; wins? returns #t if no piece is a 'goal
(define (wins? board)
  (empty? (for/fold ([accum empty])
              ([row board])
            (if (or (vector-memq 'goal row)
                    (vector-memq 'avatar-on-goal row))
                (cons #t accum)
                accum))))

(define (label-text p val)
  (string-append p (number->string val)))

(define callback-view%
  (class canvas%
    (inherit get-dc get-client-size)
    (init-field parent key-handler draw)

    (define/public redraw
      (lambda ()
        (let ([dc (get-dc)])
          (draw dc))))

    (define/override on-char
      (lambda (evt)
        (log-debug (format "~a\n" evt))
        (key-handler evt)))

    (define/override on-paint
      (lambda ()
        (send (get-dc) clear)
        (redraw)))

    (super-new [parent parent])))


(define (start-game)
  (define moves 0)
  (define player-x 0)
  (define player-y 0)

  (define board
    (vector
     (vector 'floor 'floor 'floor 'floor 'floor 'floor)
     (vector 'floor 'floor 'floor 'floor 'floor 'floor)
     (vector 'floor 'floor 'floor 'floor 'floor 'floor)
     (vector 'floor 'floor 'floor 'floor 'floor 'floor)
     (vector 'floor 'floor 'floor 'floor 'floor 'floor)
     (vector 'floor 'floor 'floor 'floor 'floor 'floor)
     (vector 'floor 'goal 'floor 'floor 'floor 'floor)))

  (define (move dx dy)
    (set! moves (add1 moves))

    ;; potential new spot.
    (define tx (+ dx player-x))
    (define ty (+ dy player-y))

    (set-board-at! board player-x player-y 'floor)
    (set-board-at! board tx ty 'avatar)

    (set! player-x (+ dx player-x))
    (set! player-y (+ dy player-y))

    (send frame set-status-text (label-text "Moves: " moves))
    (send view redraw)

    (when (wins? board)
      (display "YOU'VE WON!")))

  (define (key-handler evt)
    (case (send evt get-key-code)
      [(#\w up) (move 0 -1)]
      [(#\a left) (move -1 0)]
      [(#\s down) (move 0 1)]
      [(#\d right) (move 1 0)]))

  (define (draw dc)
    (draw-pict (board-pict board) dc 0 0))

  (define frame (make-object frame% "Sokoban"))
  (define button-panel (make-object horizontal-panel% frame))
  (define view (make-object callback-view% frame key-handler draw))

  (send frame create-status-line)
  (send frame set-status-text (label-text "Moves: " moves))

  (make-object button% "New" button-panel
               (lambda x
                 (set! moves (+ moves 1))
                 (send frame set-status-text (label-text "Moves: " moves))))

  (make-object button% "Undo" button-panel
               (lambda x
                 (set! moves (max 0 (- moves 1)))
                 (send frame set-status-text (label-text "Moves: " moves))))

  (send button-panel stretchable-height #f)
  (send frame min-width 500)
  (send frame min-height 500)

  (send frame show #t))
