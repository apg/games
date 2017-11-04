#lang racket/base

(require racket/draw
         racket/function
         pict)

(provide tile-lookup)

(define B (filled-rectangle 10 10 #:color "black" #:draw-border? #f))
(define W (filled-rectangle 10 10 #:color "white" #:draw-border? #f))
(define L (filled-rectangle 10 10 #:color "lightgray" #:draw-border? #f))
(define G (filled-rectangle 10 10 #:color "gray" #:draw-border? #f))
(define D (filled-rectangle 10 10 #:color "darkgray" #:draw-border? #f))
(define M (filled-rectangle 10 10 #:color "dimgray" #:draw-border? #f))

(define (make-sprite tiles)
  (apply vl-append
         (map (lambda (xs) (apply htl-append xs))
              tiles)))

(define background
  (make-sprite
   (list (list G G G G G)
         (list G G G G G)
         (list G G G G G)
         (list G G G G G)
         (list G G G G G))))

(define avatar
  (make-sprite
   (list (list G B B B G)
         (list G L L L G)
         (list B B W B B)
         (list G B B B G)
         (list G B G B G))))

(define floor
  (make-sprite
   (list (list G G G G G)
         (list G G G G G)
         (list G G L G G)
         (list G G G G G)
         (list G G G G G))))

(define goal
  (make-sprite
   (list (list G G G G G)
         (list G B G B G)
         (list G G B G G)
         (list G B G B G)
         (list G G G G G))))

(define crate
  (make-sprite
   (list (list M D M D M)
         (list D D B D D)
         (list M B D B M)
         (list D D B D D)
         (list M D M D M))))

(define crate-on-goal
  (make-sprite
   (list (list M D M D M)
         (list D B B B D)
         (list M B B B M)
         (list D B B B D)
         (list M D M D M))))

(define wall
  (make-sprite
   (list (list M M M M M)
         (list M M M M M)
         (list M M M M M)
         (list M M M M M)
         (list M M M M M))))


(define (tile-lookup s)
  (case s
    [(wall) wall]
    [(floor) floor]
    [(background) background]
    [(avatar) avatar]
    [(avatar-on-goal) avatar]
    [(goal) goal]
    [(crate) crate]
    [(crate-on-goal) crate-on-goal]))
