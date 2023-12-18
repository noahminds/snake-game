;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./snake_lib.rkt")

; a game is...
; - (make-game snake (listof posn) (listof posn) number)
; (define-struct game (snake food obstacles ticks))
;selectors - game-snake, game-food, game-obstacles, game-ticks
;constructor - make-game

; a direction is one of...
; - 'up
; - 'down
; - 'left
; - 'right
; If this type looks new to you, its just a symbol.
; That is ‘up is a symbol and “up” is a string.
; Symbols are like strings without spaces. 


; a snake is...
; - (make-snake direction (listof posn))
; (define-struct snake (heading segments))

; segments is either
; - (cons posn empty)
; - (cons posn segments)
; That is, segments is a non-empty list of posns. 
; x-coordinates increase from 1 to board-length (inclusive) toward the right
; y-coordinates increase from 1 to board-length (inclusive) toward the top
; the default value for board-length is 50.

; food is either
; - empty
; - (cons posn food)
; That is, food is a list of posns.

; obstacles is either
; - empty
; - (cons posn obstacles)
; Obstacles is also a list of posns.

; add-food : game posn -> game
; Given a game and posn, returns a new game (so you want to call make-game here)
; where food has been added
; at that posn. 
(define (add-food g p)
  (make-game (make-snake (snake-heading (game-snake g))
                         (snake-segments (game-snake g)))
             (cons p (game-food g))
             (game-obstacles g)
             (game-ticks g)))

(check-expect
 (add-food (make-game (make-snake 'up (list (make-posn 1 2)))
                      (list (make-posn 3 4))
                      (list (make-posn 10 10)
                            (make-posn 20 20))
                      5)
           (make-posn 6 7))
 (make-game (make-snake 'up (list (make-posn 1 2)))
            (list (make-posn 6 7) (make-posn 3 4))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            5))

(check-expect
 (add-food (make-game (make-snake 'left (list (make-posn 1 2)))
                      (list (make-posn 3 4))
                      (list (make-posn 10 10)
                            (make-posn 20 20))
                      3)
           (make-posn 8 1))
 (make-game (make-snake 'left (list (make-posn 1 2)))
            (list (make-posn 8 1) (make-posn 3 4))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            3))

; change-direction : game direction -> game
; Given a game and direction, returns a new game where the snake
;   is now headed in the provided direction. 
(define (change-direction g d)
  (make-game (make-snake d (snake-segments (game-snake g)))
             (game-food g)
             (game-obstacles g)
             (game-ticks g)))

(check-expect
 (change-direction
  (make-game (make-snake 'down (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5)
  'left)
 (make-game (make-snake 'left (list (make-posn 1 2)))
            (list (make-posn 3 4))
            empty
            5))

(check-expect
 (change-direction
  (make-game (make-snake 'down (list (make-posn 2 3)))
             (list (make-posn 5 6))
             (list (make-posn 12 12))
             3)
  'up)
 (make-game (make-snake 'up (list (make-posn 2 3)))
            (list (make-posn 5 6))
            (list (make-posn 12 12))
            3))

; game-score : game -> number
; Given a game, returns a score (as a number)
(define (game-score g)
  (- (* 100
        (length (snake-segments (game-snake g))))
     (game-ticks g)))

(check-expect (game-score (make-game (make-snake 'left (list (make-posn 1 2)))
                                     (list (make-posn 8 1) (make-posn 3 4))
                                     (list (make-posn 10 10)
                                           (make-posn 20 20))
                                     3))
              97)
(check-expect (game-score (make-game (make-snake 'left (list (make-posn 10 10)
                                                             (make-posn 10 9)
                                                             (make-posn 9 9)))
                                     (list (make-posn 8 1) (make-posn 3 4))
                                     (list (make-posn 10 10)
                                           (make-posn 20 20))
                                     40))
              260)

; no tests are provided for game-score because it is open-ended
; feel free to implement it however you would like to

; game-over? : game -> boolean
; Given a game, returns true if that snake has died and false otherwise.
; We strongly recommend writing helper functions for this question!

; col-itself? : game -> boolean
; Given a game, returns true if the snake collides with itself.
(define (col-itself? g)
  (or (and (> (length (snake-segments (game-snake g))) 2)
           (ormap (λ(s)
                    (equal? (first (snake-segments (game-snake g)))
                            s))
                  (rest (snake-segments (game-snake g)))))
      (empty? (snake-segments (game-snake g)))))

; col-wall? : game -> boolean
; Given a game, returns true if the snake collides with a wall.
(define (col-wall? g)
  (or (< (posn-x (first (snake-segments (game-snake g)))) 1)
      (< (posn-y (first (snake-segments (game-snake g)))) 1)
      (> (posn-x (first (snake-segments (game-snake g)))) 50)
      (> (posn-y (first (snake-segments (game-snake g)))) 50)))

; col-obstacle? : game -> boolean
; Given a game, returns true if the snake collides with an obstacle.
(define (col-obstacle? g)
  (ormap (λ(o)
           (equal? (first (snake-segments (game-snake g)))
                   o))
         (game-obstacles g)))

(define (game-over? g)
  (or (col-itself? g)
      (col-wall? g)
      (col-obstacle? g)))

(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 1 1))) empty empty 5))
 false)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn -1 1))) empty empty 5))
 true)
(check-expect (game-over? (make-game (make-snake 'up empty) empty empty 3))
              true)
(check-expect (game-over? (make-game (make-snake 'up (list (make-posn 10 10)
                                                           (make-posn 10 9)
                                                           (make-posn 9 9)
                                                           (make-posn 9 10)
                                                           (make-posn 10 10)
                                                           (make-posn 11 10)
                                                           (make-posn 12 10)))
                                     empty
                                     empty
                                     40))
              true)
(check-expect (game-over? (make-game (make-snake 'up (list (make-posn 11 9)
                                                           (make-posn 10 9)
                                                           (make-posn 9 9)
                                                           (make-posn 9 10)
                                                           (make-posn 10 10)
                                                           (make-posn 11 10)
                                                           (make-posn 12 10)))
                                     empty
                                     empty
                                     40))
              false)
(check-expect (game-over? (make-game (make-snake 'up (list (make-posn 10 10)
                                                           (make-posn 10 9)))
                                     empty
                                     (list (make-posn 10 10)
                                           (make-posn 34 3))
                                     24))
              true)
(check-expect (game-over? (make-game (make-snake 'down (list (make-posn 10 8)
                                                             (make-posn 10 8)))
                                     empty
                                     (list (make-posn 10 10)
                                           (make-posn 34 3))
                                     24))
              false)
(check-expect (game-over? (make-game (make-snake 'left (list (make-posn 9 9)
                                                             (make-posn 10 9)))
                                     empty
                                     (list (make-posn 10 10)
                                           (make-posn 34 3))
                                     24))
              false)

; advance-game : game -> game
; Takes a game as input and advances the game one tick. The snake
;  moves forward one segment and eats or not. 

; add-seg : game -> snake
; Adds a segment to the head of a snake at a coordinate determined by the by the segment that was
; previously at the front of the snake and the direction the snake is heading.
(define (add-seg g)
  (cond [(equal? 'up (snake-heading (game-snake g)))      (make-snake (snake-heading (game-snake g))
                                                                      (cons (make-posn (posn-x (first (snake-segments (game-snake g))))
                                                                                       (+ 1 (posn-y (first (snake-segments (game-snake g))))))
                                                                            (snake-segments (game-snake g))))]
        [(equal? 'down (snake-heading (game-snake g)))    (make-snake (snake-heading (game-snake g))
                                                                      (cons (make-posn (posn-x (first (snake-segments (game-snake g))))
                                                                                       (- (posn-y (first (snake-segments (game-snake g)))) 1))
                                                                            (snake-segments (game-snake g))))]
        [(equal? 'right (snake-heading (game-snake g)))   (make-snake (snake-heading (game-snake g))
                                                                      (cons (make-posn (+ 1 (posn-x (first (snake-segments (game-snake g)))))
                                                                                       (posn-y (first (snake-segments (game-snake g)))))
                                                                            (snake-segments (game-snake g))))]
        [(equal? 'left (snake-heading (game-snake g)))   (make-snake (snake-heading (game-snake g))
                                                                     (cons (make-posn (- (posn-x (first (snake-segments (game-snake g)))) 1)
                                                                                      (posn-y (first (snake-segments (game-snake g)))))
                                                                           (snake-segments (game-snake g))))]))

; col-food? : game -> boolean
; Given a game, returns true if the snake will collide with food following a direction change.
(define (col-food? g)
  (ormap (λ(f)
           (equal? (first (snake-segments (add-seg g)))
                   f))
         (game-food g)))

(check-expect (col-food? (make-game (make-snake 'up (list (make-posn 10 10)
                                                          (make-posn 10 9)))
                                    (list (make-posn 10 11)
                                          (make-posn 34 3))
                                    empty
                                    24))
              true)

(define (advance-game g)
  (cond [(col-food? g)   (make-game (add-seg g)
                                    (remove (first (snake-segments (add-seg g)))
                                            (game-food g))
                                    (game-obstacles g)
                                    (+ 1 (game-ticks g)))]
        [else            (make-game (make-snake (snake-heading (game-snake g))
                                                (reverse (rest (reverse (snake-segments (add-seg g))))))
                                    (game-food g)
                                    (game-obstacles g)
                                    (+ 1 (game-ticks g)))]))

(check-expect
 (advance-game
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             empty
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)))
            empty
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))
(check-expect
 (advance-game
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             (list (make-posn 2 1) (make-posn 8 9))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)
                                    (make-posn 3 3)))
            (list (make-posn 8 9))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))

; a starting game to experiment with
(define game-start
  (make-game (make-snake 'up (list (make-posn 12 12)))
             (list (make-posn 2 2) 
                   (make-posn 5 20)
                   (make-posn 15 15)
                   (make-posn 24 24))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             0))

;; play : game -> game
(define (play initial-game)
  (play-game initial-game advance-game add-food change-direction game-score game-over?))

;to start a game
(play game-start)

;to make sure all procedures are defined with no typos
(check-expect (procedure? add-food) #true)
(check-expect (procedure? change-direction) #true)
(check-expect (procedure? game-score) #true)
(check-expect (procedure? game-over?) #true)
(check-expect (procedure? advance-game) #true)