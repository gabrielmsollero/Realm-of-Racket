;;;     Code for the first game of the book 'Realm of Racket - Learn to program, one game at a time!'. Obviously many of the functions could be written
;;; in a more lispy way, but I made it based on the theory given by the book up to chapter 6. All the challenges except for the moving obstacles
;;; were completed, and I tried to make it in a way that any number of snakes could be included, given enough keypads. The input and end game sections,
;;; though, aren't ready for this feature yet. All functions are properly documented and a few specific lines, which I thought were worth explaining
;;; better, are commented too.

#lang racket
(require 2htdp/image 2htdp/universe)

;; Defining structs:
(struct pit (snakes goos obstacles)) ; list of snakes, list of goos and list of obstacles.
(struct snake (dir segs eaten)       ; direction, list of segments (positions), number of goos eaten.
(struct posn (x y))                  ; structure used for positioning.
(struct goo (loc expire special?))   ; snake food. Has a location, an expire time and might be special (grows 2 segments in snake when eaten).
(struct obstacle (segs))

;; Defining constant values:
(define TICK-RATE 0.1)

;; There are 50x50 possible positions in x and y axes where the elements of the game can be. Each one is a square of 8x8 px.
(define SEG-SIZE 8)
(define SIZE 50)
(define SIZE-PX (* SIZE SEG-SIZE))

;; For practical purposes, all the images are drawn when the code is run.
(define HEAD-UP-IMG (circle (/ SEG-SIZE 2) "solid" "green"))
(define HEAD-DOWN-IMG (circle (/ SEG-SIZE 2) "solid" "green"))
(define HEAD-RIGHT-IMG (circle (/ SEG-SIZE 2) "solid" "green"))
(define HEAD-LEFT-IMG (circle (/ SEG-SIZE 2) "solid" "green"))
(define GOO-IMG (circle (/ SEG-SIZE 2) "solid" "red"))
(define SPECIAL-GOO-IMG (circle SEG-SIZE "solid" "red"))
(define SEG-IMG (circle (/ SEG-SIZE 2) "solid" "green"))
(define OBSTACLE-IMG (square SEG-SIZE "solid" "black"))

(define MT-SC (empty-scene SIZE-PX SIZE-PX)) ; background
(define EXPIRATION-TIME 150)
(define ENDGAME-TEXT-SIZE 20)
(define MAX-GOOS 10)
(define MAX-OBSTACLE-SIZE 10) ; number of segments that an obstacle can have.
(define MAX-OBSTACLES 10)
(define PADS '(("up" "down" "left" "right") ("w" "a" "s" "d"))) ; list of pads that control the snakes, one for each snake.

;; ---------- MAIN ------------

(define (start-snake)
  (big-bang (pit (list (snake "right" (list (posn 1 1)) 0)
                       (snake "left" (list (posn (sub1 SIZE) (sub1 SIZE))) 0)) ; initializing the snakes at opposite corners.
                 (fresh-goos (add1 (random (sub1 MAX-GOOS)))) ; generating a random number of goos within MAX-GOOS.
                 (generate-obstacles (add1 (random (sub1 MAX-OBSTACLES))))) ; generating a random number of obstacles within MAX-OBSTACLES.
    (on-tick next-pit TICK-RATE)
    (on-key direct-snake)
    (to-draw render-pit)
    (stop-when any-dead? render-end)))

;; -------- CLOCK TICKS ----------

;; next-pit: pit -> pit. Consumes the current pit and creates the next one.
(define (next-pit w)
  (define snakes (pit-snakes w))
  (define goos ; preventing the possibility of no goos in the pit.
    (if (empty? (pit-goos w))
        (fresh-goos (random 3))
        (pit-goos w)))
  (define obstacles (pit-obstacles w))
  (define new-snakes-and-goos-eaten (any-snake-can-eat snakes goos))
  (define new-snakes (first new-snakes-and-goos-eaten))
  (define goos-to-eat (second new-snakes-and-goos-eaten))
  (pit new-snakes ; new pit
       (age-goo (eat goos goos-to-eat))
       obstacles))

;; any-snake-can-eat: snakes-list goos-list -> '(snakes-list goos-list). Searches for any consumable goo and, if any,
;; grows the eating snake and lists the consumed goo. Snakes who don't eat any goo slither.
(define (any-snake-can-eat snakes goos)
  (cond
    [(empty? snakes) '(() ())]
    [else (define next (any-snake-can-eat (rest snakes) goos))
          (define goo-to-eat (can-eat (first snakes) goos))
          (if goo-to-eat
              (cons                                                 ; new snakes list + goos eaten list
               (cons (grow (first snakes) goo-to-eat) (first next)) ; new snakes list
               (list (cons goo-to-eat (second next))))              ; goos eaten list
              (cons                                         ; analogous
               (cons (slither (first snakes)) (first next))
               (list (second next))))]))

;; can-eat: snake goo-list -> goo. Returns a consumable goo if any (by a given snake), #f otherwise.
(define (can-eat snake goos) 
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (goo-loc (first goos)))
                  (first goos)
                  (can-eat snake (rest goos)))]))

;; close?: posn posn -> boolean. Returns true if positions are posn=? and false otherwise.
(define (close? s g)
  (posn=? s g))

;; eat: goo-list1 goo-list2 -> goo-list. Adds up to 3 new goos for each goo from list 2 removed from list 1, and returns the created list.
(define (eat goos goos-to-eat)
  (cond
    [(empty? goos-to-eat) goos]
    [else
     (append (fresh-goos (random 3)) (eat (remove (first goos-to-eat) goos) (rest goos-to-eat)))]))

;; grow: snake goo -> snake. Adds segments to the snake based on special? of goo, and returns the grown snake.
(define (grow sn g)
  (define new-sn (snake (snake-dir sn)
                        (cons (step (snake-head sn) (snake-dir sn)) (snake-segs sn)) ; creating a new segment in front of the snake
                        (add1 (snake-eaten sn))))
  (when (goo-special? g)
    (set! new-sn (snake (snake-dir new-sn)
                        (cons (step (snake-head new-sn) (snake-dir new-sn)) (snake-segs new-sn)) ; creating another segment
                        (snake-eaten new-sn)))) ; maintaining the score
  new-sn) ; returning the new snake

;; slither: snake -> snake. Creates a new snake walking sn to the direction it is pointing.
(define (slither sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (snake (snake-dir sn)
         (cons (step head dir) (all-but-last (snake-segs sn))) ; appending the stepped head to the body without the last segment.
         (snake-eaten sn)))

;; all-but-last: list -> list. Returns the given list without its last element.
(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

;; step: posn string -> posn. Creates a new posn moving the given one to the given direction by 1.
(define (step p dir)
  (cond [(string=? dir "up")    (posn-move p 0 -1)]
        [(string=? dir "down")  (posn-move p 0  1)]
        [(string=? dir "left")  (posn-move p -1 0)]
        [(string=? dir "right") (posn-move p 1  0)]))

;; posn-move: posn int1 int2 -> posn. Creates a new posn adding dx to the x coordinate and dy to the y coordinate.
(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

;; age-goo: goo-list -> goo-list. Renews goos-list and then rots it.
(define (age-goo goos)
  (rot (renew goos)))

;; rot: goo-list -> goo-list. Applies decay to every goo in list.
(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

;; decay goo -> goo. Decreases by 1 the goo's expire and returns a new one with the new expire.
(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g)) (goo-special? g)))

;; renew: goo-list -> goo-list. Removes rotten or exceding goos, if any, from goo list and returns it.
;; For every rotten goo, it appends up to 3 new ones to the list.
(define (renew goos)
  (cond [(empty? goos) '()]
        [(> (length goos) MAX-GOOS) (rest goos)] ; ignoring exceding goos
        [(rotten? (first goos))
         (append (fresh-goos (random 3)) (renew (rest goos)))] ; adding fresh goos
        [else
         (cons (first goos) (renew (rest goos)))]))

;; rotten?: goo -> boolean. Returns true if the goo's expire time is over.
(define (rotten? g)
  (zero? (goo-expire g)))

;; fresh-goo: generates a goo at a random posn, with full expiration time and with 25% of chance of being special.
(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME (> 2 (random 4)))) ; The probability of the last expression returning true is 1/4 = 25%.

;; fresh-goos: int -> goo-list. Invokes fresh-goo n times appending the result to a list and then returning it.
(define (fresh-goos n)
  (cond
    [(zero? n) '()]
    [else (cons (fresh-goo) (fresh-goos (sub1 n)))]))

;; generate-obstacles: int -> obstacle-list. Invokes generate-obstacle n times appending the result to a list and then returning it.
(define (generate-obstacles n)
  (cond
    [(zero? n) '()]
    [else (append (generate-obstacle) (generate-obstacles (sub1 n)))]))

;; generate-obstacle: generates an obstacle at a random posn, with a random size up to MAX-OBSTACLE-SIZE.
(define (generate-obstacle)
  (define size (add1 (random (sub1 MAX-OBSTACLE-SIZE))))
  (define initial-posn (posn (random SIZE) (random SIZE)))
  (define (corners p) ; identifies if a posn is at the corners
    (or
     (and (> 10 (posn-x p)) (> 10 (posn-y p)))
     (and (> 10 (- SIZE-PX (posn-x p))) (> 10 (- SIZE-PX (posn-y p))))))
  (filter-not corners (remove-duplicates (grow-obstacle initial-posn size)))) ; removing obstacles in the corners where snakes spawn and duplicates

;; grow-obstacle: posn int -> posn-list. Creates 'size' adjacent obstacle blocks to the initial position.
(define (grow-obstacle initial-posn size)
  (cond
    [(zero? size) '()]
    [else 
     (define dir (list-ref '("up" "down" "left" "right") (random 4))) ; picking a random direction
     (define new-block (step initial-posn dir)) ; stepping to the picked direction
     (cons new-block (grow-obstacle new-block (sub1 size)))]))
  
;; --------- INPUT ----------

;; direct-snake: pit key -> pit. Calls world-change-dir on the pit if the key is processable, returns the current world otherwise.
(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w (dir? ke) (if (member ke (first PADS)) 0 1))] ; the 'if' signals the snake that world-change-dir should move.
        [else w]))

;; dir?: key -> string. Returns the direction that x represents or #f if it doesn't represent any.
(define (dir? x)
  (cond
    [(or (key=? x "up") (key=? x "w")) "up"]
    [(or (key=? x "left") (key=? x "a")) "left"]
    [(or (key=? x "down") (key=? x "s")) "down"]
    [(or (key=? x "right") (key=? x "d")) "right"]
    [else #f]))

;; world-change-dir: pit string int -> pit. Consumes a pit and creates a new one changing the direction of the snake represented by 'index' to 'd'.
(define (world-change-dir w d index)
  (define snakes (pit-snakes w))
  (define moving-snake (list-ref snakes index)) ; obtaining the represented snake
  (cond [(and (opposite-dir? (snake-dir moving-snake) d) ; making sure the snake isn't eating itself
              (cons? (rest (snake-segs moving-snake))))
         (stop-with w)]
         [else
          (pit
           (insert (snake-change-dir moving-snake d) index (remove moving-snake snakes))
           (pit-goos w)
           (pit-obstacles w))]))

;; opposite-dir?: string string -> boolean. Returns true if the strings represent opposite directions, and false otherwise.
(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))

;; -------- RENDERING --------

;; render-pit: pit -> image. Represents the given pit in an image.
(define (render-pit w)
  (define goos-scene (goo-list+scene (pit-goos w) MT-SC))
  (define obstacles+goos-scene (obstacle-list+scene (pit-obstacles w) goos-scene)) ; overlaying goos with obstacles
  (snakes+scene (pit-snakes w)
               obstacles+goos-scene)) ; overlaying goos and obstacles with snakes

;; snakes+scene: snake-list image -> image. Places the image of a snake over the scene with the previous snake, recursively.
(define (snakes+scene snakes scene)
  (cond
    [(empty? snakes) scene]
    [else
     (snakes+scene (rest snakes) (snake+scene (first snakes) scene))]))

;; snake+scene: snake image -> image. Places the image of snake over scene.
(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? "up" dir) HEAD-UP-IMG] ; this piece of code is useful only when there are different images for the head.
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "right" dir) HEAD-LEFT-IMG]
                   [(string=? "left" dir) HEAD-RIGHT-IMG])
             snake-body-scene))

;; img-list+scene: posn-list image image -> image. Places img at the given positions, over scene.
(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene (first posns)
                         img
                         (img-list+scene (rest posns) img scene))]))

;; img+scene: posn image image -> image. Places img at posn over scene.
(define (img+scene posn img scene)
  (place-image img
               (* SEG-SIZE (posn-x posn))
               (* SEG-SIZE (posn-y posn))
               scene))
  
;; goo-list+scene: goo-list image -> image. Creates an image of each goo and places all of them over scene.
(define (goo-list+scene goos scene)
  (define img (img-list+scene
               (map goo-loc (filter goo-special? goos)) ; This isn't actually teached until chapter 7, but it would be painful to write it in any other way.
               SPECIAL-GOO-IMG scene))
  (img-list+scene
   (map goo-loc (filter-not goo-special? goos))
   GOO-IMG img))

;; obstacle-list+scene: obstacle-list image -> image. Since an obstacle list is a list of posns, this is just a named call to img-list+scene.
(define (obstacle-list+scene obstacles scene)
  (img-list+scene obstacles
                  OBSTACLE-IMG
                  scene))

;; ----------- END GAME ----------

;; any-dead?: pit -> boolean. Returns true only if dead? returns true for any of the snakes.
(define (any-dead? w)
  (define snakes (pit-snakes w))
  (define obstacles (pit-obstacles w))
  (or (dead? (first snakes) obstacles) (dead? (second snakes) obstacles)))

;; dead?: snake obstacles -> boolean. Returns true only if the snake is colliding with either herselg, a wall or an obstacle.
(define (dead? s o)
  (or (self-colliding? s) (wall-colliding? s) (obstacle-colliding? s o)))

;; render-end: pit -> image. Places the ending game text over the current pit's image.
(define (render-end w)
  (define snakes (pit-snakes w))
  (overlay (text (string-append "Game Over!\n" (write-score snakes)) ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

;; write-score: snakes -> string. Generates a text with each snake's score.
(define (write-score snakes)
  (string-append
   "Player 1: " (number->string (snake-eaten (first snakes)))  "\n"
   "Player 2: " (number->string (snake-eaten (second snakes))) "\n"))

;; self-colliding?: snake -> boolean. Returns true only if the snake's head is part of its body.
(define (self-colliding? snake)
  (posn-member (snake-head snake) (snake-body snake)))

;; wall-colliding?: snake -> boolean. Returns true only if the snake's head is part of the pit's boundaries.
(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (> 0 x) (> x SIZE)
      (> 0 y) (> y SIZE)))

;; self-colliding?: snake obstacle -> boolean. Returns true only if the snake's head is part of any obstacle in the pit.
(define (obstacle-colliding? snake obstacles)
  (posn-member (snake-head snake) (flatten obstacles)))

;; ------------- AUXILIARY FUNCTIONS -------------

;; posn=?: posn posn -> boolean. Returns true only if the corresponding fields of the posns are equal.
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;; posn-member: posn list -> boolean. Returns true only if posn is posn=? to any element in the list.
(define (posn-member p lst)
  (cond
    [(empty? lst) #f]
    [else (if (posn=? p (first lst))
              #t
              (posn-member p (rest lst)))]))

;; snake-head: snake -> posn. Returns the posn of the first segment of the snake.
(define (snake-head sn)
  (first (snake-segs sn)))

;; snake-body: snake -> list. Returns the list of posns of the snake without the head.
(define (snake-body sn)
  (rest (snake-segs sn)))

;; snake-head: snake -> posn. Returns the posn of the last segment of the snake.
(define (snake-tail sn)
  (last (snake-segs sn)))

;; snake-change-dir: snake string -> snake. Consumes the snake and creates a similar one but with the direction d.
(define (snake-change-dir sn d)
  (snake d (snake-segs sn) (snake-eaten sn)))

;; insert: any int list -> list. Returns the list with elem in position pos, and everything from pos to the last element is moved one index to the right.
(define (insert elem pos lst)
  (cond
    [(zero? pos) (cons elem lst)]
    [else (cons (first lst) (insert elem (sub1 pos) (rest lst)))]))
