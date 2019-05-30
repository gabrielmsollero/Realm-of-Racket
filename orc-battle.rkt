;;; This is my version of the second game in 'Realm of Racket', by Matthias Felleisen, with all three challenges included. Much of the code could be reduced with
;;; macros (the stats selectors, for instance), but I didn't learn about them in Racket yet. I'd prefer to select the fields in the struct with something like
;;; (get-stat player field), and this would affect interval+ and many other parts of the code. I'll just leave it for when I have experience with macros. Also,
;;; with this tool it will be possible to generalize stat manipulation so that any new stats can be created and affected within the game easily.

#lang racket
(require 2htdp/image 2htdp/universe)

;; --------- STRUCTS AND CONSTANTS -------------

(struct orc-world (player lom attack# target) #:mutable #:transparent) ; lom stands for 'list of monsters'.
(struct player (health agility strength armor) #:mutable #:transparent)
(struct monster (image [health #:mutable]) #:transparent) ; image and health are common for all monsters.
(struct orc monster (club) #:transparent)
(struct hydra monster () #:transparent)
(struct slime monster (sliminess) #:transparent)
(struct brigand monster () #:transparent)

;; Player constants:
(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)
(define MAX-ARMOR 15)
(define ATTACKS# 10)
(define HEALING 10)
(define STAB-DAMAGE 5)
(define FLAIL-DAMAGE 5)

;; Monsters constants:
(define MONSTER# 9)
(define MONSTER-HEALTH0 16)
(define CLUB-STRENGTH 5)
(define ORC-STRENGTH-DAMAGE 3)
(define SLIMINESS 5)
(define SLIME-HEALTH-DAMAGE 3)
(define BRIGAND-HEALTH-DAMAGE 1)
(define BRIGAND-STRENGTH-DAMAGE 1)
(define BRIGAND-AGILITY-DAMAGE 1)

;; Display constants:
(define V-SPACER (rectangle 0 10 "solid" "white"))
(define H-SPACER (rectangle 10 0 "solid" "white"))
(define PER-ROW 3)

;; Colors:
(define ATTACK-COLOR 'black)
(define MESSAGE-COLOR 'black)
(define STRENGTH-COLOR 'blue)
(define AGILITY-COLOR 'green)
(define HEALTH-COLOR 'red)
(define ARMOR-COLOR 'gray)
(define MONSTER-COLOR 'green)

;; Message constants:
(define MESSAGES-SIZE 10)
(define INSTRUCTION-TEXT-SIZE 10)
(define LOSE "You Lose!")
(define WIN "You Win!")
(define DEAD-TEXT (text "You're dead!" MESSAGES-SIZE MESSAGE-COLOR))
(define REMAINING "Remaining attacks: ")
(define INSTRUCTION-TEXT (text "Select using arrows\nS: Stab | F: Flail | H: Heal" INSTRUCTION-TEXT-SIZE MESSAGE-COLOR))

;; Bars:
(define HEALTH-BAR-HEIGHT 10)
(define HEALTH-BAR-WIDTH 30)
(define HEALTH-SIZE 10)

;; Images: I defined them as texts so that they can be run by anyone.
(define PLAYER-IMG (text "PLAYER" 10 'black))
(define ORC-IMG (text "ORC" 10 'black))
(define SLIME-IMG (text "SLIME" 10 'black))
(define HYDRA-IMG (text "HYDRA" 10 'black))
(define BRIGAND-IMG (text "BRIGAND" 10 'black))
(define TARGET-IMG (square 20 'outline 'red))

;; player-update! : procedure procedure number -> procedure. Returns a function that changes a specific element of the player struct.
(define (player-update! setter selector mx)
  (lambda (player delta)
    (setter player (interval+ (selector player) delta mx))))

(define player-health+
  (player-update! set-player-health! player-health MAX-HEALTH))

(define player-agility+
  (player-update! set-player-agility! player-agility MAX-AGILITY))

(define player-strength+
  (player-update! set-player-strength! player-strength MAX-STRENGTH))

(define player-armor+
  (player-update! set-player-armor! player-armor MAX-ARMOR))

;; ------------- MAIN FUNCTIONS -------------

;; start : Starts the game.
(define (start)
  (big-bang (initialize-orc-world)
	    (on-key player-acts-on-monsters)
	    (to-draw render-orc-battle)
	    (stop-when end-of-orc-battle? render-the-end)))

;; initialize-orc-world : -> orc-world. Creates the first state of the game.
(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))

;; end-of-orc-battle? : orc-world -> boolean. Returns victory or defeat when the battle ends.
(define (end-of-orc-battle? w)
  (or (win? w) (lose? w)))

;; render-orc-battle : orc-world -> image. Displays the orc-world as an image.
(define (render-orc-battle w)
  (render-orc-world w (orc-world-target w) (instructions w)))

;; render-the-end: orc-world -> image. Displays the orc-world as an image with an ending message.
(define (render-the-end w)
  (render-orc-world w #f (message (if (lose? w) LOSE WIN))))

;; player-acts-on-monsters : orc-world key -> orc-world. Handles keyboard input.
(define (player-acts-on-monsters w k)
  (cond
   [(zero? (orc-world-attack# w)) (void)]
   [(key=? "s" k) (stab w)]
   [(key=? "h" k) (recover w 'health)]
   [(key=? "p" k) (recover w 'strength)]
   [(key=? "a" k) (recover w 'agility)]
   [(key=? "f" k) (flail w)]
   [(key=? "e" k) (end-turn w)]
   [(key=? "n" k) (initialize-orc-world)]
   [(key=? "right" k) (move-target w +1)]
   [(key=? "left" k) (move-target w -1)]
   [(key=? "down" k) (move-target w (+ PER-ROW))]
   [(key=? "up" k) (move-target w (- PER-ROW))])
  (give-monster-turn-if-attack#=0 w)
  w)

;; ------------- INITIALIZATION -------------

;; initialize-player : -> player. Creates a player with max stats.
(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH MAX-ARMOR))

;; random-number-of-attacks : player -> number. Returns a pseudo-random number of attacks.
(define (random-number-of-attacks p)
  (random-quotient (player-agility p) ATTACKS#))

;; initialize-monsters : -> list. Creates a random list of monsters.
(define (initialize-monsters)
  (build-list
   MONSTER#
   (lambda (_) ; _ is usually an ignored parameter
     (define health (random+ MONSTER-HEALTH0))
     (case (random 4)
       [(0) (orc ORC-IMG health (random+ CLUB-STRENGTH))]
       [(1) (hydra HYDRA-IMG health)]
       [(2) (slime SLIME-IMG health (random+ SLIMINESS))]
       [(3) (brigand BRIGAND-IMG health)]))))

;; ----------- RENDERING -------------

;; instructions : orc-world -> image. Builds the game's instruction text.
(define (instructions w)
  (define na (number->string (orc-world-attack# w)))
  (define ra (string-append REMAINING na))
  (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
  (above txt INSTRUCTION-TEXT))

(define (message str)
  (text str MESSAGES-SIZE MESSAGE-COLOR))

;; render-orc-world : orc-world int/boolean text -> image. Shows the current world in an image.
(define (render-orc-world w t additional-text)
  (define i-player (render-player (orc-world-player w)))
  (define i-monster (render-monsters (orc-world-lom w) t))
  (above V-SPACER
	 (beside H-SPACER
		 i-player
		 H-SPACER H-SPACER H-SPACER
		 (above i-monster
			V-SPACER V-SPACER V-SPACER
			additional-text)
		 H-SPACER)
	 V-SPACER))

;; render-player : player -> image. Generates an image for a player.
(define (render-player p)
  (define s (player-strength p))
  (define a (player-agility p))
  (define h (player-health p))
  (define r (player-armor p))
  (above/align
   "left"
   (status-bar s MAX-STRENGTH STRENGTH-COLOR "STRENGTH")
   V-SPACER
   (status-bar a MAX-AGILITY AGILITY-COLOR "AGILITY")
   V-SPACER
   (status-bar h MAX-HEALTH HEALTH-COLOR "HEALTH")
   V-SPACER
   (status-bar r MAX-ARMOR ARMOR-COLOR "ARMOR")
   V-SPACER V-SPACER V-SPACER
   PLAYER-IMG))

;; status-bar : number number color string -> image. Generates the image of a vertical bar with a label.
(define (status-bar v-current v-max color label)
  (define w (* (/ v-current v-max) HEALTH-BAR-WIDTH)) ; obtaining the width proportionally to the current value
  (define f (rectangle w HEALTH-BAR-HEIGHT 'solid color))
  (define b (rectangle HEALTH-BAR-WIDTH HEALTH-BAR-HEIGHT 'outline 'black))
  (define bar (overlay/align "left" "top" f b))
  (beside bar H-SPACER (text label HEALTH-SIZE color)))

;; render-monsters : list int/boolean -> image. Generates an image of all the monsters marking the current target, if any.
(define (render-monsters lom with-target)
  (define target
    (if (number? with-target)
	(list-ref lom with-target)
	'silly)) ; this could be any symbol that doesn't represent a monster.

  (define (render-one-monster m)
    (define image
      (if (eq? m target)
	  (overlay TARGET-IMG (monster-image m))
	  (monster-image m)))
    (define health (monster-health m))
    (define health-bar
      (if (zero? health)
	  (overlay DEAD-TEXT (status-bar 0 1 'white "")) ; we include the status bar to maintain the size.
	  (status-bar health MONSTER-HEALTH0 MONSTER-COLOR "")))
    (above health-bar image))
  (arrange (map render-one-monster lom)))

;; arrange : list -> image. Arranges the monster list in a matrix of images with PER-ROW width.
(define (arrange lom)
  (cond
   [(empty? lom) empty-image]
   [else
    (define r (apply beside (take lom PER-ROW))) ; take = butlast (common lisp)
    (above r (arrange (drop lom PER-ROW)))])) ; drop = nthcdr (common lisp)

;; ---------------- END GAME ----------------

;; win? : orc-world -> boolean. Verifies if all monsters are dead.
(define (win? w)
  (all-dead? (orc-world-lom w)))

;; lose? : orc-world -> boolean. Verifies if the player is dead.
(define (lose? w)
  (player-dead? (orc-world-player w)))

;; player-dead? : player -> boolean. Verifies if any attribute from player is zero.
(define (player-dead? p)
  (ormap zero? (struct->list p)))

;; all-dead? : list -> boolean. Verifies if all monsters in a list are dead.
(define (all-dead? lom)
  (not (ormap monster-alive? lom)))

;; monster-alive? : monster -> boolean. Verifies if a monster's health is 0.
(define (monster-alive? m)
  (positive? (monster-health m)))

;; ----------------- ACTIONS -----------------

;; end-turn : orc-world -> void. Ends the player's turn to attack.
(define (end-turn w)
  (set-orc-world-attack#! w 0))

;; recover : orc-world -> void. Consumes one attack and increases one of the player's stats.
(define (recover w stat)
  (decrease-attack# w)
  (case stat ; this could've also been reduced with macros.
    [(health)   (player-health+ (orc-world-player w) HEALING)]
    [(strength) (player-strength+ (orc-world-player w) HEALING)]
    [(agility)  (player-agility+ (orc-world-player w) HEALING)]))

;; stab : orc-world -> void. Consumes one attack and damages one monster.
(define (stab w)
  (decrease-attack# w)
  (define target
    (list-ref (orc-world-lom w) (orc-world-target w)))
  (define damage
    (random-quotient (player-strength (orc-world-player w)) STAB-DAMAGE))
  (damage-monster target damage))

;; damage-monster : number number -> void. Decreases the target's life.
(define (damage-monster target damage)
  (set-monster-health! target (interval+ (monster-health target) (- damage)
					 (monster-health target))))

;; flail : w -> void. Decreases the life of some monsters based on the player's strength.
(define (flail w)
  (decrease-attack# w)
  (define target (current-target w))
  (define alive (filter monster-alive? (orc-world-lom w)))
  (define pick# ; how many monsters will be damaged based on player's strength. Maximum is all monsters alive.
    (min
     (random-quotient (player-strength (orc-world-player w))
		      FLAIL-DAMAGE)
     (length alive)))
  (define getem (cons target (take alive pick#))) ; target appears twice in the list so that it receives double damage.
  (for-each (lambda (m) (damage-monster m 1)) getem))

;; move-target : orc-world number -> void. Moves the target by delta.
(define (move-target w delta)
  (define new (+ (orc-world-target w) delta))
  (set-orc-world-target! w (modulo new MONSTER#))) ; this ensures that the target returns to the first monster after the last one.

;; --------------- MONSTERS -----------------

;; give-monster-turn-if-attack#=0 : orc-world -> void. Represents the monsters' attack turn in the game.
(define (give-monster-turn-if-attack#=0 w)
  (when (zero? (orc-world-attack# w))
    (define player (orc-world-player w))
    (all-monsters-attack-player player (orc-world-lom w))
    (set-orc-world-attack#! w (random-number-of-attacks player))))

;; all-monsters-attack-player : player list -> void. Gives every alive monster its turn to attack the player and defines the attack of each monster.
(define (all-monsters-attack-player player lom)
  (define (one-monster-attacks-player m)
    (cond ; defining the attacks based on the monsters.
     [(orc? m)
      (split-damage (random- (orc-club m)) player)
      (player-strength+ player ORC-STRENGTH-DAMAGE)]
     [(hydra? m)
      (split-damage (random- (monster-health m)) player)]
     [(slime? m)
      (player-health+ player SLIME-HEALTH-DAMAGE)
      (player-agility+ player
		       (random- (slime-sliminess m)))]
     [(brigand? m)
      (case (random 3)
	[(0) (player-health+ player BRIGAND-HEALTH-DAMAGE)] ; ignores the player's armor
	[(1) (player-agility+ player BRIGAND-AGILITY-DAMAGE)]
	[(2) (player-strength+ player BRIGAND-STRENGTH-DAMAGE)])]))
  (define live-monsters (filter monster-alive? lom))
  (for-each one-monster-attacks-player live-monsters))

;; split-damage : number player -> void. Splits the damage to be partly absorbed by the armor and partly inflicted to player's health. Greater armor means greater absortion.
(define (split-damage dmg player)
  (define absortion (/ (player-armor player) MAX-ARMOR))
  (player-armor+ player (floor (* absortion dmg)))
  (player-health+ player (ceiling (* (- 1 absortion) dmg)))) ; floor and ceiling make sure that both damages are integers.

;; --------------- AUXILIARY ----------------

;; random-quotient : number number -> number. Returns a pseudo-random number between 1 and (quotient x y).
(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div) 0 (random+ (add1 div))))

;; random+ : number -> number. Returns a pseudo-random positive number up to n.
(define (random+ n)
  (add1 (random n)))

;; random- : number -> number. Negative version of random+.
(define (random- n)
  (- (add1 (random n))))

;; interval+ : number number number -> number. Sums n and delta up to mx.
(define (interval+ n delta mx)
  (max 0 (min (+ n delta) mx)))

;; decrease-attack# : orc-world -> void. Decreases the number of attacks left.
(define (decrease-attack# w)
  (set-orc-world-attack#! w (sub1 (orc-world-attack# w))))

;; current-target : orc-world -> monster. Returns the monster indicated by target, which is a number.
(define (current-target w)
  (list-ref (orc-world-lom w) (orc-world-target w)))

;; struct->list : struct -> list. Converts a struct's fields to a list. Documentation says this function already exists but i keep getting undefined identifier error. The struct must be transparent.
(define (struct->list s)
  (rest (vector->list (struct->vector s))))
