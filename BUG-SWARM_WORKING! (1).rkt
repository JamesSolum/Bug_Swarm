;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname BUG-SWARM_WORKING!) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
#|
Acknowledgments

By submitting this file with this header, I asserting that the file
contains the result of my own thought and work, with
exceptions as identified in these acknowledgements.  When citing help
given or received, I have described the nature of the help
and the exercise or function-name in question.

NAME: James Solum

ASSIGNMENT: SWARM PROJECT: Part 1

RECEIVED HELP FROM:

Katie Morhoff: worked together for everything
Kyle Hansen: helped and explained the velocity functions

GAVE HELP TO:
David: explained how to do some velocity functions.
Mark: explained some functions to him
Katie: worked together

|#


;;;;; Preliminaries and Definitions ;;;;;
;----------------------------------------

;; A bug is a structure:(make-bug p1 p2)
;; where p1 and p2 are posns representing the
;; bug's location and velocity, respectively.
(define-struct bug (loc vel))

;; a swarm is a [List-of bug].



;;;;; CONSTANTS and PARAMETERS ;;;;;
;-----------------------------------
; (Go Ahead and play with these)
(define WIDTH 500)
(define HEIGHT 500)
(define COLOR "teal")
(define SWARM-SIZE 15)
(define BUG-SIZE 4)
(define VEL-DETER 10)
(define N-RAD 50)
(define FOV pi)
(define SEPARATION-SCALE 10) 
(define COHESION-SCALE 3)
(define ALIGN-SCALE 3)
(define WALL-SCALE 5)
(define NOMINAL-SPEED 4)


;-----------(DO NOT CHANGE THESE)-------------

(define BUG-BODY (circle BUG-SIZE "solid" COLOR))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define HEAD-SCALE (+ 10 BUG-SIZE))
;; MAX-VEL is the number we substract from vel-deter to achieve both + and - vel values.
(define MAX-VEL (/ VEL-DETER 2))



;;;;;;;;;;;; TEMPLATES ;;;;;;;;;;;;
;----------------------------------

;; Templates for functions that process bugs:
#|
(define (f-for-bug b)
  (... (bug-loc b) ...)
 (... (bug-vel b) ...))
|#

;; Templates for functions that swarm processing:
#|
(define (f-for-swarm s)
  (cond [(empty?) ...]
        [(cons?)... (...(bug-loc (first s))...)
               ... (...(bug-vel (first s))...)
               ... (... (f-for-swarm (rest s))...)]))
|#

;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;
;-----------------------------------

;; posn* : operator posn posn -> posn
;; performs an operation on two posns to create a new posn
(define (posn* op p1 p2)
  (make-posn (op (posn-x p1) (posn-x p2)) (op (posn-y p1) (posn-y p2))))
(check-expect (posn* + (make-posn 1 2) (make-posn 3 4)) (make-posn 4 6))
(check-expect (posn* - (make-posn 3 4) (make-posn 1 2)) (make-posn 2 2))

;; posn+ : posn posn -> posn
;; adds two posns together
(define (posn+ p1 p2)
  (posn* + p1 p2))
(check-expect (posn+ (make-posn 1 2) (make-posn 3 4)) (make-posn 4 6))

;; posn- : posn posn -> posn
;; subtracts two posns together
(define (posn- p1 p2)
  (posn* - p1 p2))
(check-expect (posn- (make-posn 3 4) (make-posn 1 2)) (make-posn 2 2))

;; posn-mag : posn -> number
;; finds the magnitude of a posn
(define (posn-mag p1)
  (sqrt (+ (sqr (posn-x p1)) (sqr (posn-y p1)))))
(check-expect (posn-mag (make-posn 3 4)) 5)

;; posn-scale : number posn -> posn
;; scales a posn by a factor of n
;; posn-scale : posn number -> posn
;; scales a posn by a factor, n
(define (posn-scale n p)
  (make-posn (* n (posn-x p)) (* n (posn-y p))))
(check-expect (posn-scale 5 (make-posn 3 4) ) (make-posn 15 20))

;; decimal-modulo : Number Number -> Number
;; returns a modulo even when the numnbers have decimals.
(define (decimal-modulo x y)
  (- (modulo (round x) y) (- (round x) x)))


;-------- MORE UTILITIES --------
;(used especially in separation)

;; Find-unit-vector: posn -> posn
;; finds the unit vector
(define (FIND-UNIT-VECTOR p)
  (cond [(and (=  (posn-x p) 0) (= (posn-y p) 0)) (make-posn 0 0)]
        [else (posn-scale (/ 1 (posn-mag p)) p)]))

(check-expect (FIND-UNIT-VECTOR (make-posn 3 4)) (make-posn 3/5 4/5))

;; BUG-DISTANCE : bug bug -> number
;; finds the distance between two bugs
(define (BUG-DISTANCE b1 b2)
  (sqrt (+ (sqr (- (posn-x (bug-loc b1))
                   (posn-x (bug-loc b2))))
           (sqr (- (posn-y (bug-loc b1))
                   (posn-y (bug-loc b2)))))))

(check-expect (BUG-DISTANCE (make-bug (make-posn 0 0)
                                      (make-posn 0 0))
                            (make-bug (make-posn 3 4)
                                      (make-posn 0 0))) 5)

;; ANGLE-BTWN-VS: posn posn -> number
;; finds the angle between two vectors
(define (ANGLE-BTWN-VS p1 p2)
  (cond [(zero? (* (posn-mag p1) (posn-mag p2))) 0]  
        [else (acos (/ (+ (* (posn-x p1) (posn-x p2))
                          (* (posn-y p1) (posn-y p2)))
                       (* (posn-mag p1) (posn-mag p2))))]))

(check-expect (ANGLE-BTWN-VS (make-posn 3 4) (make-posn 6 8)) 0)

;; DIR-DIST: bug bug -> posn
;; finds the directional distance between two bugs
(define (DIR-DIST b1 b2)
  (posn- (bug-loc b2) (bug-loc b1)))
(check-expect (DIR-DIST (make-bug (make-posn 0 0)
                                      (make-posn 0 0))
                            (make-bug (make-posn 3 4)
                                      (make-posn 0 0))) (make-posn 3 4))

;; Neighbor?: bug bug -> boolean
;; determines if a bug is within the neighborhood radius and the FOV
(define (NEIGHBOR? b1 b2)
  (and (< 0 (BUG-DISTANCE b1 b2))
       (<= (BUG-DISTANCE b1 b2) N-RAD)
       (<= (- 0 FOV)
           (if (and (= (posn-x (FIND-UNIT-VECTOR (DIR-DIST b1 b2)))
                       (- 0 (posn-x (FIND-UNIT-VECTOR (bug-vel b1)))))
                    (= (posn-y (FIND-UNIT-VECTOR (DIR-DIST b1 b2)))
                       (- 0 (posn-y (FIND-UNIT-VECTOR (bug-vel b1))))))
                    ;; If the second bug is directly behind the other bug
               pi
               (ANGLE-BTWN-VS (DIR-DIST b1 b2)
                              (bug-vel b1)))
           FOV
           ;;in between -fov and fov.
           )))
(check-expect (NEIGHBOR? (make-bug (make-posn 1 1)
                                   (make-posn 0 1))
                         (make-bug (make-posn 3 4)
                                   (make-posn 0 1))) #t)

;; posn-average: [List-of posns] -> posn
;; finds the average from a list of posns
(define (posn-average lop)
  (cond [(empty? lop) (make-posn 0 0)]
        [else (posn-scale (/ 1 (length lop)) (foldr posn+ (make-posn 0 0) lop))]))

(check-expect (posn-average '()) (make-posn 0 0))
(check-expect (posn-average (list (make-posn 1 1) (make-posn 1 2))) (make-posn 1 1.5))




;;;;;;;;;;;;;;;;;;;;; VELOCITY VECTOR FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;
;--------------------------------------------------------------------

;; get-neighbors: bug swarm -> swarm
;; given the entire swarm, this function
;; returns a sub-swarm consisting of those bugs
;; within the given bug's neighborhood.
(define (get-neighbors b s)
  (filter (lambda (b1) (NEIGHBOR? b b1)) s))

(check-expect (get-neighbors TEST-BUG TEST-SWARM) TEST-NEIGHBORS)


;; get-separation-vector: bug swarm -> posn
;; Compute the steering vector that will tend to
;; prevent the bug from colliding with its neighbors
;; in the sub-swarm of neighbors.
(define (get-separation-vector b s)
  (foldl posn+ (make-posn 0 0) (map (lambda (b1)
                                      (posn-scale (sqrt (/ SEPARATION-SCALE (BUG-DISTANCE b b1)))
                                                  (FIND-UNIT-VECTOR (DIR-DIST b1 b))))
                                    (get-neighbors b s))))

(check-within (get-separation-vector (make-bug (make-posn 0 0) (make-posn 0 1))
                                     (list (make-bug (make-posn 1 1) (make-posn 0 1))
                                           (make-bug (make-posn 100 100) (make-posn 0 1))))
              (make-posn -1.88 -1.88) 0.1)


;; get-cohesion-vector: bug swarm -> posn
;; Compute the steering vector that points
;; from the given bug to the center-of-mass of its neighbors.
(define (get-cohesion-vector b s)
  (posn-scale COHESION-SCALE (FIND-UNIT-VECTOR
                              (posn- (posn-average
                                      (cons (bug-loc b)
                                            (map (lambda (b1) (bug-loc b1))
                                                 (get-neighbors b s))))
                                     (bug-loc b)))))
(check-within (get-cohesion-vector TEST-BUG TEST-SWARM) (make-posn 2.12 2.12) 0.1)


;; get-alignment-vector: bug swarm -> posn
;; Compute the steering vector that reflects the
;; difference between the average heading of the given
;; bug's neighbors and the bug's own heading. When this
;; vector is added to the given bug's current velocity,
;; it should produce the average velocity vector.
(define (get-alignment-vector b s)
  (posn-scale ALIGN-SCALE (FIND-UNIT-VECTOR
                              (posn-average
                               (cons (bug-vel b)
                                      (map (lambda (b1) (bug-vel b1))
                                           (get-neighbors b s)))))))

(check-expect (get-alignment-vector TEST-BUG TEST-SWARM) (make-posn 0 3))


;; get-edge-avoidance-vector: bug -> posn
;; Compute the avoidance vector for an edge
;; that is visible within the neighborhood.
(define (get-edge-avoidance-vector b)
  (posn-scale WALL-SCALE (get-separation-vector b  (WALLS b))))

(check-within (get-edge-avoidance-vector (make-bug (make-posn 1 1)
                                                   (make-posn -1 -1)))
              (make-posn 15.81 15.81) 0.1)


;; WALLS: b -> [list-of bugs]
;; gives walls that are represented by bugs.
(define (WALLS b)
  (list (make-bug (make-posn 0 (posn-y (bug-loc b))) (make-posn 0 0))
        (make-bug (make-posn (posn-x (bug-loc b)) 0) (make-posn 0 0))
        (make-bug (make-posn WIDTH (posn-y (bug-loc b))) (make-posn 0 0))
        (make-bug (make-posn (posn-x (bug-loc b)) HEIGHT) (make-posn 0 0))))
                         
;; get-nominal-speed-vector: bug -> posn
;; Have the bug speed up or slow down in
;; order to maintain its desired nominal speed.
(define (get-nominal-speed-vector b)
  (posn- (posn-scale NOMINAL-SPEED (FIND-UNIT-VECTOR (bug-vel b))) (bug-vel b)))

(check-expect (get-nominal-speed-vector TEST-BUG) (make-posn 0 3))
       


;;;;;;;;;;;;;;  SWARM  ;;;;;;;;;;;;;;;;;;
;----------------------------------------

;; make-swarm : N -> swarm
;; will make a swarm with an N number of bugs
(define (make-swarm n)
  (cond [(= n 0) '()]
        [else (cons (make-bug (make-posn (random WIDTH) (random HEIGHT))
                              (make-posn (- (random VEL-DETER) MAX-VEL)
                                         (- (random VEL-DETER) MAX-VEL)))
                    (make-swarm (sub1 n)))]))

(check-random (make-swarm 5) (cond [(= 5 0) '()]
        [else (cons (make-bug (make-posn (random WIDTH) (random HEIGHT))
                              (make-posn (- (random VEL-DETER) MAX-VEL)
                                         (- (random VEL-DETER) MAX-VEL)))
                    (make-swarm (sub1 5)))]))



;;;;;;;;;;;;;; DRAW BUG ;;;;;;;;;;;;;;;;;
;----------------------------------------

;; draw-bug : bug image -> image
;; given a bug and an image of the world, we will add
;; a rendered image of a given bug onto the world scene at
;; the position defined by the bugs position posn.
(define (draw-bug b img)
  (local [(define HEAD-VECT-CALC
            (HEAD-VECT b))
          (define LOC+VEL
            (posn+ (bug-loc b) HEAD-VECT-CALC))]
           (add-line (place-image BUG-BODY 
               (posn-x (bug-loc b))
               (posn-y (bug-loc b))
               img)
            (posn-x (bug-loc b))
            (posn-y (bug-loc b))
            (cond [(> 0 (posn-x LOC+VEL)) 0]
                  [(< WIDTH (posn-x LOC+VEL)) WIDTH]
                  [else (posn-x LOC+VEL)])
            (cond [(> 0 (posn-y LOC+VEL)) 0]
                  [(< HEIGHT (posn-y LOC+VEL)) HEIGHT]
                  [else (posn-y LOC+VEL)])
            COLOR)))

(check-expect (draw-bug (make-bug (make-posn (/ WIDTH 2) (/ HEIGHT 2)) (make-posn 1 1)) MTSCN)
             (add-line (place-image BUG-BODY
               (/ WIDTH 2) (/ HEIGHT 2)
               MTSCN)
            (posn-x (bug-loc (make-bug (make-posn (/ WIDTH 2) (/ HEIGHT 2)) (make-posn 1 1))))
            (posn-y (bug-loc (make-bug (make-posn (/ WIDTH 2) (/ HEIGHT 2)) (make-posn 1 1))))
            (posn-x (posn+ (bug-loc (make-bug (make-posn (/ WIDTH 2) (/ HEIGHT 2)) (make-posn 1 1)))
                           (HEAD-VECT (make-bug (make-posn (/ WIDTH 2) (/ HEIGHT 2)) (make-posn 1 1)))))
            (posn-y (posn+ (bug-loc (make-bug (make-posn (/ WIDTH 2) (/ HEIGHT 2)) (make-posn 1 1)))
                           (HEAD-VECT (make-bug (make-posn (/ WIDTH 2) (/ HEIGHT 2)) (make-posn 1 1)))))
            COLOR))

;--- HELPER ---
;; HEAD-VECT : bug -> posn
;; scales the magnitude of the velocity to a magnitude of HEAD-SCALE
(define (HEAD-VECT b)
  (cond[(zero? (posn-mag (bug-vel b))) (HEAD-VECT (make-bug (bug-loc b) (make-posn 1 0)))]
       [else (posn-scale (/ HEAD-SCALE (posn-mag (bug-vel b))) (bug-vel b))]))

(check-within (HEAD-VECT (make-bug (make-posn 1 1) (make-posn 1 1)))
              (make-posn (/ HEAD-SCALE (sqrt 2)) (/ HEAD-SCALE (sqrt 2)))
              0.001)



;;;;;;;;;;;;; UPDATE BUG ;;;;;;;;;;;;;;;;
;----------------------------------------

;; update-bug: bug -> bug
;; create a new bug that reflects
;; the movement of the given bug according
;; to the bug's current velocity.
(define (update-bug b s)
  (local [(define loc+vel
            (posn+ (bug-loc b)(bug-vel b)))
          (define VELOCITY-LIST
            (list (bug-vel b)
                  (get-separation-vector b s)
                  (get-cohesion-vector b s)
                  (get-alignment-vector b s)
                  (get-edge-avoidance-vector b)
                  (get-nominal-speed-vector b)))]
    
    (make-bug (make-posn (decimal-modulo (posn-x loc+vel) WIDTH)
                         (decimal-modulo (posn-y loc+vel) HEIGHT))
                     (cond [(= 0 (posn-mag (bug-vel b))) (make-posn 0 1)]
                           [else (foldl posn+ (make-posn 0 0) VELOCITY-LIST)]))))

(check-within (update-bug TEST-BUG TEST-SWARM) (make-bug (make-posn 250 251)
                                                         (make-posn 1.66 8.66)) 0.1)
(check-expect (update-bug (make-bug (make-posn 5 5)
                                    (make-posn 0 0)) (list (make-bug (make-posn 100 100)
                                                                     (make-posn 1 1))))
              (make-bug (make-posn 5 5) (make-posn 0 1)))


                
;;;;;;;;;;;;;; DRAW SWARM ;;;;;;;;;;;;;;;
;----------------------------------------

;; draw-swarm: swarm -> image
;; Draw each of the bugs in the given swarm on an initially empty scene.

(define (draw-swarm s)
  (cond [(empty? s) MTSCN]
        [(cons? s) (draw-bug (first s) (draw-swarm (rest s)))]))

(check-expect (draw-swarm (list (make-bug (make-posn 100 100)
                                          (make-posn 1 1))
                                (make-bug (make-posn 200 200)
                                          (make-posn -1 -1))))
              (draw-bug (make-bug (make-posn 100 100)
                                  (make-posn 1 1))
                        (draw-bug (make-bug (make-posn 200 200)
                                            (make-posn -1 -1))
                                  MTSCN)))


;;;;;;;;;;;;; UPDATE SWARM ;;;;;;;;;;;;;;                                                   
;----------------------------------------

;; update-swarm: swarm -> swarm
;; Given a swarm, update each bug in the swarm using update-bug
(define (update-swarm s)
  (cond [(empty? s) '()]
        [else (cons (update-bug (first s) (get-neighbors (first s) (rest s))) (update-swarm (rest s)))]))

(check-expect (update-swarm TEST-SWARM)(list
                                        (make-bug
                                         (make-posn 299 251)
                                         (make-posn 0 7))
                                        (make-bug
                                         (make-posn 250 300)
                                         (make-posn 0 7))
                                        (make-bug
                                         (make-posn 0 1)
                                         (make-posn 0 7))
                                        (make-bug
                                         (make-posn 0 1)
                                         (make-posn 0 7))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BIG BANG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------------------------------------
;; Making the Animation!

(big-bang (make-swarm SWARM-SIZE)
          (on-tick update-swarm)
          (to-draw draw-swarm))































;;;;;;;;;;;; MISCELLANEOUS and/or NO LONGER NEED ;;;;;;;;;;;
;-----------------------------------------------------------

;; This Bug and Swarm will be used for Check-expects 
(define TEST-BUG (make-bug (make-posn (/ WIDTH 2) (/ HEIGHT 2))
                           (make-posn 0 1)))
(define TEST-SWARM (list (make-bug (make-posn (+ (/ WIDTH 2) (sub1 N-RAD)) (/ HEIGHT 2))
                                   (make-posn 0 1))
                         (make-bug (make-posn (/ WIDTH 2) (+ (/ HEIGHT 2) (sub1 N-RAD)))
                                   (make-posn 0 1))
                         (make-bug (make-posn 0 0)
                                   (make-posn 0 1))
                         (make-bug (make-posn WIDTH HEIGHT)
                                   (make-posn 0 1))))
(define TEST-NEIGHBORS (list (make-bug (make-posn (+ (/ WIDTH 2) (sub1 N-RAD)) (/ HEIGHT 2))
                                       (make-posn 0 1))
                             (make-bug (make-posn (/ WIDTH 2) (+ (/ HEIGHT 2) (sub1 N-RAD)))
                                       (make-posn 0 1))))


#|  OLD get-edge-avoidance-vector
(define (get-edge-avoidance-vector b)
  (local [(define FIND-WALLS
            (get-neighbors b (WALLS b)))]
    (foldl posn+ (make-posn 0 0)
           (map (lambda (wall) (posn-scale
                                (sqr (/ WALL-SCALE (BUG-DISTANCE b wall)))
                                (FIND-UNIT-VECTOR (DIR-DIST wall b))))
                FIND-WALLS))))
|#
