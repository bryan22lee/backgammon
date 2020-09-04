#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)
(require (only-in typed/racket/gui/base put-file get-file))

;; Backgammon Game Project by Chanik Bryan Lee

;; defining the Player type, consisting of symbols representing Black and White
(define-type Player (U 'Black 'White))

;; defining the struct OccupiedPoint, representing an occupied point.
;; It suffices to represent the player/checker color and the count of checkers
;; on the occupied point to represent OccupiedPoint (since the order of
;; checkers per occupied point does not matter).
(define-struct OccupiedPoint
  ([color : Player]
   [count : Integer]))

;; defining the Point type, which is the union of an OccupiedPoint (struct)
;; and an empty point (represented by the symbol 'EmptyPoint).
(define-type Point (U OccupiedPoint 'EmptyPoint))

;; defining the struct for describing the state of the game and board, called
;; Board. Has a list of point with type (Listof Point) describing the state
;; of each of the 24 points on the board. Has respective black-bar and
;; white-bar values to describe the states (number of checkers) on each
;; respective bar (order of the checkers do not matter here).
;; Finally, black-off and white-off, respectively for each color, represent
;; the number of checkers that have been borne off for that respective color.
(define-struct Board
  ([points : (Listof Point)] ;; should have 24 points
   [black-bar : Integer]
   [white-bar : Integer]
   [black-off : Integer]
   [white-off : Integer]))

;; defining the struct to represent the desired Style of the game.
;; For flexibility and the allowance for the appearance to be customized,
;; the user can select checker-radius, spacing between the points, black-checker
;; and white-checker (to construct images for the respective checkers),
;; dark-point and light-point (to construct images for the respective points),
;; background (to construct image for the background of the board), and label
;; (to construct image labels for certain checkers as needed).
;; This allows many aspects of the appearance of the game to be customized
;; as desired (flexibility).
(define-struct Style
  ([checker-radius : Integer]
   [spacing : Integer]
   [black-checker : (Integer -> Image)]
   [white-checker : (Integer -> Image)]
   [dark-point : (Integer Boolean -> Image)]
   [light-point : (Integer Boolean -> Image)]
   [background : (Integer Integer -> Image)]
   [label : (String -> Image)]
   [black-die : (Integer Integer -> Image)]
   [white-die : (Integer Integer -> Image)]))

(: labelfunc : String -> Image)
;; sample label function for my Style struct that takes in a String and
;; returns an Image representation of that string (to be overlayed over a
;; checker as a label). I chose the color red to contrast both black and
;; white checkers.
;; For testing purposes
(define (labelfunc str)
  (text str 15 "red"))
(labelfunc "20") ;; eyeball test
(labelfunc "13") ;; eyeball test
(labelfunc "6") ;; eyeball test

;; sampleboard1 for later testing (an input parameter for draw-board)
(define sampleboard1 : Board
  (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         (OccupiedPoint 'Black 5) (OccupiedPoint 'Black 5)
         (OccupiedPoint 'Black 5) (OccupiedPoint 'White 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)) 0 0 0 0))
;; sampleboard2 for later testing (an input parameter for draw-board)
(define sampleboard2 : Board
  (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 2)
         (OccupiedPoint 'Black 5)
         (OccupiedPoint 'Black 5) (OccupiedPoint 'White 2)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 3 3 0 0))
;; sampleboard3 for later testing (an input parameter for draw-board)
(define sampleboard3 : Board
  (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint
         (OccupiedPoint 'Black 5) (OccupiedPoint 'Black 5)
         (OccupiedPoint 'Black 2) (OccupiedPoint 'White 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 2)
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 7 7 6 6))
;; extrasampleboard for later testing (an input parameter for draw-board)
(define extrasampleboard : Board
  (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint
         (OccupiedPoint 'Black 5) (OccupiedPoint 'Black 5)
         (OccupiedPoint 'Black 2) (OccupiedPoint 'White 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 2)
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 2 3 3 4))
;; extrasampleboard for later testing (an input parameter for draw-board)
(define emptysampleboard : Board
  (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 0 0))

(: black-checker1 : (Integer -> Image))
;; Helper function for samplestyle1 that creates black-checker image
;; from Integer input (specifies radius of the round checkers).
(define (black-checker1 i)
  (circle i "solid" "black"))
(black-checker1 5) ;; eye-ball tested and it works
(: white-checker1 : (Integer -> Image))
;; Helper function for samplestyle1 that creates white-checker image
;; from Integer input (specifies radius of the round checkers).
(define (white-checker1 i)
  (overlay
   (circle i "outline" "black")
   (circle i "solid" "white")))
(white-checker1 5) ;; eye-ball tested and it works

;; Style constructor function that constructs a sample Style struct (for tests)
;; based on input parameter checker-radius

(: dark-point : (Integer Boolean -> Image))
;; Sample dark-point function that takes in an integer (function of the
;; checker radius) and a Boolean specifying the orientation of the triangle
;; (#t is up). Returns an image of the desired dark-point with the correct
;; orientation. Integer input here is the inputted checker radius
;; from earlier.
(define (dark-point i b) ;; where i is checker radius
  (if b (isosceles-triangle (sqrt (* 101 (* i i)))
                            (* (/ 180 pi) (* 2
                                             (asin (/ i (sqrt
                                                         (* 101 (* i i)))))))
                            "solid"
                            "dimgray")
      (isosceles-triangle (sqrt (* 101 (* i i)))
                          (- 360 (* (/ 180 pi) (* 2
                                                  (asin (/ i (sqrt
                                                              (* 101
                                                                 (* i i))))))))
                          "solid"
                          "dimgray")))
(dark-point 5 #t) ;; eyeball-tested and it works
(dark-point 3 #f) ;; eyeball-tested and it works

(: light-point : (Integer Boolean -> Image))
;; Sample light-point function that takes in an integer (function of the
;; checker radius) and a Boolean specifying the orientation of the triangle
;; (#t is up). Returns an image of the desired light-point with the correct
;; orientation. Integer input here is the inputted checker radius
;; from earlier.
(define (light-point i b) ;; where i is checker radius
  (if b (isosceles-triangle (sqrt (* 101 (* i i)))
                            (* (/ 180 pi) (* 2
                                             (asin (/ i (sqrt
                                                         (* 101 (* i i)))))))
                            "solid"
                            "lightgray")
      (isosceles-triangle (sqrt (* 101 (* i i)))
                          (- 360 (* (/ 180 pi) (* 2
                                                  (asin (/ i (sqrt
                                                              (* 101
                                                                 (* i i))))))))
                          "solid"
                          "lightgray")))
(light-point 5 #t) ;; eyeball-tested and it works
(light-point 3 #f) ;; eyeball-tested and it works

(: coolthing1 : Integer -> Image)
;; decoration-like element to add to my board (takes in check-radius to scale)
(define (coolthing1 i)
  (overlay
   (rotate 45 (square (exact-round (* (/ 3 2) i)) "solid" "lightgray"))
   (rotate 45 (square (exact-round (* 2 i)) "solid" "dimgray"))
   (rotate 45 (square (exact-round (* 3 i)) "outline" "black"))))
(coolthing1 5) ;; test, it worked
(: coolthing2 : Integer -> Image)
;; decoration-like element to add to my board (takes in check-radius to scale)
(define (coolthing2 i)
  (overlay
   (rotate 45 (square (exact-round (* 3 i)) "outline" "black"))
   (rotate 45 (square (exact-round (* (/ 3 2) i)) "solid" "dimgray"))
   (rotate 45 (square (* 3 i) "solid" "lightgray"))))
(coolthing2 5) ;; test, it worked

(: board-right : Integer Integer -> Image)
;; board-right takes in checker-radius and spacing values to return an
;; Image for the right half of the board (with a unique cool-looking decoration
;; at the center). Located on the right side of the center bar.
(define (board-right c-rad sp)
  (overlay (coolthing1 c-rad)
           (rectangle (+ (* c-rad 12) (* sp 5))
                      (* 26 c-rad) "solid" "white")))
(board-right 5 2) ;;sample for testing - it worked.

(: board-left : Integer Integer -> Image)
;; board-left takes in checker-radius and spacing values to return an
;; Image for the left half of the board (with a unique cool-looking center
;; decoration). Located on the right side of the center bar.
(define (board-left c-rad sp)
  (overlay (coolthing2 c-rad)
           (rectangle (+ (* c-rad 12) (* sp 5))
                      (* 26 c-rad) "solid" "white")))
(board-left 5 2) ;;sample for testing - it worked.

(: draw-die-black : Integer Integer -> Image)
;; function for drawing image of black die that takes in two integers,
;; the first being checker radius and the second is the number to show
;; on the die between one and six, inclusive
;; zero indicates empty-image (for game start)
(define (draw-die-black c-rad num)
  (local
    {(define pip : Image (circle (* (/ 1 6) c-rad) "solid" "white"))}
    (cond
      [(or (< num -1) (> num 6))
       (error "second Integer input should be between -1 and 6, inclusive")]
      [(= num 0) empty-image]
      [(= num 1) (overlay pip (square (* 2 c-rad) "solid" "black"))]
      [(= num 2) (overlay
                  (rotate 45 (beside pip (square c-rad "solid" "black") pip))
                          (square (* 2 c-rad) "solid" "black"))]
      [(= num 3) (overlay (rotate 45
                          (beside pip (square (/ c-rad 3) "solid" "black") pip
                                  (square (/ c-rad 3) "solid" "black") pip))
                        (square (* 2 c-rad) "solid" "black"))]
      [(= num 4) (overlay (above
                           (beside pip (square (/ c-rad 3) "solid" "black") pip)
                             (square (/ c-rad 3) "solid" "black")
                          (beside pip (square (/ c-rad 3) "solid" "black") pip))
                        (square (* 2 c-rad) "solid" "black"))]
      [(= num 5) (overlay (above
                           (beside pip (square (/ c-rad 3) "solid" "black") pip)
                           (square (/ c-rad 9) "solid" "black")
                           pip
                           (square (/ c-rad 9) "solid" "black")
                          (beside pip (square (/ c-rad 3) "solid" "black") pip))
                        (square (* 2 c-rad) "solid" "black"))]
      [else (overlay (above
                           (beside pip (square (/ c-rad 3) "solid" "black") pip)
                            (square (/ c-rad 9) "solid" "black")
                           (beside pip (square (/ c-rad 3) "solid" "black") pip)
                             (square (/ c-rad 9) "solid" "black")
                          (beside pip (square (/ c-rad 3) "solid" "black") pip))
                        (square (* 2 c-rad) "solid" "black"))])))
(check-error (draw-die-black 30 -2)
              "second Integer input should be between -1 and 6, inclusive")
(check-error (draw-die-black 30 7)
              "second Integer input should be between -1 and 6, inclusive")
(draw-die-black 30 1)
(draw-die-black 30 -1)
(draw-die-black 30 2)
(draw-die-black 30 3)
(draw-die-black 30 4)
(draw-die-black 30 5)
(draw-die-black 30 6)
(draw-die-black 30 0)

(: draw-die-white : Integer Integer -> Image)
;; function for drawing image of white die that takes in two integers,
;; the first being checker radius and the second is the number to show
;; on the die between one and six, inclusive
;; zero indicates empty-image (for starting the game)
(define (draw-die-white c-rad num)
  (local
    {(define pip : Image (circle (* (/ 1 6) c-rad) "solid" "black"))
     (define die-square : Image (overlay (square (* 2 c-rad) "outline" "black")
                                         (square (* 2 c-rad) "solid" "white")))}
    (cond
      [(or (< num -1) (> num 6))
       (error "second Integer input should be between -1 and 6, inclusive")]
      [(= num 0) empty-image]
      [(= num 1) (overlay pip die-square)]
      [(= num 2) (overlay
                  (rotate 45 (beside pip (square c-rad "solid" "white") pip))
                          die-square)]
      [(= num 3) (overlay (rotate 45
                         (beside pip (square (/ c-rad 3) "solid" "white") pip
                                 (square (/ c-rad 3) "solid" "white") pip))
                        die-square)]
      [(= num 4) (overlay (above
                           (beside pip (square (/ c-rad 3) "solid" "white") pip)
                             (square (/ c-rad 3) "solid" "white")
                          (beside pip (square (/ c-rad 3) "solid" "white") pip))
                        die-square)]
      [(= num 5) (overlay (above
                           (beside pip (square (/ c-rad 3) "solid" "white") pip)
                           (square (/ c-rad 9) "solid" "white")
                           pip
                           (square (/ c-rad 9) "solid" "white")
                          (beside pip (square (/ c-rad 3) "solid" "white") pip))
                        die-square)]
      [else (overlay (above
                           (beside pip (square (/ c-rad 3) "solid" "white") pip)
                            (square (/ c-rad 9) "solid" "white")
                           (beside pip (square (/ c-rad 3) "solid" "white") pip)
                             (square (/ c-rad 9) "solid" "white")
                          (beside pip (square (/ c-rad 3) "solid" "white") pip))
                        die-square)])))
(check-error (draw-die-white 30 -2)
              "second Integer input should be between -1 and 6, inclusive")
(check-error (draw-die-white 30 7)
              "second Integer input should be between -1 and 6, inclusive")
(draw-die-white 30 1)
(draw-die-white 30 -1)
(draw-die-white 30 2)
(draw-die-white 30 3)
(draw-die-white 30 4)
(draw-die-white 30 5)
(draw-die-white 30 6)

(: state-point-up-dark : Point Style -> Image)
;; Updates the state of a point (function from Integer and Boolean to Image)
;; (whether it's empty or has checkers on it)
;; and returns the updated Image of that point (with relevant checkers of the
;; correct color) based on the Point that is inputted as well.
;; state-point-up is only for points pointing up (i.e. Boolean for up is #t).
;; Also takes in a Style to specify desired checker specifications/style/colors.
;; Further, this is ONLY for dark points only.
(define (state-point-up-dark p s)
  (match* (p s)
    [('EmptyPoint (Style c-rad _ _ _ dark-point _ _ _ _ _))
     (dark-point c-rad #t)]
    [((OccupiedPoint color count) (Style c-rad _ bc wc dark-point _ _ lab _ _))
     (cond
       [(<= count 0) (error "count should be >= 1, unless 'EmptyPoint")]
       [(and (symbol=? color 'Black) (= count 1))
        (overlay/align "middle" "bottom" (bc c-rad) (dark-point c-rad #t))]
       [(and (symbol=? color 'Black) (= count 2))
        (overlay/align "middle" "bottom" (above (bc c-rad) (bc c-rad))
                                               (dark-point c-rad #t))]
       [(and (symbol=? color 'Black) (= count 3))
        (overlay/align "middle" "bottom" (above (bc c-rad) (bc c-rad)
                                                (bc c-rad))
                                               (dark-point c-rad #t))]
       [(and (symbol=? color 'Black) (= count 4))
        (overlay/align "middle" "bottom" (above (bc c-rad) (bc c-rad)
                                                (bc c-rad) (bc c-rad))
                                               (dark-point c-rad #t))]
       [(and (symbol=? color 'Black) (= count 5))
        (overlay/align "middle" "bottom" (above (bc c-rad) (bc c-rad) (bc c-rad)
                                                (bc c-rad) (bc c-rad))
                                               (dark-point c-rad #t))]
       [(and (symbol=? color 'Black) (> count 5))
        (overlay/align "middle" "bottom"
                       (above
                        (overlay (lab (number->string count))
                                 (bc c-rad))
                        (bc c-rad) (bc c-rad) (bc c-rad) (bc c-rad))
                       (dark-point c-rad #t))]
       [(and (symbol=? color 'White) (> count 5))
        (overlay/align "middle" "bottom"
                       (above
                        (overlay (lab (number->string count))
                                 (wc c-rad))
                        (wc c-rad) (wc c-rad) (wc c-rad) (wc c-rad))
                       (dark-point c-rad #t))]
       [(and (symbol=? color 'White) (= count 5))
        (overlay/align "middle" "bottom" (above (wc c-rad) (wc c-rad) (wc c-rad)
                                                (wc c-rad) (wc c-rad))
                                               (dark-point c-rad #t))]
       [(and (symbol=? color 'White) (= count 4))
        (overlay/align "middle" "bottom" (above (wc c-rad) (wc c-rad)
                                                (wc c-rad) (wc c-rad))
                                               (dark-point c-rad #t))]
       [(and (symbol=? color 'White) (= count 3))
        (overlay/align "middle" "bottom" (above (wc c-rad)
                                                (wc c-rad) (wc c-rad))
                                               (dark-point c-rad #t))]
       [(and (symbol=? color 'White) (= count 2))
        (overlay/align "middle" "bottom" (above (wc c-rad) (wc c-rad))
                       (dark-point c-rad #t))]
       [else
        (overlay/align "middle" "bottom" (wc c-rad) (dark-point c-rad #t))])]))
(check-error (state-point-up-dark (OccupiedPoint 'Black -5)
                                  (Style 5 2 black-checker1 white-checker1
                                         dark-point light-point board-left
                                         labelfunc
                                         draw-die-black draw-die-white))
             "count should be >= 1, unless 'EmptyPoint")
(state-point-up-dark (OccupiedPoint 'White 3)
                     (Style 5 2 black-checker1 white-checker1
                            dark-point light-point board-left labelfunc
                            draw-die-black draw-die-white))
(state-point-up-dark (OccupiedPoint 'White 11)
                     (Style 10 2 black-checker1 white-checker1
                            dark-point light-point board-left labelfunc
                            draw-die-black draw-die-white))
(state-point-up-dark 'EmptyPoint
                      (Style 5 2 black-checker1 white-checker1 dark-point
                             light-point board-left labelfunc
                             draw-die-black draw-die-white))

(: state-point-up-light : Point Style -> Image)
;; Same as state-point-up-dark, but for light points instead.
(define (state-point-up-light p s)
  (match* (p s)
    [('EmptyPoint (Style c-rad _ _ _ _ light-point _ _ _ _))
     (light-point c-rad #t)]
    [((OccupiedPoint color count) (Style c-rad _ bc wc _ light-point _ lab _ _))
     (cond
       [(<= count 0) (error "count should be >= 1, unless 'EmptyPoint")]
       [(and (symbol=? color 'Black) (= count 1))
        (overlay/align "middle" "bottom" (bc c-rad) (light-point c-rad #t))]
       [(and (symbol=? color 'Black) (= count 2))
        (overlay/align "middle" "bottom" (above (bc c-rad) (bc c-rad))
                                               (light-point c-rad #t))]
       [(and (symbol=? color 'Black) (= count 3))
        (overlay/align "middle" "bottom" (above (bc c-rad) (bc c-rad)
                                                (bc c-rad))
                                               (light-point c-rad #t))]
       [(and (symbol=? color 'Black) (= count 4))
        (overlay/align "middle" "bottom" (above (bc c-rad) (bc c-rad)
                                                (bc c-rad) (bc c-rad))
                                               (light-point c-rad #t))]
       [(and (symbol=? color 'Black) (= count 5))
        (overlay/align "middle" "bottom" (above (bc c-rad) (bc c-rad) (bc c-rad)
                                                (bc c-rad) (bc c-rad))
                                               (light-point c-rad #t))]
       [(and (symbol=? color 'Black) (> count 5))
        (overlay/align "middle" "bottom"
                       (above
                        (overlay (lab (number->string count))
                                 (bc c-rad))
                        (bc c-rad) (bc c-rad) (bc c-rad) (bc c-rad))
                       (light-point c-rad #t))]
       [(and (symbol=? color 'White) (> count 5))
        (overlay/align "middle" "bottom"
                       (above
                        (overlay (lab (number->string count))
                                 (wc c-rad))
                        (wc c-rad) (wc c-rad) (wc c-rad) (wc c-rad))
                       (light-point c-rad #t))]
       [(and (symbol=? color 'White) (= count 5))
        (overlay/align "middle" "bottom" (above (wc c-rad) (wc c-rad) (wc c-rad)
                                                (wc c-rad) (wc c-rad))
                                               (light-point c-rad #t))]
       [(and (symbol=? color 'White) (= count 4))
        (overlay/align "middle" "bottom" (above (wc c-rad) (wc c-rad)
                                                (wc c-rad) (wc c-rad))
                                               (light-point c-rad #t))]
       [(and (symbol=? color 'White) (= count 3))
        (overlay/align "middle" "bottom" (above (wc c-rad)
                                                (wc c-rad) (wc c-rad))
                                               (light-point c-rad #t))]
       [(and (symbol=? color 'White) (= count 2))
        (overlay/align "middle" "bottom" (above (wc c-rad) (wc c-rad))
                       (light-point c-rad #t))]
       [else
        (overlay/align "middle" "bottom" (wc c-rad) (light-point c-rad #t))])]))
(check-error (state-point-up-light (OccupiedPoint 'Black -5)
                                  (Style 5 2 black-checker1 white-checker1
                                         dark-point light-point board-left
                                         labelfunc
                                         draw-die-black draw-die-white))
             "count should be >= 1, unless 'EmptyPoint")
(state-point-up-light (OccupiedPoint 'White 3)
                    (Style 5 2 black-checker1 white-checker1
                            dark-point light-point board-left labelfunc
                            draw-die-black draw-die-white))
(state-point-up-light (OccupiedPoint 'White 11)
                     (Style 10 2 black-checker1 white-checker1
                            dark-point light-point board-left labelfunc
                            draw-die-black draw-die-white))
(state-point-up-light 'EmptyPoint
                      (Style 5 2 black-checker1 white-checker1 dark-point
                             light-point board-left labelfunc
                             draw-die-black draw-die-white))

(: state-point-down-dark : Point Style -> Image)
;; Same as state-point-up-dark, but instead for dark points pointing DOWN
;; (i.e. the Boolean for "down" is #f), so the point of the triangle is lower
;; than the base.
(define (state-point-down-dark p s)
  (match* (p s)
    [('EmptyPoint (Style c-rad _ _ _ dark-point _ _ _ _ _))
     (dark-point c-rad #f)]
    [((OccupiedPoint color count) (Style c-rad _ bc wc dark-point _ _ lab _ _))
     (cond
       [(<= count 0) (error "count should be >= 1, unless 'EmptyPoint")]
       [(and (symbol=? color 'Black) (= count 1))
        (overlay/align "middle" "top" (bc c-rad) (dark-point c-rad #f))]
       [(and (symbol=? color 'Black) (= count 2))
        (overlay/align "middle" "top" (above (bc c-rad) (bc c-rad))
                                               (dark-point c-rad #f))]
       [(and (symbol=? color 'Black) (= count 3))
        (overlay/align "middle" "top" (above (bc c-rad) (bc c-rad)
                                                (bc c-rad))
                                               (dark-point c-rad #f))]
       [(and (symbol=? color 'Black) (= count 4))
        (overlay/align "middle" "top" (above (bc c-rad) (bc c-rad)
                                                (bc c-rad) (bc c-rad))
                                               (dark-point c-rad #f))]
       [(and (symbol=? color 'Black) (= count 5))
        (overlay/align "middle" "top" (above (bc c-rad) (bc c-rad) (bc c-rad)
                                                (bc c-rad) (bc c-rad))
                                               (dark-point c-rad #f))]
       [(and (symbol=? color 'Black) (> count 5))
        (overlay/align "middle" "top"
                       (above
                        (bc c-rad) (bc c-rad) (bc c-rad) (bc c-rad)
                        (overlay (lab (number->string count))
                                 (bc c-rad)))
                       (dark-point c-rad #f))]
       [(and (symbol=? color 'White) (> count 5))
        (overlay/align "middle" "top"
                       (above
                        (wc c-rad) (wc c-rad) (wc c-rad) (wc c-rad)
                        (overlay (lab (number->string count))
                                 (wc c-rad)))
                       (dark-point c-rad #f))]
       [(and (symbol=? color 'White) (= count 5))
        (overlay/align "middle" "top" (above (wc c-rad) (wc c-rad) (wc c-rad)
                                                (wc c-rad) (wc c-rad))
                                               (dark-point c-rad #f))]
       [(and (symbol=? color 'White) (= count 4))
        (overlay/align "middle" "top" (above (wc c-rad) (wc c-rad)
                                                (wc c-rad) (wc c-rad))
                                               (dark-point c-rad #f))]
       [(and (symbol=? color 'White) (= count 3))
        (overlay/align "middle" "top" (above (wc c-rad)
                                                (wc c-rad) (wc c-rad))
                                               (dark-point c-rad #f))]
       [(and (symbol=? color 'White) (= count 2))
        (overlay/align "middle" "top" (above (wc c-rad) (wc c-rad))
                       (dark-point c-rad #f))]
       [else
        (overlay/align "middle" "top" (wc c-rad) (dark-point c-rad #f))])]))
(check-error (state-point-down-dark (OccupiedPoint 'Black -5)
                                  (Style 5 2 black-checker1 white-checker1
                                         dark-point light-point board-left
                                         labelfunc draw-die-black
                                         draw-die-white))
             "count should be >= 1, unless 'EmptyPoint")
(state-point-down-dark (OccupiedPoint 'White 3)
                    (Style 5 2 black-checker1 white-checker1
                            dark-point light-point board-left labelfunc
                            draw-die-black draw-die-white))
(state-point-down-dark (OccupiedPoint 'White 11)
                     (Style 10 2 black-checker1 white-checker1
                            dark-point light-point board-left labelfunc
                            draw-die-black draw-die-white))
(state-point-down-dark 'EmptyPoint
                     (Style 5 2 black-checker1 white-checker1
                            dark-point light-point board-left labelfunc
                            draw-die-black draw-die-white))

(: state-point-down-light : Point Style -> Image)
;; Same as state-point-up-dark, but instead for LIGHT points pointing DOWN
;; (i.e. the Boolean for "down" is #f), so the point of the triangle is lower
;; than the base.
(define (state-point-down-light p s)
  (match* (p s)
    [('EmptyPoint (Style c-rad _ _ _ _ light-point _ _ _ _))
     (light-point c-rad #f)]
    [((OccupiedPoint color count) (Style c-rad _ bc wc dark-point _ _ lab _ _))
     (cond
       [(<= count 0) (error "count should be >= 1, unless 'EmptyPoint")]
       [(and (symbol=? color 'Black) (= count 1))
        (overlay/align "middle" "top" (bc c-rad) (light-point c-rad #f))]
       [(and (symbol=? color 'Black) (= count 2))
        (overlay/align "middle" "top" (above (bc c-rad) (bc c-rad))
                                               (light-point c-rad #f))]
       [(and (symbol=? color 'Black) (= count 3))
        (overlay/align "middle" "top" (above (bc c-rad) (bc c-rad)
                                                (bc c-rad))
                                               (light-point c-rad #f))]
       [(and (symbol=? color 'Black) (= count 4))
        (overlay/align "middle" "top" (above (bc c-rad) (bc c-rad)
                                                (bc c-rad) (bc c-rad))
                                               (light-point c-rad #f))]
       [(and (symbol=? color 'Black) (= count 5))
        (overlay/align "middle" "top" (above (bc c-rad) (bc c-rad) (bc c-rad)
                                                (bc c-rad) (bc c-rad))
                                               (light-point c-rad #f))]
       [(and (symbol=? color 'Black) (> count 5))
        (overlay/align "middle" "top"
                       (above
                        (bc c-rad) (bc c-rad) (bc c-rad) (bc c-rad)
                        (overlay (lab (number->string count))
                                 (bc c-rad)))
                       (light-point c-rad #f))]
       [(and (symbol=? color 'White) (> count 5))
        (overlay/align "middle" "top"
                       (above
                        (wc c-rad) (wc c-rad) (wc c-rad) (wc c-rad)
                        (overlay (lab (number->string count))
                                 (wc c-rad)))
                       (light-point c-rad #f))]
       [(and (symbol=? color 'White) (= count 5))
        (overlay/align "middle" "top" (above (wc c-rad) (wc c-rad) (wc c-rad)
                                                (wc c-rad) (wc c-rad))
                                               (light-point c-rad #f))]
       [(and (symbol=? color 'White) (= count 4))
        (overlay/align "middle" "top" (above (wc c-rad) (wc c-rad)
                                                (wc c-rad) (wc c-rad))
                                               (light-point c-rad #f))]
       [(and (symbol=? color 'White) (= count 3))
        (overlay/align "middle" "top" (above (wc c-rad)
                                                (wc c-rad) (wc c-rad))
                                               (light-point c-rad #f))]
       [(and (symbol=? color 'White) (= count 2))
        (overlay/align "middle" "top" (above (wc c-rad) (wc c-rad))
                       (light-point c-rad #f))]
       [else
        (overlay/align "middle" "top" (wc c-rad) (light-point c-rad #f))])]))
(check-error (state-point-down-light (OccupiedPoint 'Black -5)
                                  (Style 5 2 black-checker1 white-checker1
                                         dark-point light-point board-left
                                         labelfunc
                                         draw-die-black draw-die-white))
             "count should be >= 1, unless 'EmptyPoint")
(state-point-down-light (OccupiedPoint 'White 3)
                    (Style 5 2 black-checker1 white-checker1
                            dark-point light-point board-left labelfunc
                            draw-die-black draw-die-white))
(state-point-down-light (OccupiedPoint 'Black 11)
                     (Style 10 2 black-checker1 white-checker1
                            dark-point light-point board-left labelfunc
                            draw-die-black draw-die-white))
(state-point-down-light 'EmptyPoint
                     (Style 5 2 black-checker1 white-checker1
                            dark-point light-point board-left labelfunc
                            draw-die-black draw-die-white))


(: first-six : (All (A) (Listof A) -> (Listof A)))
;; returns sublist of the first six elements of the original list
(define (first-six l)
  (match l
    ['() '()]
    [(cons hd tl)
     (cons hd
           (match tl
             [(cons hd tl)
              (cons hd
                                 (match tl
                                   [(cons hd tl)
                                    (cons hd
                                                       (match tl
                                                         [(cons hd tl)
                                                          (cons hd
                         (match tl
                            [(cons hd tl)
                                  (cons hd
                                           (match tl
                                                [(cons hd tl)
                                     (cons hd '())]))]))]))]))]))]))
(check-expect (first-six (list 1 2 3 4 5 6 7)) (list 1 2 3 4 5 6))
(check-expect (first-six '()) '())
(check-expect (first-six (list 3 4 5 6 7 8 9 0)) (list 3 4 5 6 7 8))

(: next-six : (All (A) (Listof A) -> (Listof A)))
;; removes the first six elements and isolates the next six elements
;; returns a new list of those next six elements
(define (next-six l)
  (match l
    ['() '()]
    [(cons hd tl)
     (first-six (rest (rest (rest (rest (rest tl))))))]))
(check-expect (next-six (list 3 4 5 6 7 8 2 3 4 5 6 2)) (list 2 3 4 5 6 2))
(check-expect (next-six '()) '())
(check-expect (next-six (list 3 4 5 6 7 8 9 0 1 2 3 1 4))
              (list 9 0 1 2 3 1))

(: next-next-six : (All (A) (Listof A) -> (Listof A)))
;; isolates elements 13-18 of a list
(define (next-next-six l)
  (match l
    ['() '()]
    [(cons hd tl)
     (first-six
      (rest (rest (rest (rest (rest (rest
                               (rest (rest (rest (rest (rest tl))))))))))))]))
(check-expect
 (next-next-six (list 3 4 5 6 7 8 2 3 4 5 6 2 1 2 5 3 2 6)) (list 1 2 5 3 2 6))
(check-expect (next-next-six '()) '())
(check-expect
 (next-next-six (list 3 4 5 6 7 8 9 0 1 2 3 1 4 1 3 2 5 3 1))
 (list 4 1 3 2 5 3))

(: last-six : (All (A) (Listof A) -> (Listof A)))
;; isolates elements 19-24 of a list
(define (last-six l)
  (match l
    ['() '()]
    [(cons hd tl)
     (first-six
      (rest (rest (rest (rest (rest (rest
                                     (rest (rest (rest (rest (rest (rest
                               (rest (rest (rest (rest
                                                  (rest tl))))))))))))))))))]))
(check-expect
 (last-six (list 3 4 5 6 7 8 2 3 4 5 6 2 1 2 5 3 2 6 1 2 3 4 5 6))
 (list 1 2 3 4 5 6))
(check-expect (last-six '()) '())
(check-expect
 (last-six (list 3 4 5 6 7 8 9 0 1 2 3 1 4 1 3 2 5 3 1 2 3 4 5 6 7))
 (list 1 2 3 4 5 6))

;; Sample board for later testing
(define sampleboard4 : Board
  (Board (list (OccupiedPoint 'Black 3) (OccupiedPoint 'White 3) 'EmptyPoint
               'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
               'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
               'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
               'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
               'EmptyPoint) 5 5 7 7))

;; sample style for later testing
(define samplestyle1 : Style
  (Style 9 2 black-checker1 white-checker1
         dark-point light-point board-left labelfunc draw-die-black
         draw-die-white))

;; Sample board for later testing
(define sampleboard5 : Board
  (Board '() 5 5 7 7))

(: first-points : Board Style -> Image)
;; construct an image of  the first six points
;; for the bottom row of the board (right half)
(define (first-points b s)
  (match* (b s)
    [((Board xs _ _ _ _) (Style _ sp _ _ _ _ _ _ _ _))
     (match (first-six xs)
       ['() (error "list of points in the board struct shouldn't be empty")]
       [(cons hd tl)
        (beside/align "bottom"
                      (state-point-up-light (first (rest (rest (rest
                                                                (rest tl))))) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-up-dark (first (rest (rest (rest tl)))) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-up-light (first (rest (rest tl))) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-up-dark (first (rest tl)) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-up-light (first tl) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-up-dark hd s))])]))
(check-error (first-points sampleboard5 samplestyle1)
             "list of points in the board struct shouldn't be empty")
(first-points sampleboard4 samplestyle1) ;; it worked!!!!!

(: next-points : Board Style -> Image)
;; construct an image of  the points 7-12
;; for the bottom row of the board (left half)
(define (next-points b s)
  (match* (b s)
    [((Board xs _ _ _ _) (Style _ sp _ _ _ _ _ _ _ _))
     (match (next-six xs)
       ['() (error "list of points in the board struct shouldn't be empty")]
       [(cons hd tl)
        (beside/align "bottom"
                      (state-point-up-light (first (rest (rest (rest
                                                                (rest tl))))) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-up-dark (first (rest (rest (rest tl)))) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-up-light (first (rest (rest tl))) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-up-dark (first (rest tl)) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-up-light (first tl) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-up-dark hd s))])]))
(check-error (next-points sampleboard5 samplestyle1)
             "list of points in the board struct shouldn't be empty")
(next-points sampleboard3 samplestyle1) ;; it worked!!!!!

(: next-next-points : Board Style -> Image)
;; construct an image of  the points 13-18
;; for the top row of the board (left half)
(define (next-next-points b s)
  (match* (b s)
    [((Board xs _ _ _ _) (Style _ sp _ _ _ _ _ _ _ _))
     (match (next-next-six xs)
       ['() (error "list of points in the board struct shouldn't be empty")]
       [(cons hd tl)
        (beside/align "top"
                      (state-point-down-dark hd s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-down-light (first tl) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-down-dark (first (rest tl)) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-down-light (first (rest (rest tl))) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-down-dark (first (rest (rest (rest tl)))) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-down-light
                       (first (rest (rest (rest (rest tl))))) s))])]))
(check-error (next-next-points sampleboard5 samplestyle1)
             "list of points in the board struct shouldn't be empty")
(next-next-points sampleboard2 samplestyle1) ;; it worked!!!!!

(: last-points : Board Style -> Image)
;; construct an image of  the points 19-24
;; for the top row of the board (right half)
(define (last-points b s)
  (match* (b s)
    [((Board xs _ _ _ _) (Style _ sp _ _ _ _ _ _ _ _))
     (match (last-six xs)
       ['() (error "list of points in the board struct shouldn't be empty")]
       [(cons hd tl)
        (beside/align "top"
                      (state-point-down-dark hd s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-down-light (first tl) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-down-dark (first (rest tl)) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-down-light (first (rest (rest tl))) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-down-dark (first (rest (rest (rest tl)))) s)
                      (rectangle sp 0 "outline" "white")
                      (state-point-down-light
                       (first (rest (rest (rest (rest tl))))) s))])]))
(check-error (last-points sampleboard5 samplestyle1)
             "list of points in the board struct shouldn't be empty")
(last-points sampleboard1 samplestyle1) ;; it worked!!!!! beautiful

(: draw-right-half : Board Style -> Image)
;; draws/constructs the right half of the board, including relevant points
;; based on the board state (as specified by the board struct)
(define (draw-right-half b s)
  (match s
    [(Style c-rad sp _ _ _ _ _ _ _ _)
     (overlay/align "middle" "top" (last-points b s)
                    (overlay/align "middle" "bottom"
                                   (first-points b s)
                                   (board-right c-rad sp)))]))
(draw-right-half sampleboard4 samplestyle1) ;; it works

(: draw-left-half : Board Style -> Image)
;; draws/constructs the left half of the board, including relevant points
;; based on the board state (as specified by the board struct)
(define (draw-left-half b s)
  (match s
    [(Style c-rad sp _ _ _ _ _ _ _ _)
     (overlay/align "middle" "top" (next-next-points b s)
                    (overlay/align "middle" "bottom"
                                   (next-points b s)
                                   (board-left c-rad sp)))]))
(draw-left-half sampleboard2 samplestyle1) ;; it works

(: half-bg : Style -> Image)
;; draws partial background for the half of the board
(define (half-bg s)
  (match s
    [(Style c-rad sp _ _ _ _ _ _ _ _)
     (overlay
      (rectangle (+ (* sp 5) (* c-rad  12) (* c-rad 6))
                (* 28 c-rad) "outline" "black")
      (rectangle (+ (* sp 5) (* c-rad  12) (* c-rad 6))
                 (* 28 c-rad) "solid" "brown"))]))

;; White 1 at the bottom right corner

(: white-off : Style Board -> Image)
;; Image for white checkers that have been borne off (bottom right)
(define (white-off s b)
  (match* (s b)
    [((Style c-rad _ _ wc _ _ _ lab _ _) (Board _ _ _ _ wo))
     (cond
       [(< wo 0) (error "cannot have negative values for white-off")]
       [(= wo 0) (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown")]
       [(= wo 1)
        (overlay/align "middle" "bottom" (wc c-rad)
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown"))]
       [(= wo 2)
        (overlay/align "middle" "bottom"
                       (above (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown"))]
       [(= wo 3)
        (overlay/align "middle" "bottom"
                       (above (wc c-rad) (wc c-rad)
                              (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown"))]
       [(= wo 4)
        (overlay/align "middle" "bottom"
                       (above (wc c-rad) (wc c-rad) (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown"))]
       [(= wo 5)
        (overlay/align "middle" "bottom"
                       (above (wc c-rad) (wc c-rad) (wc c-rad)
                              (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown"))]
       [else
        (overlay/align "middle" "bottom"
                       (above
                        (overlay (lab (number->string wo))
                                 (wc c-rad))
                        (wc c-rad) (wc c-rad)
                        (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown"))])]))
(white-off samplestyle1 sampleboard3) ;; eyeball test -> it worked
(check-error (white-off samplestyle1 (Board '() 0 0 0 -5))
             "cannot have negative values for white-off")

(: black-off : Style Board -> Image)
;; Image for black checkers that have been borne off (top right)
(define (black-off s b)
  (match* (s b)
    [((Style c-rad _ wc _ _ _ _ lab _ _) (Board _ _ _ wo _))
     (cond
       [(< wo 0) (error "cannot have negative values for black-off")]
       [(= wo 0) (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown")]
       [(= wo 1)
        (overlay/align "middle" "top" (wc c-rad)
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown"))]
       [(= wo 2)
        (overlay/align "middle" "top"
                       (above (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown"))]
       [(= wo 3)
        (overlay/align "middle" "top"
                       (above (wc c-rad) (wc c-rad)
                              (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown"))]
       [(= wo 4)
        (overlay/align "middle" "top"
                       (above (wc c-rad) (wc c-rad) (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown"))]
       [(= wo 5)
        (overlay/align "middle" "top"
                       (above (wc c-rad) (wc c-rad) (wc c-rad)
                              (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown"))]
       [else
        (overlay/align "middle" "top"
                       (above
                        (wc c-rad) (wc c-rad)
                        (wc c-rad) (wc c-rad)
                        (overlay (lab (number->string wo))
                                 (wc c-rad)))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown"))])]))
(black-off samplestyle1 sampleboard3) ;; eye-ball test -> it worked
(check-error (black-off samplestyle1 (Board '() 0 0 -5 0))
             "cannot have negative values for black-off")

(: black-bar : Style Board -> Image)
;; Image of the part of the bar depicting black checkers on the bar
;; based on the relevant board struct (above)
(define (black-bar s b)
  (match* (s b)
    [((Style c-rad _ wc _ _ _ _ lab _ _) (Board _ wo _ _ _))
     (cond
       [(< wo 0) (error "cannot have negative values for black-bar")]
       [(= wo 0) (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown")]
       [(= wo 1)
        (overlay/align "middle" "bottom" (wc c-rad)
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid"
                                  "rosybrown"))]
       [(= wo 2)
        (overlay/align "middle" "bottom"
                       (above (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid"
                                  "rosybrown"))]
       [(= wo 3)
        (overlay/align "middle" "bottom"
                       (above (wc c-rad) (wc c-rad)
                              (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid"
                                  "rosybrown"))]
       [(= wo 4)
        (overlay/align "middle" "bottom"
                       (above (wc c-rad) (wc c-rad) (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid"
                                  "rosybrown"))]
       [(= wo 5)
        (overlay/align "middle" "bottom"
                       (above (wc c-rad) (wc c-rad) (wc c-rad)
                              (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid"
                                  "rosybrown"))]
       [else
        (overlay/align "middle" "bottom"
                       (above
                        (overlay (lab (number->string wo))
                                 (wc c-rad))
                        (wc c-rad) (wc c-rad)
                        (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad)
                                  "solid" "rosybrown"))])]))
(black-bar samplestyle1 sampleboard3) ;; it worked
(black-bar samplestyle1 extrasampleboard) ;; it worked
(check-error (black-bar samplestyle1 (Board '() -2 0 0 0))
             "cannot have negative values for black-bar")

(: white-bar : Style Board -> Image)
;; Image of the part of the bar depicting white checkers on the bar
;; based on the relevant board struct (below)
(define (white-bar s b)
  (match* (s b)
    [((Style c-rad _ _ wc _ _ _ lab _ _) (Board a b wo c d))
     (cond
       [(< wo 0) (white-bar s (Board a b 0 c d))]
       [(= wo 0) (rectangle (* 2 c-rad) (* 10 c-rad) "solid" "rosybrown")]
       [(= wo 1)
        (overlay/align "middle" "top" (wc c-rad)
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid"
                                  "rosybrown"))]
       [(= wo 2)
        (overlay/align "middle" "top"
                       (above (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid"
                                  "rosybrown"))]
       [(= wo 3)
        (overlay/align "middle" "top"
                       (above (wc c-rad) (wc c-rad)
                              (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid"
                                  "rosybrown"))]
       [(= wo 4)
        (overlay/align "middle" "top"
                       (above (wc c-rad) (wc c-rad) (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid"
                                  "rosybrown"))]
       [(= wo 5)
        (overlay/align "middle" "top"
                       (above (wc c-rad) (wc c-rad) (wc c-rad)
                              (wc c-rad) (wc c-rad))
                       (rectangle (* 2 c-rad) (* 10 c-rad) "solid"
                                  "rosybrown"))]
       [else
        (overlay/align "middle" "top"
                       (above
                        (wc c-rad) (wc c-rad)
                        (wc c-rad) (wc c-rad)
                        (overlay (lab (number->string wo))
                                 (wc c-rad)))
                       (rectangle (* 2 c-rad) (* 10 c-rad)
                                  "solid" "rosybrown"))])]))
(white-bar samplestyle1 sampleboard3) ;; it worked
(white-bar samplestyle1 extrasampleboard) ;; it worked
(white-bar samplestyle1 (Board '() 0 -2 0 0)) ;; expecting empty white-bar

(: bar : Style Board -> Image)
;; draw the image of type Image of the entire status of the bar
;; (black checkers and white checkers, respectively, depending on the described
;; state of the board by the inputted Board struct (and scaled by the
;; checker radius in the inputted Style struct)
(define (bar s b)
  (above (black-bar s b) (white-bar s b)))
(bar samplestyle1 sampleboard3) ;; it worked!
(bar samplestyle1 extrasampleboard) ;; it worked!

(: guarantee-byte : Integer -> Byte)
;; ensures that the inputted Integer is a Byte (guarantees it)
;; borrowed from Professor Shaw's working version.
(define (guarantee-byte i)
  (if (byte? i) i (error "not a Byte")))
(check-error (guarantee-byte 300) "not a Byte")
(check-expect (guarantee-byte 0) 0)
(check-expect (guarantee-byte 255) 255)
(check-expect (guarantee-byte 10) 10)

(: draw-board : Style Board -> Image)
;; takes in a style and a board, and, utilizing helper functions that
;; generate portions of the final image of the state of the board
;; (based on the inputted Style and Board), generates the combined
;; Image for the state of the board for a given Style and Board.
(define (draw-board s b)
  (match s
    [(Style c-rad sp _ _ _ _ _ _ _ _)
     (underlay/offset
     (underlay/align "middle" "top"
                     (overlay/align "middle" "bottom"
                     (text "press the 'r' key to reset the board"
                           (guarantee-byte c-rad) "black")
                     (overlay
                     (rectangle (* (+ (* sp 5) (* c-rad  12) (* c-rad 6)) 2)
                     (* (/ 295 10) c-rad) "outline" "black")
                     (rectangle (* (+ (* sp 5) (* c-rad  12) (* c-rad 6)) 2)
                     (* (/ 295 10) c-rad) "solid" "white")))
     (overlay (bar s b)
              (overlay
               (rectangle (* (+ (* sp 5) (* c-rad  12) (* c-rad 6)) 2)
                (* 28 c-rad) "outline" "black")
               (rectangle (* 2 c-rad) (* 20 c-rad) "solid" "rosybrown")
               (rectangle (* (/ 3 2) c-rad) (* 22 c-rad) "solid" "rosybrown")
               (rectangle c-rad (* 24 c-rad) "solid" "rosybrown")
               (rectangle (* (/ 1 2) c-rad) (* 26 c-rad) "solid" "rosybrown")
               (overlay/align
                        "right" "bottom" (white-off s b)
                        (overlay/align
                                       "right" "top"
                                       (black-off s b)
                                       (beside
                                        (overlay (draw-left-half b s)
                                                 (half-bg s))
                                        (overlay (draw-right-half b s)
                                                 (half-bg s))))))))
 0 (- (* (/ 3 4) c-rad)) (rectangle (* 2 c-rad)
                                    (* 0.1 c-rad) "solid" "black"))]))

;; Eye-ball visual tests for draw-board function are below:
(draw-board samplestyle1 sampleboard3) ;; eye-ball tested and it works!
(draw-board samplestyle1 extrasampleboard) ;; eye-ball tested and it works!
(draw-board samplestyle1 emptysampleboard) ;; eye-ball tested and it works!

;; Board struct describing the state of the board prior to play.
;; Initial/starting board that is set-up before any moves are made.
(define starting-board : Board
  (Board
   (list (OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint
         (OccupiedPoint 'White 5) 'EmptyPoint (OccupiedPoint 'White 3)
         'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
         (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 3)
         'EmptyPoint
         (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint
         (OccupiedPoint 'White 2)) 0 0 0 0))

;; The relevant functions for the fields of this sample test struct
;; (test-style) are defined as helper functions earlier in this code file.
;; The constant, test-style (defined below), is
;; for testing, and if my draw-board function is called with my test-style,
;; it will render an image of type Image with all the needed characteristics.
(define test-style : Style
  (Style 10 5 black-checker1 white-checker1
         dark-point light-point board-left labelfunc draw-die-black draw-die-white))
(draw-board test-style starting-board) ;; draws the starting state of the board

;; REMARK: eye-ball tests for my dice drawing functions (for the respective two
;; of dice) are earlier in my code so that subsequent edited code with my
;; new Style struct (with two additional fields at this point in the project)
;; all work. Scroll up in the interactions panel to see the results of these
;; eyeball tests, which worked and passed upon visual inspection.

;; Defining the Game struct below with two fields - board of type Board and turn
;; of type Player - in order to specify and represent the current game state.
(define-struct Game
  ([board : Board]
   [turn : Player]
   [moves : (Listof Integer)]))

;; struct definition to specify the selected point number (from White 1 to
;; White 24), where point White 1 corresponds to (PointNum 1) and
;; point Black 1 corresponds to (PointNum 24).
(define-struct PointNum
  ([num : Integer]))

;; defining the type ClickLoc, the location of a Mouse-Event mouse click,
;; as the union of PointNum, 'BlackBar, 'WhiteBar, 'BlackOff, 'WhiteOff,
;; 'BlackDice 'WhiteDice, and 'Nowhere. The Symbol 'Nowhere represents
;; an invalid location for a click. ClickLoc represents information about
;; location of a mouse click on a part of the board.
(define-type ClickLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
                         'BlackDice 'WhiteDice 'Nowhere))

;; The type defined below, BoardLoc, is a subset of the possibilites of
;; ClickLoc that is restricted solely to the union of PointNum, 'BlackBar
;; 'WhiteBar, 'BlackOff, 'WhiteOff, and 'Nowhere. In other words, it EXCLUDES
;; 'BlackDice and 'WhiteDice clicks. In other words, BoardLoc represents
;; solely checker locations on the board (as well as the invalid
;; location represented by the Symbol 'Nowhere within the BoardLoc type).
(define-type BoardLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
                         'Nowhere))

(: click-where : Style Integer Integer -> ClickLoc)
;; This helper function click-where takes in a Style and a mouse click location
;; represented by two integers of type Integer (x and y coordinates of the
;; mouse click). With these parameters, it returns a ClickLoc to specify
;; where the user clicked. It takes into account checker radius and
;; spacing specifications in the Style struct - as well as how my board Image
;; is produced by my draw-board function - to accurately describe where the
;; user clicked on the board.
(define (click-where style x y)
  (match style
    [(Style c-rad sp _ _ _ _ _ _ _ _)
     ;; total board dimensions are (10*sp + 36*c-rad)wide by 28*c-rad high
     ;; for height, middle portion is 6*c-rad, outer edges are c-rad each
     ;; for weidth, middle portion is 6*c-rad and outside edges are 3*c-rad each
     ;; specify range for xs and ys to determine where the user clicked
     (cond
       [(and (< c-rad y (* 11 c-rad)) (< (* 3 c-rad) x (* 5 c-rad)))
        (PointNum 13)] ;; c-rad is given checker-radius by the Style input
       [(and (< c-rad y (* 11 c-rad))
             (< (+ (* 5 c-rad) sp) x (+ (* 7 c-rad) sp))) (PointNum 14)]
       [(and (< c-rad y (* 11 c-rad))
             (< (+ (* 7 c-rad) (* 2 sp)) x (+ (* 9 c-rad) (* 2 sp))))
        (PointNum 15)]
       [(and (< c-rad y (* 11 c-rad))
             (< (+ (* 9 c-rad) (* 3 sp)) x (+ (* 11 c-rad) (* 3 sp))))
        (PointNum 16)]
       [(and (< c-rad y (* 11 c-rad))
             (< (+ (* 11 c-rad) (* 4 sp)) x (+ (* 13 c-rad) (* 4 sp))))
        (PointNum 17)]
       [(and (< c-rad y (* 11 c-rad))
             (< (+ (* 13 c-rad) (* 5 sp)) x (+ (* 15 c-rad) (* 5 sp))))
        (PointNum 18)]
       [(and (< c-rad y (* 11 c-rad))
             (< (+ (* 21 c-rad) (* 5 sp)) x (+ (* 23 c-rad) (* 5 sp))))
        (PointNum 19)]
       [(and (< c-rad y (* 11 c-rad))
             (< (+ (* 23 c-rad) (* 6 sp)) x (+ (* 25 c-rad) (* 6 sp))))
        (PointNum 20)]
       [(and (< c-rad y (* 11 c-rad))
             (< (+ (* 25 c-rad) (* 7 sp)) x (+ (* 27 c-rad) (* 7 sp))))
        (PointNum 21)]
       [(and (< c-rad y (* 11 c-rad))
             (< (+ (* 27 c-rad) (* 8 sp)) x (+ (* 29 c-rad) (* 8 sp))))
        (PointNum 22)]
       [(and (< c-rad y (* 11 c-rad))
             (< (+ (* 29 c-rad) (* 9 sp)) x (+ (* 31 c-rad) (* 9 sp))))
        (PointNum 23)]
       [(and (< c-rad y (* 11 c-rad))
             (< (+ (* 31 c-rad) (* 10 sp)) x (+ (* 33 c-rad) (* 10 sp))))
        (PointNum 24)]
       [(and (< (* 17 c-rad) y (* 27 c-rad)) (< (* 3 c-rad) x (* 5 c-rad)))
        (PointNum 12)]
       [(and (< (* 17 c-rad) y (* 27 c-rad))
             (< (+ (* 5 c-rad) sp) x (+ (* 7 c-rad) sp))) (PointNum 11)]
       [(and (< (* 17 c-rad) y (* 27 c-rad))
             (< (+ (* 7 c-rad) (* 2 sp)) x (+ (* 9 c-rad) (* 2 sp))))
        (PointNum 10)]
       [(and (< (* 17 c-rad) y (* 27 c-rad))
             (< (+ (* 9 c-rad) (* 3 sp)) x (+ (* 11 c-rad) (* 3 sp))))
        (PointNum 9)]
       [(and (< (* 17 c-rad) y (* 27 c-rad))
             (< (+ (* 11 c-rad) (* 4 sp)) x (+ (* 13 c-rad) (* 4 sp))))
        (PointNum 8)]
       [(and (< (* 17 c-rad) y (* 27 c-rad))
             (< (+ (* 13 c-rad) (* 5 sp)) x (+ (* 15 c-rad) (* 5 sp))))
        (PointNum 7)]
       [(and (< (* 17 c-rad) y (* 27 c-rad))
             (< (+ (* 21 c-rad) (* 5 sp)) x (+ (* 23 c-rad) (* 5 sp))))
        (PointNum 6)]
       [(and (< (* 17 c-rad) y (* 27 c-rad))
             (< (+ (* 23 c-rad) (* 6 sp)) x (+ (* 25 c-rad) (* 6 sp))))
        (PointNum 5)]
       [(and (< (* 17 c-rad) y (* 27 c-rad))
             (< (+ (* 25 c-rad) (* 7 sp)) x (+ (* 27 c-rad) (* 7 sp))))
        (PointNum 4)]
       [(and (< (* 17 c-rad) y (* 27 c-rad))
             (< (+ (* 27 c-rad) (* 8 sp)) x (+ (* 29 c-rad) (* 8 sp))))
        (PointNum 3)]
       [(and (< (* 17 c-rad) y (* 27 c-rad))
             (< (+ (* 29 c-rad) (* 9 sp)) x (+ (* 31 c-rad) (* 9 sp))))
        (PointNum 2)]
       [(and (< (* 17 c-rad) y (* 27 c-rad))
             (< (+ (* 31 c-rad) (* 10 sp)) x (+ (* 33 c-rad) (* 10 sp))))
        (PointNum 1)]
       [(and (< (+ (* 5 sp) (* 17 c-rad)) x (+ (* 5 sp) (* 19 c-rad)))
             (< (* 4 c-rad) y (* 14 c-rad))) 'BlackBar]
       [(and (< (+ (* 5 sp) (* 17 c-rad)) x (+ (* 5 sp) (* 19 c-rad)))
             (< (* 14 c-rad) y (* 24 c-rad))) 'WhiteBar]
       [(and (< 0 y (* 10 c-rad))
             (< (+ (* 10 sp) (* 34 c-rad)) x (+ (* 10 sp) (* 36 c-rad))))
        'BlackOff]
       [(and (< (* 18 c-rad) y (* 28 c-rad))
             (< (+ (* 10 sp) (* 34 c-rad)) x (+ (* 10 sp) (* 36 c-rad))))
        'WhiteOff]
       [(and (< (+ (* 6 c-rad) (* 2 sp)) x (+ (* 12 c-rad) (* 3 sp)))
             (< (* 12 c-rad) y (* 16 c-rad))) 'WhiteDice]
       [(and (< (+ (* 24 c-rad) (* 7 sp)) x (+ (* 30 c-rad) (* 8 sp)))
             (< (* 12 c-rad) y (* 16 c-rad))) 'BlackDice]
       [else 'Nowhere])]))
;; samplestyle1 has checker-radius 9 and spacing 2
(check-expect (click-where samplestyle1 25 15) 'Nowhere)
(check-expect (click-where samplestyle1 99 108) 'Nowhere)
(check-expect (click-where samplestyle1 29 15) (PointNum 13))
(check-expect (click-where samplestyle1 308 20) (PointNum 24))
(check-expect (click-where samplestyle1 50 160) (PointNum 11))
(check-expect (click-where samplestyle1 308 172) (PointNum 1))
(check-expect (click-where samplestyle1 290 172) (PointNum 2))
(check-expect (click-where samplestyle1 172 90) 'BlackBar)
(check-expect (click-where samplestyle1 172 189) 'WhiteBar)
(check-expect (click-where samplestyle1 335 45) 'BlackOff)
(check-expect (click-where samplestyle1 340 236) 'WhiteOff)

(: list-replace : (All (A) (Listof A) Integer A -> (Listof A)))
;; helper function that takes in an Integer (index number plus 1)
;; and an element of the same type of the elements of the list (type A).
;; It returns a list of the same type (Listof A) such that the element of that
;;list with the inputted index number (+1) is replaced with the inputted element
;; of type A. Otherwise, the (Listof A) list is kept the same.
(define (list-replace xs i a)
  (match xs
    ['() '()]
    [(cons hd tl) (if (= i 1) (cons a tl)
                      (cons hd (list-replace tl (- i 1) a)))]))
(check-expect (list-replace '() 0 5) '())
(check-expect (list-replace '() 3 5) '())
(check-expect (list-replace (list 1 2 3 4) 1 5) (list 5 2 3 4))
(check-expect (list-replace (list 1 2 3 4) 3 15) (list 1 2 15 4))
(check-expect (list-replace (list "a" "bb" "c") 2 "h") (list "a" "h" "c"))

(: point-add : Point Integer -> Point)
;; helper function that adds or subtracts checkers from a point already occupied
;; works with subtracting too (by adding negative Integer)
(define (point-add point num)
  (match point
    ['EmptyPoint 'EmptyPoint]
    [(OccupiedPoint col count)
     (cond
       [(= (- count) num) 'EmptyPoint]
       [(> (- num) count) (error "can't have negative count")]
       [else (OccupiedPoint col (+ count num))])]))
(check-expect (point-add 'EmptyPoint 5) 'EmptyPoint)
(check-expect (point-add (OccupiedPoint 'Black 5) 2) (OccupiedPoint 'Black 7))
(check-expect (point-add (OccupiedPoint 'White 3) -2) (OccupiedPoint 'White 1))
(check-expect (point-add (OccupiedPoint 'White 3) -3) 'EmptyPoint)
(check-error (point-add (OccupiedPoint 'White 3) -4)
             "can't have negative count")

(: point-add-empty : Point Player -> Point)
;; helper function that adds a checker of an inputted color (symbol 'Black or
;; 'White) to an 'EmptyPoint
(define (point-add-empty p col)
  (match p
    [(OccupiedPoint _ _) (error "use a different function")]
    ['EmptyPoint (OccupiedPoint col 1)]))
(check-error (point-add-empty (OccupiedPoint 'Black 3) 'Black)
             "use a different function")
(check-error (point-add-empty (OccupiedPoint 'White 3) 'Black)
             "use a different function")
(check-expect (point-add-empty 'EmptyPoint 'Black) (OccupiedPoint 'Black 1))
(check-expect (point-add-empty 'EmptyPoint 'White) (OccupiedPoint 'White 1))

(: point-returner : (Listof Point) Integer -> Point)
;; helper function that returns a desired point from a list of points based
;; on an inputted Integer representing point number (with respect to 'White)
(define (point-returner points i)
  (match points
    ['() (error "list of points is empty")]
    [(cons hd tl) (if (= i 1) hd (point-returner tl (- i 1)))]))
(check-expect (point-returner (list 'EmptyPoint 'EmptyPoint) 1) 'EmptyPoint)
(check-expect (point-returner (list 'EmptyPoint 'EmptyPoint) 2) 'EmptyPoint)
(check-error (point-returner '() 0) "list of points is empty")
(check-expect (point-returner (list 'EmptyPoint 'EmptyPoint
                    (OccupiedPoint 'Black 3)) 3) (OccupiedPoint 'Black 3))

(: emptypoint? : Point -> Boolean)
;; helper that returns true #t if and only if the inputted point is 'EmptyPoint
(define (emptypoint? p)
  (match p
    ['EmptyPoint #t]
    [_ #f]))
(check-expect (emptypoint? 'EmptyPoint) #t)
(check-expect (emptypoint? (OccupiedPoint 'Black 2)) #f)

(: guarantee-occupied : Point -> OccupiedPoint)
;; helper that guarantees that the inputted point is an OccupiedPoint
(define (guarantee-occupied p)
  (match p
    ['EmptyPoint (error "is not an 'OccupiedPoint")]
    [(OccupiedPoint _ _) p]))
(check-error (guarantee-occupied 'EmptyPoint) "is not an 'OccupiedPoint")
(check-expect (guarantee-occupied (OccupiedPoint 'Black 2))
              (OccupiedPoint 'Black 2))

(: what-color : (Listof Point) Integer -> Player)
;; returns the player color of type Symbol ('Black or 'White)
;; of the inputted index (Integer) for the inputted (Listof Point)
(define (what-color points i)
  (match points
    ['() (error "list of points is empty")]
    [(cons _ _) (if (emptypoint? (point-returner points i))
                    'Black
                    (OccupiedPoint-color
                     (guarantee-occupied (point-returner points i))))]))
(check-error (what-color '() 1) "list of points is empty")
(check-expect (what-color (list 'EmptyPoint 'EmptyPoint) 1)
             'Black)
(check-expect (what-color (list 'EmptyPoint (OccupiedPoint 'Black 5)) 2)
                              'Black)
(check-expect (what-color (list (OccupiedPoint 'White 5) 'EmptyPoint) 1)
                              'White)

(: rm-list : Integer (Listof Integer) -> (Listof Integer))
;; removes the inputted Integer from the inputted (Listof Integer),
;; if it is in the list
(define (rm-list i xs)
  (match xs
    ['() '()] ;; nothing is removed from an empty list
    [(cons hd tl)
     (if (= hd i) tl (cons hd (rm-list  i tl)))]))
(check-expect (rm-list 5 (list 1 2 3)) (list 1 2 3))
(check-expect (rm-list 5 '()) '())
(check-expect (rm-list 5 (list 5 6 7)) (list 6 7))
(check-expect (rm-list 5 (list 4 5 6)) (list 4 6))
(check-expect (rm-list 5 (list 3 4 5)) (list 3 4))

(: largest-in-list : (Listof Integer) -> Integer)
;; returns largest item from list
(define (largest-in-list xs)
  (match xs
    ['() 0]
    [(cons hd tl) (max hd (largest-in-list tl))]))
(check-expect (largest-in-list '()) 0)
(check-expect (largest-in-list (list 1 2 3)) 3)
(check-expect (largest-in-list (list 1 2 3 3)) 3)
(check-expect (largest-in-list (list 1 3 3 2)) 3)
(check-expect (largest-in-list (list 3 2 1)) 3)
(check-expect (largest-in-list (list 3 2 3 1)) 3)

(: larger-in-list : Integer (Listof Integer) -> (Listof Integer))
;; removes from the list largest integer that is larger than
;; or equal to the inputted integer
(define (larger-in-list i xs)
  (match xs
    ['() '()]
    [(cons hd tl) (cond
                    [(in-list? i xs) (rm-list i xs)]
                    [(>= (largest-in-list xs) i)
                      (rm-list (largest-in-list xs) xs)]
                    [else xs])]))
(check-expect (larger-in-list 5 '()) '())
(check-expect (larger-in-list 5 (list 5 6 7 8)) (list 6 7 8))
(check-expect (larger-in-list 9 (list 1 2 3)) (list 1 2 3))
(check-expect (larger-in-list 2 (list 2 3)) (list 3))

(: apply-move : Game BoardLoc BoardLoc -> Game)
;; Function that takes in a Game, origin location (click 1) and destination
;; location (click 2) to return a new Game with the desired move applied to it.
;; So, selected checker moves from origin location to destination location, and
;; hit checkers in the process of a move are automatically moved to the bar.
;; Moves that are not allowed raise an error. Origin locations are points and
;; the bar, and destination locations are points and bearing-off locations.
;; Clicking on 'Nowhere locations does nothing (is ignored)
(define (apply-move game boardloc1 boardloc2)
  (match game
    [(Game (Board points bb wb bo wo) turn moves) ;; points is a (Listof Point)
     (match* (boardloc1 boardloc2)
       [((PointNum a) (PointNum b)) ;; account for hitting checkers to the bar
        (if (legal-move? game (PointNum a) (PointNum b))
        (cond
          [(= a b) game]
          [(emptypoint? (point-returner points a)) game]
          [(emptypoint? (point-returner points b))
           (Game (Board (list-replace
             (list-replace points a (point-add (point-returner points a) -1))
             b (point-add-empty (point-returner points b)
                                (what-color points a))) bb wb bo wo)
           turn
                 (rm-list (abs (distance boardloc1 boardloc2)) moves))]
          [(or (symbol=? (what-color points a) (what-color points b))
                (emptypoint? (point-returner points b)))
           (Game (Board (list-replace
                (list-replace points a (point-add (point-returner points a) -1))
                   b (point-add (point-returner points b) 1)) bb wb bo wo)
             turn
                 (rm-list (abs (distance boardloc1 boardloc2)) moves))]
          [else
           (cond
             [(and (= (OccupiedPoint-count
                       (guarantee-occupied (point-returner points b))) 1)
                   (not (symbol=? (what-color points a) (what-color points b))))
              (if (symbol=? (what-color points b) 'Black)
              (Game (Board
               (list-replace
                (list-replace points a (point-add (point-returner points a) -1))
                b (OccupiedPoint (what-color points a) 1)) (add1 bb) wb bo wo)
            turn
                 (rm-list (abs (distance boardloc1 boardloc2)) moves))
              (Game (Board
               (list-replace
                (list-replace points a (point-add (point-returner points a) -1))
                b (OccupiedPoint (what-color points a) 1)) bb (add1 wb) bo wo)
            turn
                 (rm-list (abs (distance boardloc1 boardloc2)) moves)))]
             [else game])])
        (error "illegal move"))]
       [((PointNum a) 'BlackOff) (if
                                  (and (symbol=? (what-color points a) 'Black)
                                       (<= 19 a 24)
                                (legal-move? game (PointNum a) 'BlackOff))
              (Game (Board
               (list-replace points a (point-add (point-returner points a) -1))
               bb wb (add1 bo) wo)
            turn
                 (larger-in-list (abs (distance boardloc1 boardloc2)) moves))
              (error "illegal move"))]
       [((PointNum a) 'WhiteOff) (if
                                  (and (symbol=? (what-color points a) 'White)
                                  (<= 1 a 6)
                            (legal-move? game (PointNum a) 'WhiteOff))
              (Game (Board
               (list-replace points a (point-add (point-returner points a) -1))
               bb wb bo (add1 wo))
            turn
                 (larger-in-list (abs (distance boardloc1 boardloc2)) moves))
              (error "illegal move"))]
       [('BlackBar (PointNum x))
        (if (and (<= 1 x 6) ;; can only move to opponent's home board from
            (legal-move? game 'BlackBar (PointNum x)))     ;; 'BlackBar
        (cond
          [(not (emptypoint? (point-returner points x)))
       (cond
         [(and (symbol=? (what-color points x) 'White)
                (> (OccupiedPoint-count
                    (guarantee-occupied (point-returner points x))) 1)) game]
         [(and (symbol=? (what-color points x) 'White)
                (= (OccupiedPoint-count
                    (guarantee-occupied (point-returner points x))) 1))
          (Game (Board
         (list-replace points x (OccupiedPoint 'Black 1))
         (- bb 1) (+ wb 1) bo wo)
        turn
                 (rm-list (abs (distance boardloc1 boardloc2)) moves))]
         [else
        (Game (Board
         (list-replace points x (point-add
                                 (point-returner points x) 1))
         (- bb 1) wb bo wo)
        turn
                 (rm-list (abs (distance boardloc1 boardloc2)) moves))])]
          [else (Game (Board
         (list-replace points x (point-add-empty
                                 (point-returner points x) 'Black))
         (- bb 1) wb bo wo)
          turn
                 (rm-list (abs (distance boardloc1 boardloc2)) moves))])
        (error "illegal move"))]
       [('WhiteBar (PointNum x))
        (if (and (<= 19 x 24) ;;can only move to oppenent's home board from
              (legal-move? game 'WhiteBar (PointNum x)))   ;; 'WhiteBar
        (cond
       [(not (emptypoint? (point-returner points x)))
        (cond
          [(and (symbol=? (what-color points x) 'Black)
                (> (OccupiedPoint-count
                    (guarantee-occupied (point-returner points x))) 1)) game]
          [(and (symbol=? (what-color points x) 'Black)
                (= (OccupiedPoint-count
                    (guarantee-occupied (point-returner points x))) 1))
        (Game (Board
         (list-replace points x (OccupiedPoint 'White 1))
         (+ bb 1) (- wb 1) bo wo)
        turn
                 (rm-list (abs (distance boardloc1 boardloc2)) moves))]
          [else
           (Game (Board
         (list-replace points x (point-add
                                 (point-returner points x) 1))
         bb (- wb 1) bo wo)
        turn
                 (rm-list (abs (distance boardloc1 boardloc2)) moves))])]
       [else (Game (Board
         (list-replace points x (point-add-empty
                                 (point-returner points x) 'White))
         bb (- wb 1) bo wo)
        turn
                 (rm-list (abs (distance boardloc1 boardloc2)) moves))])
       (error "illegal move"))]
       [(_ _) game])]))
;; This function produces a Board, so will not write check-expect test
;; (as specified in project guidelines)

;; Below, defining World struct with fields that hold the current Game and
;; Style, as well as other implementation needed to manage my Universe.
;; These include highlighted portion of board, and dice Integer values.
;; SUBJECT TO CHANGE (with more fields possibly being added) as my Universe code
;; comes to fruition.
(define-struct World
  ([state : Game]
   [style : Style]
   [highlighted? : ClickLoc]
   [whitedice1 : Integer]
   [whitedice2 : Integer]
   [blackdice1 : Integer]
   [blackdice2 : Integer]
   [history : (Listof Game)]))

(: updated-random : Integer -> Integer)
;; updated version of built-in random function that re-draws/re-rolls if the
;; return value is 0
(define (updated-random i)
  (local
    {(define randnum : Integer (random i))}
    (if (= randnum 0) (updated-random i) randnum)))
(check-within (updated-random 7) 4 3)
(check-within (updated-random 7) 3 3)

(: double-dice : Integer Integer Integer Player -> Image)
;; function that returns Image of generated dice face (between 1 and 6,
;; inclusive) of two die of the same color. To be overlayed over board Image.
;; if 0, then one part of the dice is empty image, i.e. for starting the game
(define (double-dice c-rad d1 d2 p)
  (match p
    ['Black
  (overlay/offset (draw-die-black c-rad d1)
       (* 4 c-rad) (* (/ 3 2) c-rad) (draw-die-black c-rad d2))]
    ['White
  (overlay/offset (draw-die-white c-rad d1)
       (* 4 c-rad) (* (/ 3 2) c-rad) (draw-die-white c-rad d2))]))
(double-dice 15 1 2 'Black)
(double-dice 30 3 4 'White)
(double-dice 24 5 6 'Black)
(double-dice 20 5 5 'White)

;; sample world initial-world is the starting board game state for when the game
;; starts, with all checkers in correct positions and starting with 'Black turn
(define initial-world : World (local
                                {(define b1 : Integer (updated-random 7))
                                 (define b2 : Integer (updated-random 7))}
  (World (Game starting-board 'Black (list b1 b2)) samplestyle1
                                     'Nowhere (updated-random 7)
  (updated-random 7) b1 b2 '())))

;; sample world solely for testing purposes
(define testing-world : World
  (World (Game starting-board 'Black (list 4 6)) samplestyle1
                                     'Nowhere 4 6 4 6 '()))

;; sample world solely for testing purposes
(define testing-world2 : World
  (World (Game starting-board 'Black (list 4 6)) samplestyle1
                                     'Nowhere 3 0 0 3 '()))
;; dice values different since moves history list is empty

(: distance : BoardLoc BoardLoc -> Integer)
;; takes in an origin and a destination, and returns the "distance" for that
;; move, i.e. the dice roll number (Integer) needed to move from the origin to
;; the destination.
;; non-negative for the black player and non-positive for the white player
(define (distance b1 b2)
  (match* (b1 b2)
    [((PointNum x) (PointNum y)) (- y x)]
    [('BlackBar (PointNum a)) (if (<= 1 a 6) a
                                  99)]
    [('WhiteBar (PointNum a)) (if (<= 19 a 24) (- 25 a)
                                  99)]
    [((PointNum a) 'BlackOff) (if (<= 19 a 24) (- 25 a)
                                  99)]
    [((PointNum a) 'WhiteOff) (if (<= 1 a 6) a
                                  99)]
    [(_ _) 99]))
(check-expect (distance (PointNum 3) (PointNum 4)) 1)
(check-expect (distance (PointNum 5) (PointNum 2)) -3)
(check-expect (distance 'BlackBar (PointNum 2)) 2)
(check-expect (distance 'BlackBar (PointNum 15)) 99)
(check-expect (distance 'WhiteBar (PointNum 23)) 2)
(check-expect (distance 'WhiteBar (PointNum 2)) 99)
(check-expect (distance (PointNum 5) 'WhiteOff) 5)
(check-expect (distance (PointNum 7) 'WhiteOff) 99)
(check-expect (distance (PointNum 23) 'BlackOff) 2)
(check-expect (distance (PointNum 2) 'BlackOff) 99)
(check-expect (distance 'Nowhere 'Nowhere) 99)

(: in-list? : Integer (Listof Integer) -> Boolean)
;; Is the input integer equal to ANY of the elements of the list?
(define (in-list? i xs)
  (ormap (lambda ([m : Integer]) (= i m)) xs))
(check-expect (in-list? 5 '()) #f)
(check-expect (in-list? 5 (list 1 2 3 4)) #f)
(check-expect (in-list? 5 (list 1 2 3 4 5)) #t)
(check-expect (in-list? 5 (list 1 2 3 4 5 6)) #t)
(check-expect (in-list? 5 (list 5 6 7 8 9)) #t)
(check-expect (in-list? 5 (list 6 7 8 9)) #f)

(: less-than-list? : Integer (Listof Integer) -> Boolean)
;; Is the input integer less than or equal to ANY of the elements of the list?
(define (less-than-list? i xs)
  (ormap (lambda ([m : Integer]) (<= i m)) xs))
(check-expect (less-than-list? 5 '()) #f)
(check-expect (less-than-list? 5 (list 1 2 3 4)) #f)
(check-expect (less-than-list? 5 (list 1 2 3 4 5)) #t)
(check-expect (less-than-list? 5 (list 1 2 3 4 5 6)) #t)
(check-expect (less-than-list? 5 (list 5 6 7 8 9)) #t)
(check-expect (less-than-list? 5 (list 6 7 8 9)) #t)

;; samplegame1 for testing (an input parameter for legal-move? function)
(define samplegame1 : Game
  (Game
  (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) (OccupiedPoint 'Black 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 0 0) 'Black (list 1 3)))

;; samplegame2 for testing (an input parameter for legal-move? function)
(define samplegame2 : Game
  (Game
  (Board
   (list (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5) 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) (OccupiedPoint 'Black 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 3 3 0 0) 'White (list 1 1)))

;; samplegame3 for testing (an input parameter for legal-move? function)
(define samplegame3 : Game
  (Game
  (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) (OccupiedPoint 'Black 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 3 3 0 0) 'Black (list 5 6)))

(: max-possible : (Listof Integer) (Listof Integer) -> Integer)
;; Returns the max possible value that can be utilized from a list of possible
;; distances that can be moved based on a set of available moves(Listof Integer)
(define (max-possible xs moves)
  (match xs
    ['() 0]
    [(cons hd tl) (if (less-than-list? hd moves)
                      (max hd (max-possible tl moves))
                      (max-possible tl moves))]))
(check-expect (max-possible '() (list 1 2 3)) 0)
(check-expect (max-possible (list 1 2) (list 1 2 3)) 2)
(check-expect (max-possible (list 3 4 5) (list 3 4 7)) 5)

(: legal-move? : Game BoardLoc BoardLoc -> Boolean)
;; takes in the current situation of the game and a proposed move and evaluates
;; whether the proposed move is legal or not (returns a Boolean). Takes the
;; rules into account, as well as dice values, to determine if a move is legal
;; and returns a Boolean #t (true) if legal or #f (false) if illegal.
(define (legal-move? g b1 b2)
  (if (= (distance b1 b2) 99) #f
  (match g
    [(Game (Board points bb wb bo wo) turn moves)
     (match* (b1 b2)
       [('BlackBar (PointNum a)) (if (and (> bb 0)
               (<= 1 a 6)) (in-list? (distance b1 b2) moves) #f)]
       [('WhiteBar (PointNum a)) (if (and (> wb 0)
               (<= 19 a 24)) (in-list? (abs (distance b1 b2)) moves) #f)]
       [((PointNum a) (PointNum b))
        (if (= a b) #f
        (if (symbol=? turn 'Black)
                (and (symbol=? (what-color points a) 'Black)
                     (or (emptypoint? (point-returner points b))
                         (symbol=? (what-color points b) 'Black)
                (and (symbol=? (what-color points b) 'White)
                     (= (OccupiedPoint-count (guarantee-occupied
                                           (point-returner points b))) 1)))
                     (in-list? (distance b1 b2) moves))
                (and (symbol=? (what-color points a) 'White)
                     (or (emptypoint? (point-returner points b))
                         (symbol=? (what-color points b) 'White)
                (and (symbol=? (what-color points b) 'Black)
                     (= (OccupiedPoint-count (guarantee-occupied
                                           (point-returner points b))) 1)))
                     (in-list? (abs (distance b1 b2)) moves))))]
       [((PointNum a) 'WhiteOff) (if
          (and (symbol=? (what-color points a) 'White) (<= 1 a 6)
          (less-than-list? (abs (distance b1 b2)) moves))
          (<= (abs (distance b1 b2)) (max-possible (list 1 2 3 4 5 6) moves))
          #f)]
       [((PointNum a) 'BlackOff) (if
          (and (symbol=? (what-color points a) 'Black) (<= 19 a 24)
          (less-than-list? (distance b1 b2) moves))
          (<= (abs (distance b1 b2)) (max-possible (list 1 2 3 4 5 6) moves))
          #f)]
       [(_ _) #f])])))
(check-expect (legal-move? samplegame1 (PointNum 1) (PointNum 14)) #f)
(check-expect (legal-move? samplegame1 (PointNum 13) (PointNum 15)) #f)
(check-expect (legal-move? samplegame1 (PointNum 13) (PointNum 16)) #t)
(check-expect (legal-move? samplegame2 (PointNum 15) (PointNum 17)) #f)
(check-expect (legal-move? samplegame2 (PointNum 15) (PointNum 16)) #t)
(check-expect (legal-move? samplegame2 (PointNum 13) (PointNum 14)) #f)
(check-expect (legal-move? samplegame1 (PointNum 13) (PointNum 14)) #t)
(check-expect (legal-move? samplegame3 'BlackBar (PointNum 14)) #f)
(check-expect (legal-move? samplegame3 'BlackBar (PointNum 14)) #f)
(check-expect (legal-move? samplegame3 'BlackBar (PointNum 1)) #f)
(check-expect (legal-move? samplegame3 'BlackBar (PointNum 5)) #t)
(check-expect (legal-move? samplegame3 'BlackBar (PointNum 6)) #t)
(check-expect (legal-move? samplegame2 (PointNum 5) 'WhiteOff) #f)
(check-expect (legal-move? samplegame2 (PointNum 5) 'BlackOff) #f)
(check-expect (legal-move? samplegame2 (PointNum 1) 'BlackOff) #f)
(check-expect (legal-move? samplegame2 (PointNum 1) 'WhiteOff) #t)
(check-expect (legal-move? samplegame2 'BlackBar 'BlackOff) #f)

(: clickloc->boardloc : ClickLoc -> BoardLoc)
;; Helper function that takes in a ClickLoc and returns the same things
;; as a BoardLoc, so long as ClickLoc is not 'WhiteDice or 'WhiteDice
(define (clickloc->boardloc t)
  (match t
    ['BlackDice (error "not a BoardLoc")]
    ['WhiteDice (error "not a BoardLoc")]
    ['BlackBar 'BlackBar]
    ['WhiteBar 'WhiteBar]
    ['BlackOff 'BlackOff]
    ['WhiteOff 'WhiteOff]
    ['Nowhere 'Nowhere]
    [(PointNum n) (PointNum n)]))
(check-expect (clickloc->boardloc 'BlackBar) 'BlackBar)
(check-expect (clickloc->boardloc 'WhiteBar) 'WhiteBar)
(check-expect (clickloc->boardloc 'BlackOff) 'BlackOff)
(check-expect (clickloc->boardloc 'WhiteOff) 'WhiteOff)
(check-expect (clickloc->boardloc (PointNum 5)) (PointNum 5))
(check-error (clickloc->boardloc 'BlackDice) "not a BoardLoc")
(check-error (clickloc->boardloc 'WhiteDice) "not a BoardLoc")

(: guarantee-symb : ClickLoc -> Symbol)
;; helper function to return typer Symbol version of a ClickLoc so that I can
;; make symbol=? comparisons in my click function.
(define (guarantee-symb c)
  (match c
    ['BlackDice 'BlackDice]
    ['WhiteDice 'WhiteDice]
    ['Nowhere 'Nowhere]
    [(PointNum _) 'PointNum]
    ['BlackBar 'BlackBar]
    ['WhiteBar 'WhiteBar]
    ['BlackOff 'BlackOff]
    ['WhiteOff 'WhiteOff]))
(check-expect (guarantee-symb 'WhiteDice) 'WhiteDice)
(check-expect (guarantee-symb 'BlackDice) 'BlackDice)
(check-expect (guarantee-symb 'Nowhere) 'Nowhere)
(check-expect (guarantee-symb (PointNum 5)) 'PointNum)
(check-expect (guarantee-symb 'BlackBar) 'BlackBar)
(check-expect (guarantee-symb 'WhiteBar) 'WhiteBar)
(check-expect (guarantee-symb 'BlackOff) 'BlackOff)
(check-expect (guarantee-symb 'WhiteOff) 'WhiteOff)

(: tick : World -> World)
;; tick function for ensuring that dice roll occurs once, then stops
(define (tick w)
  (local
       {(define bd1 : Integer (updated-random 7))
        (define bd2 : Integer (updated-random 7))
        (define wd1 : Integer (updated-random 7))
        (define wd2 : Integer (updated-random 7))}
  (match w
    [(World (Game board _ _) style 'BlackDice w1 w2 _ _ hist)
     (World (Game board 'Black
                  (if (= bd1 bd2) (list bd1 bd1 bd1 bd1) (list bd1 bd2)))
            style 'Nowhere w1 w2 bd1 bd2 hist)]
    [(World (Game board _ _) style 'WhiteDice _ _ b1 b2 hist)
     (World (Game board 'White
                  (if (= wd1 wd2) (list wd1 wd1 wd1 wd1) (list wd1 wd2)))
            style 'Nowhere wd1 wd2 b1 b2 hist)]
    [_ w])))

(: same-clickloc? : ClickLoc ClickLoc -> Boolean)
;; are the two inputted ClickLoc values (locations) equal?
(define (same-clickloc? c1 c2)
  (match* (c1 c2)
    [((PointNum a) (PointNum b)) (= a b)]
    [(_ _) (symbol=? (guarantee-symb c1) (guarantee-symb c2))]))
(check-expect (same-clickloc? (PointNum 1) (PointNum 2)) #f)
(check-expect (same-clickloc? (PointNum 1) (PointNum 1)) #t)
(check-expect (same-clickloc? (PointNum 1) 'BlackBar) #f)
(check-expect (same-clickloc? 'WhiteBar 'Nowhere) #f)
(check-expect (same-clickloc? 'BlackBar 'BlackBar) #t)

(: same-point? : Point Point -> Boolean)
;; are the two inputted points of type Point equal?
(define (same-point? p1 p2)
  (match* (p1 p2)
    [('EmptyPoint 'EmptyPoint) #t]
    [('EmptyPoint _) #f]
    [(_ 'EmptyPoint) #f]
    [((OccupiedPoint col1 num1) (OccupiedPoint col2 num2))
     (and (symbol=? col1 col2) (= num1 num2))]))
(check-expect (same-point? 'EmptyPoint 'EmptyPoint) #t)
(check-expect (same-point? 'EmptyPoint (OccupiedPoint 'Black 3)) #f)
(check-expect (same-point? (OccupiedPoint 'Black 3) 'EmptyPoint) #f)
(check-expect (same-point? (OccupiedPoint 'White 3) (OccupiedPoint 'Black 3))
              #f)
(check-expect (same-point? (OccupiedPoint 'Black 7) (OccupiedPoint 'Black 3))
              #f)
(check-expect (same-point? (OccupiedPoint 'Black 3) (OccupiedPoint 'Black 3))
              #t)

(: list-num : (Listof Point) Point -> Integer)
;; returns the index number (starting at 1) (with base 1)
;; for the element of a specified list of Points
(define (list-num xs i)
  (match xs
    ['() 0]
    [(cons hd tl) (if (same-point? hd i) 1 (+ 1 (list-num tl i)))]))
(check-expect (list-num '() 'EmptyPoint) 0)
(check-expect (list-num (list (OccupiedPoint 'Black 3) 'EmptyPoint)
                        (OccupiedPoint 'Black 3)) 1)
(check-expect (list-num (list 'EmptyPoint (OccupiedPoint 'Black 3))
                        (OccupiedPoint 'Black 3)) 2)

(: return-points : (Listof Point) Player Integer -> (Listof BoardLoc))
;; From a (Listof Point), returns a (Listof BoardLoc) of all points
;; sharing the same color of the input Player (U 'Black 'White)
(define (return-points points col i)
  (match points
    ['() '()]
    [(cons hd tl) (if (and (not (emptypoint? hd))
         (symbol=? (OccupiedPoint-color (guarantee-occupied hd)) col))
           (cons (PointNum (+ i (list-num points hd))) (return-points
                                                  tl col (+ i 1)))
           (return-points tl col (+ i 1)))]))
(check-expect (return-points '() 'Black 0) '())
(check-expect (return-points
               (list (OccupiedPoint 'Black 3) (OccupiedPoint 'White 5))
               'White 0) (list (PointNum 2)))
(check-expect (return-points
               (list 'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 3)
       (OccupiedPoint 'White 1) (OccupiedPoint 'Black 4) 'EmptyPoint) 'Black 0)
              (list (PointNum 3) (PointNum 5)))

(: point->boardloc : PointNum -> BoardLoc)
;; guarantee's that Point type is changed to type BoardLoc
(define (point->boardloc p) p)
(check-expect (point->boardloc (PointNum 5)) (PointNum 5))
(check-expect (point->boardloc (PointNum 6)) (PointNum 6))

(: any-avail-moves? : Game (Listof BoardLoc) (Listof BoardLoc) -> Boolean)
;; helper function that determines if there are ANY available moves between
;; list of origins and a list of possible destinations
(define (any-avail-moves? game origins dests)
  (match* (origins dests)
    [('() '()) #f]
    [('() _) #f]
    [(_ '()) #f]
    [((cons ohd otl) (cons dhd dtl))
     (or (any-avail-moves? game otl dests)
         (ormap (lambda ([i : BoardLoc]) (legal-move? game ohd i)) dests))]))
(check-expect (any-avail-moves? samplegame1 '() '()) #f)
(check-expect (any-avail-moves? samplegame1 '() (list 'BlackOff)) #f)
(check-expect (any-avail-moves? samplegame1 (list (PointNum 1)) '()) #f)
(check-expect (any-avail-moves? samplegame1 (list (PointNum 13))
                    (list (PointNum 13) 'BlackOff)) #f)
(check-expect (any-avail-moves? samplegame1 (list (PointNum 13))
                    (list (PointNum 13) (PointNum 14) 'BlackOff)) #t)

;; samplegame4 for testing (an input parameter for legal-move? function)
;; no available moves
(define samplegame4 : Game
  (Game
  (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) (OccupiedPoint 'White 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 0 0) 'Black (list 1 2)))

(: symbol->boardloc : Symbol -> BoardLoc)
;; guarantees that symbol is of type BoardLoc
(define (symbol->boardloc s)
  (match s
    ['BlackBar 'BlackBar]
    ['BlackOff 'BlackOff]
    ['WhiteBar 'WhiteBar]
    ['WhiteOff 'WhiteOff]
    ['Nowhere 'Nowhere]
    [_ (error "not a BoardLoc")]))
(check-error (symbol->boardloc 'hi) "not a BoardLoc")
(check-expect (symbol->boardloc 'BlackBar) 'BlackBar)
(check-expect (symbol->boardloc 'BlackOff) 'BlackOff)
(check-expect (symbol->boardloc 'WhiteOff) 'WhiteOff)
(check-expect (symbol->boardloc 'WhiteBar) 'WhiteBar)
(check-expect (symbol->boardloc 'Nowhere) 'Nowhere)

(: pointnums->boardlocs : (Listof PointNum) -> (Listof BoardLoc))
;; takes in a (Listof PointNum) and converts it to a (Listof BoardLoc)
(define (pointnums->boardlocs xs)
  (match xs
    ['() '()]
    [(cons hd tl) (cons (point->boardloc hd) (pointnums->boardlocs tl))]))
(check-expect (pointnums->boardlocs '()) '())
(check-expect (pointnums->boardlocs (list (PointNum 5))) (list (PointNum 5)))
(check-expect (pointnums->boardlocs (list (PointNum 5) (PointNum 6)))
              (list (PointNum 5) (PointNum 6)))

;; defining the list of PointNums (Listof PointNum) 1-24:
(define pointnums : (Listof PointNum)
  (list (PointNum 1) (PointNum 2) (PointNum 3) (PointNum 4) (PointNum 5)
        (PointNum 6) (PointNum 7) (PointNum 8) (PointNum 9) (PointNum 10)
        (PointNum 11) (PointNum 12) (PointNum 13) (PointNum 14) (PointNum 15)
        (PointNum 16) (PointNum 17) (PointNum 18) (PointNum 19) (PointNum 20)
        (PointNum 21) (PointNum 22) (PointNum 23) (PointNum 24)))

(: available-moves? : Game -> Boolean)
;; Are there any possible available moves for a player (given their turn)
;; for the current state of the game? Returns a Boolean #t if true
;; and #f if false.
(define (available-moves? game)
  (match game
    [(Game (Board points bb wb bo wo) turn moves)
     (cond
       [(empty? moves) #f]
       [(symbol=? turn 'Black)
        (any-avail-moves? game
     (append (return-points points 'Black 0) (list (symbol->boardloc 'BlackBar)))
        (cons (symbol->boardloc 'BlackBar)
                    (cons (symbol->boardloc 'BlackOff)
                          (pointnums->boardlocs pointnums))))]
       [(symbol=? turn 'White)
        (any-avail-moves? game
     (append (return-points points 'White 0) (list (symbol->boardloc 'WhiteBar)))
        (cons (symbol->boardloc 'WhiteBar)
                    (cons (symbol->boardloc 'WhiteOff)
                          (pointnums->boardlocs pointnums))))])]))
(check-expect (available-moves? samplegame1) #t)
(check-expect (available-moves? samplegame2) #t)
(check-expect (available-moves? samplegame3) #t)
(check-expect (available-moves? samplegame4) #f)

(: opp-turn : Player -> Player)
;; helper function that returns the opposing player's turn
;; i.e. (opp-turn 'Black) -> 'White
;; and (opp-turn 'White) -> 'Black
(define (opp-turn turn)
  (if (symbol=? turn 'Black) 'White 'Black))
(check-expect (opp-turn 'Black) 'White)
(check-expect (opp-turn 'White) 'Black)

(: click : World Integer Integer Mouse-Event -> World)
;; My click-handling function called click handles mouse clicks (Mouse-Event)
;; such that the user can click to move checkers around the Backgammon board
;; as desired. Illegitimate moves raise an error. Click 1 selects a checker
;; from a point or bar to be moved, and click 2 selects where that checker goes
;; (where it's moved to), whether it be another point or 'BlackOff or 'WhiteOff
(define (click w x y e)
  (match* (w e)
    [((World (Game board turn moves) style h? w1 w2 b1 b2 hist) "button-down")
     (if (game-over? (Game board turn moves)) w
     (match h?
           ['Nowhere
        (match (click-where style x y)
           [(PointNum p)
            (if (and (symbol=? turn (what-color (Board-points board) p))
                     (available-moves? (Game board turn moves)))
            (World (Game board turn moves) style (PointNum p) w1 w2 b1 b2 hist)
            w)]
           ['BlackBar (if (and (symbol=? turn 'Black)
                               (available-moves? (Game board turn moves)))
            (World (Game board turn moves) style 'BlackBar w1 w2 b1 b2 hist)
            w)]
           ['WhiteBar (if (and (symbol=? turn 'White)
                               (available-moves? (Game board turn moves)))
            (World (Game board turn moves) style 'WhiteBar w1 w2 b1 b2 hist)
            w)]
           ['BlackDice (World (Game board turn moves) style
                  (cond
                    [(and (symbol=? turn 'White)
                          (not (available-moves? (Game board turn moves))))
                     'BlackDice]
                    [else 'Nowhere])
                              w1 w2 b1 b2
                              (if (and (symbol=? turn 'White)
                          (not (available-moves? (Game board turn moves))))
                                  (cons (Game board turn moves) hist) hist))]
           ['WhiteDice (World (Game board turn moves) style
                  (cond
                    [(and (symbol=? turn 'Black)
                          (not (available-moves? (Game board turn moves))))
                     'WhiteDice]
                    [else 'Nowhere])
                              w1 w2 b1 b2
                              (if (and (symbol=? turn 'Black)
                          (not (available-moves? (Game board turn moves))))
                                  (cons (Game board turn moves) hist) hist))]
           [_ w])]
           [(PointNum p)
            (cond
              [(same-clickloc? (click-where style x y) (PointNum p))
               (World (Game board turn moves) style 'Nowhere w1 w2 b1 b2 hist)]
              [(and (not (= 0 (Board-black-bar board)))
                    (symbol=? (what-color (Board-points board) p) 'Black)) w]
              [(and (not (= 0 (Board-white-bar board)))
                    (symbol=? (what-color (Board-points board) p) 'White)) w]
             [(and (symbol=? (guarantee-symb (click-where style x y)) 'WhiteOff)
                   (symbol=? (what-color (Board-points board) p) 'White)
                   (symbol=? turn 'White)
                   (legal-move? (Game board turn moves) (PointNum p)
                             (clickloc->boardloc (click-where style x y)))
                   (<= 1 p 6))
              (World (apply-move (Game board turn moves) (PointNum p)
                                 (clickloc->boardloc (click-where style x y)))
                     style 'Nowhere w1 w2 b1 b2
                     (cons (Game board turn moves) hist))]
             [(and (symbol=? (guarantee-symb (click-where style x y)) 'WhiteOff)
                   (symbol=? (what-color (Board-points board) p) 'White)
                   (symbol=? turn 'White)
                   (not (legal-move? (Game board turn moves) (PointNum p)
                             (clickloc->boardloc (click-where style x y))))
                   (<= 1 p 6)) w]
             [(and (symbol=? (guarantee-symb (click-where style x y)) 'BlackOff)
                   (symbol=? (what-color (Board-points board) p) 'Black)
                   (symbol=? turn 'Black)
                   (legal-move? (Game board turn moves) (PointNum p)
                             (clickloc->boardloc (click-where style x y)))
                   (<= 19 p 24))
              (World (apply-move (Game board turn moves) (PointNum p)
                                 (clickloc->boardloc (click-where style x y)))
                     style 'Nowhere w1 w2 b1 b2
                     (cons (Game board turn moves) hist))]
             [(and (symbol=? (guarantee-symb (click-where style x y)) 'BlackOff)
                   (symbol=? (what-color (Board-points board) p) 'Black)
                   (symbol=? turn 'Black)
                   (not (legal-move? (Game board turn moves) (PointNum p)
                             (clickloc->boardloc (click-where style x y))))
                   (<= 19 p 24)) w]
             [(symbol=? (guarantee-symb (click-where style x y)) 'BlackDice)
               (World (Game board turn '()) style (PointNum p) w1 w2 b1 b2
                      hist)]
             [(symbol=? (guarantee-symb (click-where style x y)) 'WhiteDice)
               (World (Game board turn '()) style (PointNum p) w1 w2 b1 b2
                      hist)]
             [(= (distance (PointNum p)
                               (clickloc->boardloc (click-where style x y))) 99)
                w]
        [else (if (or (and (symbol=? (what-color (Board-points board) p) 'White)
                     (<= (distance (PointNum p)
                               (clickloc->boardloc (click-where style x y))) 0))
                    (and (symbol=? (what-color (Board-points board) p) 'Black)
                     (>= (distance (PointNum p)
                             (clickloc->boardloc (click-where style x y))) 0)))
            (cond
             [(or (symbol=? (guarantee-symb (click-where style x y)) 'BlackDice)
                 (symbol=? (guarantee-symb (click-where style x y)) 'WhiteDice))
             w]
             [(same-clickloc? (PointNum p) (click-where style x y))
              (World (Game board turn moves) style 'Nowhere w1 w2 b1 b2 hist)]
            [else (if (legal-move? (Game board turn moves) (PointNum p)
                             (clickloc->boardloc (click-where style x y)))
            (cond
              [(symbol=? (guarantee-symb (click-where style x y)) 'BlackDice)
               (World (Game board 'Black moves) style (PointNum p) w1 w2 b1 b2
                      hist)]
              [(symbol=? (guarantee-symb (click-where style x y)) 'WhiteDice)
               (World (Game board 'White moves) style (PointNum p) w1 w2 b1 b2
                      hist)]
              [else
           (World (apply-move (Game board turn moves) (PointNum p)
                              (clickloc->boardloc
                               (click-where style x y)))
                    style 'Nowhere w1 w2 b1 b2
                    (cons (Game board turn moves) hist))])
            w)]) w)])]
           ['BlackBar
            (cond
              [(symbol=? (guarantee-symb (click-where style x y)) 'BlackBar)
               (World (Game board turn moves) style 'Nowhere w1 w2 b1 b2 hist)]
              [(symbol=? (guarantee-symb (click-where style x y)) 'BlackDice)
               (World (Game board turn moves) style 'BlackBar w1 w2 b1 b2 hist)]
              [(symbol=? (guarantee-symb (click-where style x y)) 'WhiteDice)
               (World (Game board turn moves) style 'BlackBar w1 w2 b1 b2 hist)]
              [(symbol=? (guarantee-symb (click-where style x y)) 'Nowhere)
               (World (Game board turn moves) style 'BlackBar w1 w2 b1 b2 hist)]
            [(= (distance 'BlackBar
                               (clickloc->boardloc (click-where style x y))) 99)
                w]
            [else (cond
             [(or (symbol=? (guarantee-symb (click-where style x y)) 'BlackDice)
                 (symbol=? (guarantee-symb (click-where style x y)) 'WhiteDice))
             w]
             [(same-clickloc? 'BlackBar (click-where style x y))
              (World (Game board turn moves) style 'Nowhere w1 w2 b1 b2 hist)]
            [else (if (legal-move? (Game board turn moves) 'BlackBar
                             (clickloc->boardloc (click-where style x y)))
            (cond
              [(symbol=? (guarantee-symb (click-where style x y)) 'BlackDice)
               (World (Game board 'Black moves) style 'BlackBar
                      w1 w2 b1 b2 hist)]
              [(symbol=? (guarantee-symb (click-where style x y)) 'WhiteDice)
               (World (Game board 'Black moves) style 'BlackBar
                      w1 w2 b1 b2 hist)]
              [else
               (World (apply-move (Game board turn moves) 'BlackBar
                                  (clickloc->boardloc
                                   (click-where style x y)))
                    style 'Nowhere
                              w1 w2 b1 b2 hist)]) w)])])]
           ['WhiteBar
            (cond
              [(symbol=? (guarantee-symb (click-where style x y)) 'WhiteBar)
               (World (Game board turn moves) style 'Nowhere w1 w2 b1 b2 hist)]
              [(symbol=? (guarantee-symb (click-where style x y)) 'BlackDice)
               (World (Game board turn moves) style 'WhiteBar w1 w2 b1 b2 hist)]
              [(symbol=? (guarantee-symb (click-where style x y)) 'WhiteDice)
               (World (Game board turn moves) style 'WhiteBar w1 w2 b1 b2 hist)]
              [(symbol=? (guarantee-symb (click-where style x y)) 'Nowhere)
               (World (Game board turn moves) style 'WhiteBar w1 w2 b1 b2 hist)]
            [(= (distance 'WhiteBar
                               (clickloc->boardloc (click-where style x y))) 99)
                w]
            [else (cond
             [(or (symbol=? (guarantee-symb (click-where style x y)) 'BlackDice)
                 (symbol=? (guarantee-symb (click-where style x y)) 'WhiteDice))
             w]
             [(same-clickloc? 'WhiteBar (click-where style x y))
              (World (Game board turn moves) style 'Nowhere w1 w2 b1 b2 hist)]
            [else (if (legal-move? (Game board turn moves) 'WhiteBar
                             (clickloc->boardloc (click-where style x y)))
            (cond
              [(symbol=? (guarantee-symb (click-where style x y)) 'BlackDice)
               (World (Game board 'White moves) style 'WhiteBar w1 w2 b1 b2 hist)]
              [(symbol=? (guarantee-symb (click-where style x y)) 'WhiteDice)
               (World (Game board 'White moves) style 'WhiteBar w1 w2 b1 b2 hist)]
              [else
               (World (apply-move (Game board turn moves) 'WhiteBar
                                  (clickloc->boardloc
                                   (click-where style x y)))
                       style 'Nowhere w1 w2 b1 b2
                       (cons (Game board turn moves) hist))]) w)])])]
          [_ (World (Game board turn moves) style 'Nowhere w1 w2 b1 b2 hist)]))]
    ;; clicking elsewere unhighlights the board ^^
    [(_ _) w]))
;; No check-expects for functions that produce Worlds

;; samplegame5 for testing (an input parameter for game-over? function)
;; (also for winner function)
;; game is over (#t)
(define samplegame5 : Game
  (Game
  (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) (OccupiedPoint 'White 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 15 0) 'Black (list 1 2)))

;; samplegame6 for testing (an input parameter for game-over? function)
;; (also for winner function)
;; game is over (#t)
(define samplegame6 : Game
  (Game
  (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) (OccupiedPoint 'White 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 0 15) 'Black (list 1 2)))

(: game-over? : Game -> Boolean)
;; Determines if a Game is over based on input Game and returns a Boolean,
;; #t (true) if game is over, #f (false) if game is not yet over.
;; Game is over once a player successfully bears off all 15 checkers.
(define (game-over? g)
  (match g
    [(Game board turn moves) (or (= (Board-black-off board) 15)
                                 (= (Board-white-off board) 15))]))
(check-expect (game-over? samplegame1) #f)
(check-expect (game-over? samplegame2) #f)
(check-expect (game-over? samplegame3) #f)
(check-expect (game-over? samplegame4) #f)
(check-expect (game-over? samplegame5) #t)
(check-expect (game-over? samplegame6) #t)

(: winner : Game -> Player)
;; Determines the winner of a completed game. Raises an error if called on
;; an incomplete game.
(define (winner g)
  (if (not (game-over? g)) (error "Game is incomplete")
  (match g
    [(Game board turn moves)
     (if (= (Board-black-off board) 15) 'Black 'White)])))
(check-error (winner samplegame1) "Game is incomplete")
(check-error (winner samplegame2) "Game is incomplete")
(check-error (winner samplegame3) "Game is incomplete")
(check-error (winner samplegame4) "Game is incomplete")
(check-expect (winner samplegame5) 'Black)
(check-expect (winner samplegame6) 'White)

(: display-winner : Integer Player -> Image)
;; Image to overlay over a Board with a completed Game.
;; Will overlay Image of a text stating which Player won the game.
;; Input Integer is checker-radius.
(define (display-winner i p)
  (above
  (text (symbol->string p)
        (guarantee-byte (* 6 i)) 'green)
  (rectangle 0 i 'outline 'rosybrown)
  (text "Wins!!!"
        (guarantee-byte (* 6 i)) 'green)))
;; eyeball tests
(display-winner 5 'Black)
(display-winner 9 'White)

(: re-roll : Integer Integer -> Integer)
;; re-rolls if two Integers are equal
(define (re-roll i1 i2)
  (if (not (= i1 i2)) i2 (re-roll i1 (updated-random 7))))
(check-expect (re-roll 5 6) 6)
(check-within (re-roll 5 5) 4 3)
(check-within (re-roll 5 5) 3 3)

(: not-equals : Integer -> (Listof Integer))
;; The input parameter does nothing. Its sole purposes is to ensure not-equals
;; is a function that can be recalled (thus producing new dice values
;; when the board is reset.
(define (not-equals i)
  (local
    {(define b1 : Integer (updated-random 7))
     (define w1 : Integer (re-roll b1 (updated-random 7)))}
    (list b1 w1)))

(: point-diff : (Listof Point) (Listof Point) Integer -> (Listof BoardLoc))
;; Returns difference between two lists of points in the form of
;; a BoardLoc of the location of the particular point that is different
;; between the two lists. Helper function for whats-different?
;; Assumes a singular difference between the two list of points
(define (point-diff ps1 ps2 i)
  (match* (ps1 ps2)
    [('() '()) '()] ;; i.e. no difference
    [((cons hd1 tl1) (cons hd2 tl2))
     (if (not (same-point? hd1 hd2)) (cons (PointNum i)
                                           (point-diff tl1 tl2 (add1 i)))
         (point-diff tl1 tl2 (add1 i)))]
    [(_ _) (error "list of points is in incorrect format")]))
(check-error (point-diff (list 'EmptyPoint) '() 1)
             "list of points is in incorrect format")
(check-expect (point-diff (list 'EmptyPoint (OccupiedPoint 'Black 3))
                     (list (OccupiedPoint 'White 5) (OccupiedPoint 'Black 3)) 1)
              (list (PointNum 1)))
(check-expect (point-diff (list 'EmptyPoint (OccupiedPoint 'Black 4))
                     (list 'EmptyPoint (OccupiedPoint 'Black 3)) 1)
              (list (PointNum 2)))

;; sample boards for testing purposes, below:
(define testboard1 : Board
  (Board
   (list 'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
         'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) 'EmptyPoint
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 0 0))
(define testboard2 : Board
  (Board
   (list (OccupiedPoint 'Black 3) 'EmptyPoint (OccupiedPoint 'Black 5)
         'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) 'EmptyPoint
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 1 0))
(define testboard3 : Board
  (Board
   (list 'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
         'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) 'EmptyPoint
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 0 0))
(define testboard4 : Board
  (Board
   (list (OccupiedPoint 'Black 3) 
         'EmptyPoint (OccupiedPoint 'White 7)
         'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) 'EmptyPoint
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 0 0))

(: whats-different? : Board Board -> (Listof BoardLoc))
;; Returns the locations of (singular) difference between two games
(define (whats-different? prior later)
  (match* (prior later)
    [((Board points1 bb1 wb1 bo1 wo1) (Board points2 bb2 wb2 bo2 wo2))
     (cond
       [(< bb2 bb1)
        (append (point-diff points1 points2 1) (list 'BlackBar))]
       [(< wb2 wb1)
        (append (point-diff points1 points2 1) (list 'WhiteBar))]
       [(not (= bo1 bo2))
        (append (point-diff points1 points2 1) (list 'BlackOff))]
       [(not (= wo1 wo2))
        (append (point-diff points1 points2 1) (list 'WhiteOff))]
       [else (point-diff points1 points2 1)])]))
(check-expect (whats-different? testboard1 testboard2)
              (list (PointNum 1) 'BlackOff))
(check-expect (whats-different? testboard3 testboard4)
              (list (PointNum 1) (PointNum 3)))

;; samplegamee for testing
(define samplegamee : Game
  (Game
  (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) (OccupiedPoint 'Black 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 0 0) 'Black (list 1 3)))
;; samplegameee for testing
(define samplegameee : Game
  (Game
  (Board
   (list (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint 'EmptyPoint
         (OccupiedPoint 'White 6)
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) (OccupiedPoint 'Black 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 0 0) 'Black (list 1 3)))
;; samplegamex for testing
(define samplegamex : Game
  (Game
  (Board
   (list (OccupiedPoint 'White 1)
    'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) (OccupiedPoint 'Black 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 0 0) 'White (list 1 3)))
;; samplegamey for testing
(define samplegamey : Game
  (Game
  (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint ;; points 1-12
         (OccupiedPoint 'Black 2) (OccupiedPoint 'Black 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 1)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint) 0 0 0 1) 'White (list 1 3)))

(: inference : Game Game -> Integer)
;; inference of dice used dice value based on two Games
;; assumes list of differences contains only two BoardLocs
;; Helper function for my key function (for "undo" functionality)
(define (inference g1 g2)
  (match* (g1 g2) ;; format: (before-move, aftermove)
    [((Game board1 t1 m1) (Game board2 t2 m2))
     (match (whats-different? board1 board2)
       ['() -1] ;; no board change means turn was changed
       [(cons loc1 (cons loc2 '())) (abs (distance loc1 loc2))]
       [_ 0])]))
(check-expect (inference samplegamee samplegameee) 4)
(check-expect (inference samplegamex samplegamey) 1)

;; convert a text representation of an Integer to an Integer
;; raise an error if the string is not a number
;; return the integer part of the resulting number only
;; (this is intended only to be used with integers)
;; Given by Professor Wachs and Professor Shaw.
(: string->integer : String -> Integer)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) (exact-round (real-part conv))
      (error "string->integer: invalid integer"))))
(check-expect (string->integer "5") 5)
(check-expect (string->integer "79") 79)
(check-error (string->integer "w") "string->integer: invalid integer")

(: point->string : Point -> String)
;; serialization of Point data structure into a string representation
;; of that data
(define (point->string p)
  (match p
    ['EmptyPoint "_"]
    [(OccupiedPoint color count)
     (if (symbol=? color 'Black)
         (string-append "B" (number->string count))
         (string-append "W" (number->string count)))]))
(check-expect (point->string 'EmptyPoint) "_")
(check-expect (point->string (OccupiedPoint 'Black 3)) "B3")
(check-expect (point->string (OccupiedPoint 'White 7)) "W7")

(: char->string : Char -> String)
;; converts a character (type Char) into a string (type String)
;; assumes single character is given as input
(define (char->string c)
  (list->string (list c)))
(check-expect (char->string #\!) "!")
(check-expect (char->string #\n) "n")

(: string->point : String -> Point)
;; Inverse of point->string. Takes a serialization (string) and returns
;; a point from it.
(define (string->point str)
  (local
    {(define strlist : (Listof Char) (string->list str))}
    (match strlist
      ['() (error "not a point")]
      [(cons hd tl)
       (cond
         [(not (or (= (length strlist) 2) (= (length strlist) 1)))
          (error "not a point")]
         [(and (empty? tl) (char=? hd #\_)) 'EmptyPoint]
         [(and (char=? #\B hd)
               (integer? (string->integer (char->string (first tl)))))
          (OccupiedPoint 'Black (string->integer (char->string (first tl))))]
         [(and (char=? #\W hd)
               (integer? (string->integer (char->string (first tl)))))
          (OccupiedPoint 'White (string->integer (char->string (first tl))))]
         [else (error "not a point")])])))
(check-expect (string->point "B3") (OccupiedPoint 'Black 3))
(check-expect (string->point "W7") (OccupiedPoint 'White 7))
(check-expect (string->point "_") 'EmptyPoint)
(check-error (string->point "sdfsaf") "not a point")
(check-error (string->point "23234") "not a point")

(: pointslist->string : (Listof Point) -> String)
;; serialization of a (Listof Point) within a Board struct to a String
(define (pointslist->string ps)
  (match ps
    ['() ""]
    [(cons hd '()) (point->string hd)]
    [(cons hd tl)
     (string-append (point->string hd) " " (pointslist->string tl))]))
(check-expect (pointslist->string '()) "")
(check-expect (pointslist->string (list 'EmptyPoint)) "_")
(check-expect (pointslist->string
               (list (OccupiedPoint 'Black 3))) "B3")
(check-expect (pointslist->string
    (list 'EmptyPoint (OccupiedPoint 'White 7) (OccupiedPoint 'Black 3)
          'EmptyPoint)) "_ W7 B3 _")
(check-expect (pointslist->string
    (list (OccupiedPoint 'White 7) 'EmptyPoint
               'EmptyPoint (OccupiedPoint 'Black 3))) "W7 _ _ B3")

(: stringer : (Listof String) -> String)
;; helper that concatenates Strings together in correct format, i.e.
;; with spaces between each (from a (Listof String)), with no space
;; before first or after last elements
(define (stringer xs)
  (match xs
    ['() ""]
    [(cons hd '()) hd]
    [(cons hd tl) (string-append hd " " (stringer tl))]))
(check-expect (stringer '()) "")
(check-expect (stringer (list "hi")) "hi")
(check-expect (stringer (list "a" "bb" "c")) "a bb c")

(: string->pointslist : String -> (Listof Point))
;; The deserialization function of pointslist->string (inverse)
(define (string->pointslist str)
  (match (string-split str " ")
    ['() '()]
    [(cons hd tl) (cons (string->point hd) (string->pointslist
                (stringer tl)))]))
(check-expect (string->pointslist "") '())
(check-expect (string->pointslist "_ W7 B3 _")
         (list 'EmptyPoint (OccupiedPoint 'White 7) (OccupiedPoint 'Black 3)
          'EmptyPoint))
(check-expect (string->pointslist "W7 _ _ B3")
         (list (OccupiedPoint 'White 7) 'EmptyPoint
               'EmptyPoint (OccupiedPoint 'Black 3)))

(: board->string : Board -> String)
;; serialization function that takes in a Board and returns a String
;; serializes Board data structure into a String
(define (board->string b)
  (match b
    [(Board points bb wb bo wo)
     (string-append (pointslist->string points) "|"
                 (number->string bb) "|" (number->string wb) "|"
                 (number->string bo) "|" (number->string wo))]))
(check-expect (board->string sampleboard1)
              "_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ B5 B5 B5 W5 W5 W5|0|0|0|0")
(check-expect (board->string sampleboard2)
              "_ _ _ _ _ _ _ _ _ _ _ _ B2 B5 B5 W2 W5 W5 _ _ _ _ _ _|3|3|0|0")

(: string->board : String -> Board)
;; deserialization function that converts a String into a Board data structure
(define (string->board str)
  (match (string-split str "|")
    ['() (error "Board serialization is invalid, perhaps in wrong format")]
    [(cons hd tl)
     (if (= (length tl) 4)
      (match tl
       [(cons bb (cons wb (cons bo (cons wo '()))))
            (Board (string->pointslist hd) (string->integer bb)
            (string->integer wb) (string->integer bo) (string->integer wo))]
       [_ (error "Board serialization is invalid, perhaps in wrong format")])
      (error "Board serialization is invalid, perhaps in wrong format"))]))
(check-error (string->board "")
             "Board serialization is invalid, perhaps in wrong format")
(check-error (string->board "hellosdfiw|swo9e")
             "Board serialization is invalid, perhaps in wrong format")
(check-expect (string->board 
               "_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ B5 B5 B5 W5 W5 W5|0|0|0|0")
               sampleboard1)
(check-expect (string->board 
               "_ _ _ _ _ _ _ _ _ _ _ _ B2 B5 B5 W2 W5 W5 _ _ _ _ _ _|3|3|0|0")
               sampleboard2)

(: integerlist->string : (Listof Integer) -> String)
;; helper function that takes a list of Integers and returns
;; a String representation of that list, with spaces between each Integer,
;; but not before the first or after the last
(define (integerlist->string xs)
  (match xs
    ['() ""]
    [(cons hd '()) (number->string hd)]
    [(cons hd tl) (string-append (number->string hd) " "
                                 (integerlist->string tl))]))
(check-expect (integerlist->string '()) "")
(check-expect (integerlist->string (list 5)) "5")
(check-expect (integerlist->string (list 1 2)) "1 2")
(check-expect (integerlist->string (list 1 2 3)) "1 2 3")
(check-expect (integerlist->string (list 1 2 3 4)) "1 2 3 4")

(: string->integerlist : String -> (Listof Integer))
;; inverse function of integerlist->string
;; assumes input string is of the valid format, i.e. sequence of numbers
;; separated by a space, except before the first number and after the last
(define (string->integerlist str)
  (match (string-split str " ")
    ['() '()]
    [(cons hd '()) (list (string->integer hd))]
    [(cons hd tl) (cons (string->integer hd) (string->integerlist
                  (stringer tl)))]))
(check-expect (string->integerlist "") '())
(check-expect (string->integerlist "5") (list 5))
(check-expect (string->integerlist "1 2") (list 1 2))
(check-expect (string->integerlist "1 2 3") (list 1 2 3))
(check-expect (string->integerlist "1 2 3 4") (list 1 2 3 4))

(: game->string : Game -> String)
;; serialization function that takes in a Game and returns a String
(define (game->string g)
  (match g
    [(Game board turn moves)
     (string-append (board->string board) "@"
                    (if (symbol=? turn 'Black) "B" "W") "@"
                    (integerlist->string moves))]))
(check-expect (game->string samplegame1)
   "_ _ _ _ _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|0|0|0|0@B@1 3")
(check-expect (game->string samplegame2)
   "W5 W1 W5 W5 _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|3|3|0|0@W@1 1")

(: string->game : String -> Game)
;; deserialization function for returning a Game data structure from
;; an input String. Input for string->game should be valid.
(define (string->game str)
  (match (string-split str "@")
    ['() (error "Game serialization is invalid, perhaps in wrong format")]
    [(cons hd tl)
     (cond
       [(= (length (string-split str "@")) 3)
     (Game (string->board hd) (if (string=? (first tl) "B") 'Black 'White)
           (string->integerlist (first (rest tl))))]
       [(= (length (string-split str "@")) 2)
     (Game (string->board hd) (if (string=? (first tl) "B") 'Black 'White)
           '())]
       [else
        (error "Game serialization is invalid, perhaps in wrong format")])]))
(check-error (string->game "")
             "Game serialization is invalid, perhaps in wrong format")
(check-error (string->game "d@s@d2@sd@sdf@sd@sd@a")
             "Game serialization is invalid, perhaps in wrong format")
(check-expect (string->game
   "_ _ _ _ _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|0|0|0|0@B@1 3")
   samplegame1)
(check-expect (string->game
   "W5 W1 W5 W5 _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|3|3|0|0@W@1 1")
   samplegame2)

(: historylist->string : (Listof Game) -> String)
;; serialization function of history list of Game states into String format
(define (historylist->string games)
  (match games
    ['() ""]
    [(cons hd '()) (game->string hd)]
    [(cons hd tl)
     (string-append (game->string hd) "!" (historylist->string tl))]))
(check-expect (historylist->string (list samplegame1 samplegame2))
              (string-append
     "_ _ _ _ _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|0|0|0|0@B@1 3"
     "!"
     "W5 W1 W5 W5 _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|3|3|0|0@W@1 1"))
(check-expect (historylist->string (list samplegame2 samplegame1))
              (string-append
     "W5 W1 W5 W5 _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|3|3|0|0@W@1 1"
     "!"
     "_ _ _ _ _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|0|0|0|0@B@1 3"))

(: str->hist-helper : (Listof String) -> (Listof Game))
;; helper for string->historylist that takes a list of strings
;; where each element is a serialization of a game, and returns
;; a list of Games (Listof Game) from it
(define (str->hist-helper ls)
  (match ls
    ['() '()]
    [(cons hd tl) (cons (string->game hd) (str->hist-helper tl))]))
(check-expect (str->hist-helper '()) '())
(check-expect (str->hist-helper
   (list "_ _ _ _ _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|0|0|0|0@B@1 3"
    "W5 W1 W5 W5 _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|3|3|0|0@W@1 1"))
              (list samplegame1 samplegame2))
(check-expect (str->hist-helper
 (list "W5 W1 W5 W5 _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|3|3|0|0@W@1 1"
    "_ _ _ _ _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|0|0|0|0@B@1 3"))
              (list samplegame2 samplegame1))

(: string->historylist : String -> (Listof Game))
;; deserializes input String into a history list of type (Listof Game)
;; wrapper function for the helper function str->hist-helper
(define (string->historylist str)
  (str->hist-helper (string-split str "!")))
(check-expect (string->historylist "") '())
(check-expect (string->historylist
               (string-append
     "_ _ _ _ _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|0|0|0|0@B@1 3"
     "!"
     "W5 W1 W5 W5 _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|3|3|0|0@W@1 1"))
               (list samplegame1 samplegame2))
(check-expect (string->historylist
               (string-append
     "W5 W1 W5 W5 _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|3|3|0|0@W@1 1"
     "!"
     "_ _ _ _ _ _ _ _ _ _ _ _ B2 B5 W5 W1 W5 W5 _ _ _ _ _ _|0|0|0|0@B@1 3"))
               (list samplegame2 samplegame1))

;; for testing purposes
(define empty-board : Board
  (Board (list
   'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
        'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint)
  1 2 3 4))

(: world->string : World -> String)
;; serialization of a World data structure to a String
;; representing the history list of Game states,
;; with the current Game state prepended to it.
(define (world->string w)
  (match w
    [(World game _ _ _ _ _ _ hist)
     (if (not (empty? hist))
     (string-append (historylist->string (list game))
                    "!" (historylist->string hist))
     (historylist->string (list game)))]))
(check-expect (world->string testing-world)
              (string-append (board->string starting-board) "@B@4 6"))
(check-expect (world->string
               (World (Game empty-board 'Black (list 1 2)) samplestyle1 'Nowhere
                      1 2 3 4 '()))
   "_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _|1|2|3|4@B@1 2")

;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
;; given by Professor Wachs and Professor Shaw.
(: save-game! : World -> Void)
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
      (begin
        (write-string (world->string w)
          (open-output-file path))
        (void))
      (void))))
;; no check-expect tests: doesn't return anything (Void)

(: save-world! : World -> Void)
;; renaming of the save-game! function such that save-world! simply calls
;; save-game!
(define (save-world! w) (save-game! w))
;; no check-expect tests: doesn't return anything (Void)

(: change-in-color : Game Game -> Player)
;; Returns the Player 'Black or 'White depending on the differences
;; between two Game states, i.e. a move was made (inference)
(define (change-in-color g1 g2)
  (match* (g1 g2)
    [((Game _ turn1 _) (Game _ turn2 _))
     (if (symbol=? turn1 turn2) turn1 turn2)]))
(check-expect (change-in-color (Game sampleboard1 'Black '())
                               (Game sampleboard1 'Black '())) 'Black)
(check-expect (change-in-color (Game sampleboard1 'White '())
                               (Game sampleboard1 'White '())) 'White)
(check-expect (change-in-color (Game sampleboard1 'White '())
                               (Game sampleboard1 'Black '())) 'Black)
(check-expect (change-in-color (Game sampleboard1 'Black '())
                               (Game sampleboard1 'White '())) 'White)

(: game->turn : Game -> Player)
;; helper function that returns the Player that has the turn in an inputted game
(define (game->turn g)
  (match g
    [(Game _ turn _) turn]))
(check-expect (game->turn (Game sampleboard1 'Black '())) 'Black)
(check-expect (game->turn (Game sampleboard1 'White '())) 'White)

(: less-than-updated : Integer -> Integer)
;; ensures output is less than given input for (updated-random 7)
;; such that it is guaranteed that output is less than the given Integer
(define (less-than-updated less)
  (local
    {(define num : Integer (updated-random 7))}
  (if (< num less) num (less-than-updated less))))
(check-within (less-than-updated 8) 3 4)
(check-within (less-than-updated 3) 2 1)

(: stringlist->world : (Listof String) Style -> World)
;; helper function for string->world that takes a (Listof String) and Style
;; assuming each String is in proper format, i.e. each String
;; is a serialization (String representation) of a Game, and returns a World
;; with that Style
(define (stringlist->world xs style)
  (match xs
    ['() (error "String in improper format")]
    [(cons hd '())
     (World (string->game hd) style 'Nowhere
            (updated-random 7) 0 0 (updated-random 7) '())]
    [(cons hd (cons hd1 '()))
     (local
       {(define prev : Game (string->game hd1))
        (define game : Game (string->game hd))}
     (World game style 'Nowhere
            (if (symbol=? (game->turn game) 'White)
                (inference prev game) (less-than-updated (inference prev game)))
            0 0
            (if (symbol=? (game->turn game) 'White)
                (less-than-updated (inference prev game)) (inference prev game))
            '()))]
    [(cons hd tl)
     (local
       {(define hist : (Listof Game) (str->hist-helper tl))
        (define game : Game (string->game hd))
        (define curr-turn : Player (game->turn game))}
     (World game style 'Nowhere
            ;; w1 dice value (most recent white move value)
            (cond
              [(= (length hist) 2)
               (if (symbol=? curr-turn 'White)
                   (inference (first hist) game)
                   (min (inference (first hist) game)
                        (inference (first (rest hist)) (first hist))))]
              [(= (length hist) 3)
               (if (symbol=? curr-turn 'White)
                   (min (inference (first (rest (rest hist)))
                                   (first (rest hist)))
                        (inference (first (rest hist)) (first hist)))
                   (inference (first (rest hist)) (first hist)))]
              [(= (length hist) 4)
               (if (symbol=? curr-turn 'White) (inference (first hist) game)
                   (min (inference (first (rest (rest (rest hist))))
                                   (first (rest (rest hist))))
                        (inference (first (rest (rest hist)))
                                   (first (rest hist)))))]
              [(= (length hist) 5)
               (if (symbol=? curr-turn 'White)
                   (inference (first hist) game)
                   (min (inference (first (rest (rest (rest (rest hist)))))
                                   (first (rest (rest (rest hist)))))
                        (inference (first (rest (rest (rest hist))))
                                   (first (rest (rest hist))))))]
              [(= (length hist) 6)
               (if (symbol=? curr-turn 'White)
                   (max
                    (inference (first (rest (rest (rest (rest (rest hist))))))
                               (first (rest (rest (rest (rest hist))))))
                    (inference (first (rest (rest (rest (rest hist)))))
                               (first (rest (rest (rest hist))))))
                   (inference (first (rest hist)) (first hist)))]
              [else
               (cond
                 [(and (symbol=? curr-turn 'White)
                       (symbol=? (game->turn (first (rest hist))) 'White))
                  (inference (first hist) game)]
                 [(symbol=? curr-turn 'White) (updated-random 7)]
                 [(symbol=? (game->turn (first (rest hist))) 'Black)
                  (inference (first (rest (rest (rest hist))))
                             (first (rest (rest hist))))]
                 [else (inference (first (rest (rest hist)))
                                  (first (rest hist)))])])
            ;; w2 dice value (NOT most recent white move value)
            (cond
              [(or (= (length hist) 2) (= (length hist) 3)) 0]
              [(= (length hist) 4)
               (if (symbol=? curr-turn 'White)
                   (updated-random 7) 0)]
              [(= (length hist) 5)
               (if (symbol=? curr-turn 'White)
                   (inference (first (rest hist)) (first hist)) 0)]
              [(= (length hist) 6)
               (if (symbol=? curr-turn 'White) 0
                   (inference (first (rest (rest hist)))
                              (first (rest hist))))]
              [else
               (cond
                 [(and (symbol=? curr-turn 'White)
                       (symbol=? (game->turn (first (rest hist))) 'White))
                  (inference (first (rest hist)) (first hist))]
                 [(symbol=? curr-turn 'White) (inference (first hist) game)]
                 [(symbol=? (game->turn (first (rest hist))) 'Black)
                  (inference (first (rest (rest (rest (rest hist)))))
                             (first (rest (rest (rest hist)))))]
                 [else (inference (first (rest (rest (rest hist))))
                                  (first (rest (rest hist))))])])
            ;; d1 dice value (NOT most recent black move value)
            (cond
              [(or (= (length hist) 2) (= (length hist) 3)) 0]
              [(= (length hist) 4)
               (if (symbol=? curr-turn 'Black)
                   (updated-random 7) 0)]
              [(= (length hist) 5)
               (if (symbol=? curr-turn 'Black)
                   (inference (first (rest hist)) (first hist)) 0)]
              [(= (length hist) 6)
               (if (symbol=? curr-turn 'Black) 0
                   (inference (first (rest (rest hist)))
                              (first (rest hist))))]
              [else
               (cond
                 [(and (symbol=? curr-turn 'Black)
                       (symbol=? (game->turn (first (rest hist))) 'Black))
                  (inference (first (rest hist)) (first hist))]
                 [(symbol=? curr-turn 'Black) (inference (first hist) game)]
                 [(symbol=? (game->turn (first (rest hist))) 'White)
                  (inference (first (rest (rest (rest (rest hist)))))
                             (first (rest (rest (rest hist)))))]
                 [else (inference (first (rest (rest (rest hist))))
                                  (first (rest (rest hist))))])])
            ;; b2 dice value (most recent black move value)
            (cond
              [(= (length hist) 2)
               (if (symbol=? curr-turn 'Black)
                   (inference (first hist) game)
                   (min (inference (first hist) game)
                        (inference (first (rest hist)) (first hist))))]
              [(= (length hist) 3)
               (if (symbol=? curr-turn 'Black)
                   (min (inference (first (rest (rest hist)))
                                   (first (rest hist)))
                        (inference (first (rest hist)) (first hist)))
                   (inference (first (rest hist)) (first hist)))]
              [(= (length hist) 4)
               (if (symbol=? curr-turn 'Black) (inference (first hist) game)
                   (min (inference (first (rest (rest (rest hist))))
                                   (first (rest (rest hist))))
                        (inference (first (rest (rest hist)))
                                   (first (rest hist)))))]
              [(= (length hist) 5)
               (if (symbol=? curr-turn 'Black)
                   (inference (first hist) game)
                   (min (inference (first (rest (rest (rest (rest hist)))))
                                   (first (rest (rest (rest hist)))))
                        (inference (first (rest (rest (rest hist))))
                                   (first (rest (rest hist))))))]
              [(= (length hist) 6)
               (if (symbol=? curr-turn 'Black)
                   (max
                    (inference (first (rest (rest (rest (rest (rest hist))))))
                               (first (rest (rest (rest (rest hist))))))
                    (inference (first (rest (rest (rest (rest hist)))))
                               (first (rest (rest (rest hist))))))
                   (inference (first (rest hist)) (first hist)))]
              [else
               (cond
                 [(and (symbol=? curr-turn 'Black)
                       (symbol=? (game->turn (first (rest hist))) 'Black))
                  (inference (first hist) game)]
                 [(symbol=? curr-turn 'Black) (updated-random 7)]
                 [(symbol=? (game->turn (first (rest hist))) 'White)
                  (inference (first (rest (rest (rest hist))))
                             (first (rest (rest hist))))]
                 [else (inference (first (rest (rest hist)))
                                  (first (rest hist)))])]) hist))]))    
;; Returns a World, so no check-expects used.
;; Tested visually by playing around interactively with the game
;; (as suggested by Professor Wachs and Professor Shaw in Project 2).

(: string->world : Style String -> World)
;; Deserialization function that converts a String and input Style
;; to a World data structure. Raises an error for
;; improperly formatted Strings or inconsistencies in the loaded game.
;; Wrapper function for stringlist->world (above)
(define (string->world style s)
  (stringlist->world (string-split s "!") style))
;; Returns a World, so no check-expects used.
;; Tested visually by playing around interactively with the game
;; (as suggested by Professor Wachs and Professor Shaw in Project 2).

;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided Style to make a new World
;; raise an error if the user cancels or if something goes wrong
;; Given by Professor Wachs and Professor Shaw.
(: load-game : Style -> World)
(define (load-game s)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
      (string->world s (port->string (open-input-file path)))
      (error "load-game: user cancelled"))))

(: key : World String -> World)
;; key handling function in order to create functionality such that striking the
;; "r" key resets the board. This is just a personal decision for my game
;; mechanics. PRESS R TO RESET THE BOARD, press "u" key to undo a move.
;; Press "s" key to save, and "l" key to load a saved game.
(define (key w k)
  (match* (w k)
    [((World _ style _ _ _ _ _ _) "r")
     (local
       {(define not-equals1 : (Listof Integer) (not-equals 5))}
     (World (Game starting-board
                     (if (> (first not-equals1) (first (rest not-equals1)))
                         'Black 'White) not-equals1) style
               'Nowhere (first (rest not-equals1)) 0 0 (first not-equals1)
               '()))]
    [((World game style h? w1 w2 b1 b2 hist) "u")
     (cond
       [(empty? hist) w]
       [(not (symbol=? (guarantee-symb h?) 'Nowhere)) w]
       [(<= 1 (length hist) 2)
        (World (first hist) style 'Nowhere w1 0 0 b2 (rest hist))]
       [(= (length hist) 3)
        (World (first hist) style 'Nowhere
               (if (symbol=? (game->turn game) 'White)
                   (min (inference (first (rest (rest hist)))
                                   (first (rest hist)))
                        (inference (first (rest hist)) game))
                   (max (inference (first (rest (rest hist)))
                                   (first (rest hist)))
                        (inference (first (rest hist)) game)))
               0 0
               (if (symbol=? (game->turn game) 'Black)
                   (min (inference (first (rest (rest hist)))
                                   (first (rest hist)))
                        (inference (first (rest hist)) game))
                   (max (inference (first (rest (rest hist)))
                                   (first (rest hist)))
                        (inference (first (rest hist)) game)))
               (rest hist))]
       [(= (length hist) 4)
        (World (first hist) style 'Nowhere
           (if (symbol=? (game->turn (first (rest (rest (rest hist))))) 'White)
           (max (inference (first (rest (rest hist))) (first (rest hist)))
                (inference (first (rest (rest (rest hist))))
                      (first (rest (rest hist)))))
           (min (inference (first (rest (rest hist))) (first (rest hist)))
                (inference (first (rest (rest (rest hist))))
                      (first (rest (rest hist))))))
           (if (symbol=? (game->turn game) 'White)
               (inference (first hist) game) 0)
           (if (symbol=? (game->turn game) 'Black)
               (inference (first hist) game) 0)
        (if (symbol=? (game->turn (first (rest (rest (rest hist))))) 'Black)
           (max (inference (first (rest (rest hist))) (first (rest hist)))
                (inference (first (rest (rest (rest hist))))
                      (first (rest (rest hist)))))
           (min (inference (first (rest (rest hist))) (first (rest hist)))
                (inference (first (rest (rest (rest hist))))
                      (first (rest (rest hist)))))) (rest hist))]
       [(= (length hist) 5)
        (World (first hist) style 'Nowhere
           (if (symbol=? (game->turn (first (rest (rest (rest (rest hist))))))
                        'White)
               (min
                (inference (first (rest (rest (rest (rest hist)))))
                           (first (rest (rest (rest hist)))))
                (inference (first (rest (rest (rest hist))))
                           (first (rest (rest hist)))))
                     (min (inference (first (rest hist)) (first hist))
                          (inference (first hist) game)))
          (if (symbol=? (game->turn (first (rest (rest (rest (rest hist))))))
                        'White) 0
                     (max (inference (first (rest hist)) (first hist))
                          (inference (first hist) game)))
             (if (symbol=? (game->turn (first (rest (rest (rest (rest hist))))))
                        'White)
                 (max (inference (first (rest hist)) (first hist))
                          (inference (first hist) game)) 0)
        (if (symbol=? (game->turn (first (rest (rest (rest (rest hist))))))
                        'White)
                     (min (inference (first (rest hist)) (first hist))
                          (inference (first hist) game))
                     (max
                (inference (first (rest (rest (rest (rest hist)))))
                           (first (rest (rest (rest hist)))))
                (inference (first (rest (rest (rest hist))))
                           (first (rest (rest hist)))))) (rest hist))]
       [else
        (World (first hist) style 'Nowhere
               ;; w1: assumes MOST RECENT 'WhiteDice value
               (cond
                 [(= (inference (first hist) game) 99) w1]
                 [(and (= (inference (first hist) game) -1)
                       (symbol=? (change-in-color (first hist) game) 'White))
                  (if (symbol=? (game->turn (first (rest
                  (rest (rest (rest hist)))))) 'Black)
                      (inference
                   (first (rest (rest (rest (rest (rest (rest hist)))))))
                       (first (rest (rest (rest (rest (rest hist)))))))
                   (inference (first (rest (rest (rest (rest hist)))))
                              (first (rest (rest (rest hist))))))]
                 [(and (= (inference (first hist) game) -1)
                       (symbol=? (change-in-color (first hist) game) 'Black))
                  (inference (first (rest hist)) (first hist))]
                 [(symbol=? (game->turn game) 'Black) ;; curr 'Black turn
                  (cond
                    [(and (>= (length hist) 6)
                     (symbol=? 'Black
                        (game->turn (first (rest (rest (rest hist)))))))
                     (inference (first (rest (rest (rest (rest (rest hist))))))
                                (first (rest (rest (rest (rest hist))))))]
                     [(and (>= (length hist) 5)
                       (symbol=? 'Black
                        (game->turn (first (rest (rest hist))))))
                      (inference (first (rest (rest (rest (rest hist)))))
                                (first (rest (rest (rest hist)))))] ;; doubles
                    [(and (>= (length hist) 4)
                      (symbol=? (game->turn (first (rest hist))) 'Black))
                      (inference (first (rest (rest (rest hist))))
                                 (first (rest (rest hist))))]
                    [(and (>= (length hist) 3)
                          (symbol=? (game->turn (first hist)) 'Black)
                          (symbol=? (game->turn (first (rest hist))) 'White))
                     (inference (first (rest (rest hist)))
                                (first (rest hist)))]
                    [else w1])]
                  [else ;; (symbol=? (game->turn game) 'White)
                   (inference (first hist) game)])
                  ;; w2: assumes NOT most recent 'WhiteDice value
               (cond
                 [(= (inference (first hist) game) 99) w2]
                 [(and (= (inference (first hist) game) -1)
                       (symbol=? (change-in-color (first hist) game) 'White))
                  (if (symbol=? (game->turn (first (rest
                  (rest (rest (rest hist)))))) 'Black)
                      (inference
                   (first (rest (rest (rest (rest (rest (rest (rest hist))))))))
                       (first (rest (rest (rest (rest (rest (rest hist))))))))
                   (inference (first (rest (rest (rest (rest (rest hist))))))
                              (first (rest (rest (rest (rest hist)))))))]
                 [(and (= (inference (first hist) game) -1)
                       (symbol=? (change-in-color (first hist) game) 'Black))
                  (inference (first (rest (rest hist))) (first (rest hist)))]
                 [(symbol=? (game->turn game) 'Black) ;; curr 'Black turn
                  (cond
                    [(and (>= (length hist) 6)
                     (symbol=? 'Black
                        (game->turn (first (rest (rest (rest hist)))))))
                     (inference (first (rest (rest (rest (rest
                                                         (rest (rest hist)))))))
                               (first (rest (rest (rest (rest (rest hist)))))))]
                     [(and (>= (length hist) 5)
                       (symbol=? 'Black
                        (game->turn (first (rest (rest hist))))))
                      (inference (first (rest (rest (rest (rest (rest hist))))))
                     (first (rest (rest (rest (rest hist))))))] ;; doubles
                    [(and (>= (length hist) 4)
                      (symbol=? (game->turn (first (rest hist))) 'Black))
                      (inference (first (rest (rest (rest (rest hist)))))
                                 (first (rest (rest (rest hist)))))]
                    [(and (>= (length hist) 3)
                          (symbol=? (game->turn (first hist)) 'Black)
                          (symbol=? (game->turn (first (rest hist))) 'White))
                     (inference (first (rest (rest (rest hist))))
                                (first (rest (rest hist))))]
                    [else w2])]
                  [else ;; (symbol=? (game->turn game) 'White)
                   (inference (first (rest hist)) (first hist))])
               ;; b1 dice values, most recent
               (cond
                 [(= (inference (first hist) game) 99) b1]
                 [(and (= (inference (first hist) game) -1)
                       (symbol=? (change-in-color (first hist) game) 'Black))
                  (if (symbol=? (game->turn (first (rest
                  (rest (rest (rest hist)))))) 'White)
                      (inference
                   (first (rest (rest (rest (rest (rest (rest hist)))))))
                       (first (rest (rest (rest (rest (rest hist)))))))
                   (inference (first (rest (rest (rest (rest hist)))))
                              (first (rest (rest (rest hist))))))]
                 [(and (= (inference (first hist) game) -1)
                       (symbol=? (change-in-color (first hist) game) 'White))
                  (inference (first (rest hist)) (first hist))]
                 [(symbol=? (game->turn game) 'White) ;; curr 'White turn
                  (cond
                    [(and (>= (length hist) 6)
                     (symbol=? 'White
                        (game->turn (first (rest (rest (rest hist)))))))
                     (inference (first (rest (rest (rest (rest (rest hist))))))
                                (first (rest (rest (rest (rest hist))))))]
                     [(and (>= (length hist) 5)
                       (symbol=? 'White
                        (game->turn (first (rest (rest hist))))))
                      (inference (first (rest (rest (rest (rest hist)))))
                                (first (rest (rest (rest hist)))))] ;; doubles
                    [(and (>= (length hist) 4)
                      (symbol=? (game->turn (first (rest hist))) 'White))
                      (inference (first (rest (rest (rest hist))))
                                 (first (rest (rest hist))))]
                    [(and (>= (length hist) 3)
                          (symbol=? (game->turn (first hist)) 'White)
                          (symbol=? (game->turn (first (rest hist))) 'Black))
                     (inference (first (rest (rest hist)))
                                (first (rest hist)))]
                    [else b1])]
                  [else ;; (symbol=? (game->turn game) 'Black)
                   (inference (first hist) game)])
               ;; for b2 dice values inference (not most recent)
               (cond
                 [(= (inference (first hist) game) 99) b2]
                 [(and (= (inference (first hist) game) -1)
                       (symbol=? (change-in-color (first hist) game) 'Black))
                  (if (symbol=? (game->turn (first (rest
                  (rest (rest (rest hist)))))) 'White)
                      (inference
                   (first (rest (rest (rest (rest (rest (rest (rest hist))))))))
                       (first (rest (rest (rest (rest (rest (rest hist))))))))
                   (inference (first (rest (rest (rest (rest (rest hist))))))
                              (first (rest (rest (rest (rest hist)))))))]
                 [(and (= (inference (first hist) game) -1)
                       (symbol=? (change-in-color (first hist) game) 'White))
                  (inference (first (rest (rest hist))) (first (rest hist)))]
                 [(symbol=? (game->turn game) 'White) ;; curr 'White turn
                  (cond
                    [(and (>= (length hist) 6)
                     (symbol=? 'White
                        (game->turn (first (rest (rest (rest hist)))))))
                     (inference (first (rest (rest (rest (rest
                                                         (rest (rest hist)))))))
                               (first (rest (rest (rest (rest (rest hist)))))))]
                     [(and (>= (length hist) 5)
                       (symbol=? 'White
                        (game->turn (first (rest (rest hist))))))
                      (inference (first (rest (rest (rest (rest
                                                           (rest hist))))))
                                (first (rest (rest (rest (rest hist))))))]
                     ;; doubles
                    [(and (>= (length hist) 4)
                      (symbol=? (game->turn (first (rest hist))) 'White))
                      (inference (first (rest (rest (rest (rest hist)))))
                                 (first (rest (rest (rest hist)))))]
                    [(and (>= (length hist) 3)
                          (symbol=? (game->turn (first hist)) 'White)
                          (symbol=? (game->turn (first (rest hist))) 'Black))
                     (inference (first (rest (rest (rest hist))))
                                (first (rest (rest hist))))]
                    [else b2])]
                  [else ;; (symbol=? (game->turn game) 'Black)
                   (inference (first (rest hist)) (first hist))]) 
               (rest hist))])]
    [((World _ _ _ _ _ _ _ _) "s") (begin (save-world! w) w)] ;;returns World w
    [((World _ style _ _ _ _ _ _) "l") (load-game style)] ;; l key to load game
    [(_ _) w])) ;; want no change with other key strikes

(: draw-world : World -> Image)
;; draws the World based on the inputted game state of World
;; of the board, highlights, dice, and checkers in approprite locations
(define (draw-world w)
  (match w
    [(World (Game board turn moves) style h? w1 w2 b1 b2 _)
     (match style
    [(Style c-rad sp _ _ _ _ _ _ _ _)
     (local
       {;; overlay 3 different size outline rectangles for thicker highlight:
        (define highlight : Image (overlay (rectangle (+ (* 2 c-rad) 4)
                                                      (+ (* 10 c-rad) 4)
                                                      "outline" "gold")
                                   (overlay (rectangle (+ (* 2 c-rad) 2)
                                                      (+ (* 10 c-rad) 2)
                                                      "outline" "gold")
                                   (rectangle (* 2 c-rad) (* 10 c-rad)
                                             "outline" "gold"))))
        (define dice-board : Image
          (underlay/offset
          (underlay/offset (draw-board style board)
                           (- (+ (* 9 c-rad) (* sp (/ 5 2))))
                            (* (- (/ 1 2)) c-rad)
                            (double-dice c-rad w1 w2 'White))
                            (+ (* 9 c-rad)
                             (* sp (/ 5 2))) (* (- (/ 1 2)) c-rad)
                                (double-dice c-rad b1 b2 'Black)))}
       (if (game-over? (Game board turn moves))
           (overlay (display-winner c-rad (winner (Game board turn moves)))
                    dice-board)
         (match h?
           [(PointNum i)
            (if (emptypoint? (point-returner (Board-points board) i))
                dice-board
            (cond
            [(= i 1) (underlay/offset dice-board (+ (* 14 c-rad) (* 5 sp))
                                      (* (/ 29 4) c-rad) highlight)]
            [(= i 2) (underlay/offset dice-board (+ (* 12 c-rad) (* 4 sp))
                                      (* (/ 29 4) c-rad) highlight)]
            [(= i 3) (underlay/offset dice-board (+ (* 10 c-rad) (* 3 sp))
                                      (* (/ 29 4) c-rad) highlight)]
            [(= i 4) (underlay/offset dice-board (+ (* 8 c-rad) (* 2 sp))
                                      (* (/ 29 4) c-rad) highlight)]
            [(= i 5) (underlay/offset dice-board (+ (* 6 c-rad) sp)
                                      (* (/ 29 4) c-rad) highlight)]
            [(= i 6) (underlay/offset dice-board (* 4 c-rad)
                                      (* (/ 29 4) c-rad) highlight)]
            [(= i 12) (underlay/offset dice-board (- (+ (* 14 c-rad) (* 5 sp)))
                                      (* (/ 29 4) c-rad) highlight)]
            [(= i 11) (underlay/offset dice-board (- (+ (* 12 c-rad) (* 4 sp)))
                                      (* (/ 29 4) c-rad) highlight)]
            [(= i 10) (underlay/offset dice-board (- (+ (* 10 c-rad) (* 3 sp)))
                                      (* (/ 29 4) c-rad) highlight)]
            [(= i 9) (underlay/offset dice-board (- (+ (* 8 c-rad) (* 2 sp)))
                                      (* (/ 29 4) c-rad) highlight)]
            [(= i 8) (underlay/offset dice-board (- (+ (* 6 c-rad) sp))
                                      (* (/ 29 4) c-rad) highlight)]
            [(= i 7) (underlay/offset dice-board (- (* 4 c-rad))
                                      (* (/ 29 4) c-rad) highlight)]
            [(= i 24) (underlay/offset dice-board (+ (* 14 c-rad) (* 5 sp))
                                      (- (* (/ 35 4) c-rad)) highlight)]
            [(= i 23) (underlay/offset dice-board (+ (* 12 c-rad) (* 4 sp))
                                      (- (* (/ 35 4) c-rad)) highlight)]
            [(= i 22) (underlay/offset dice-board (+ (* 10 c-rad) (* 3 sp))
                                      (- (* (/ 35 4) c-rad)) highlight)]
            [(= i 21) (underlay/offset dice-board (+ (* 8 c-rad) (* 2 sp))
                                      (- (* (/ 35 4) c-rad)) highlight)]
            [(= i 20) (underlay/offset dice-board (+ (* 6 c-rad) sp)
                                      (- (* (/ 35 4) c-rad)) highlight)]
            [(= i 19) (underlay/offset dice-board (* 4 c-rad)
                                      (- (* (/ 35 4) c-rad)) highlight)]
            [(= i 13) (underlay/offset dice-board (- (+ (* 14 c-rad) (* 5 sp)))
                                      (- (* (/ 35 4) c-rad)) highlight)]
            [(= i 14) (underlay/offset dice-board (- (+ (* 12 c-rad) (* 4 sp)))
                                      (- (* (/ 35 4) c-rad)) highlight)]
            [(= i 15) (underlay/offset dice-board (- (+ (* 10 c-rad) (* 3 sp)))
                                      (- (* (/ 35 4) c-rad)) highlight)]
            [(= i 16) (underlay/offset dice-board (- (+ (* 8 c-rad) (* 2 sp)))
                                      (- (* (/ 35 4) c-rad)) highlight)]
            [(= i 17) (underlay/offset dice-board (- (+ (* 6 c-rad) sp))
                                      (- (* (/ 35 4) c-rad)) highlight)]
            [(= i 18) (underlay/offset dice-board (- (* 4 c-rad))
                                      (- (* (/ 35 4) c-rad)) highlight)]
      [else (error "PointNum Integer should be between 1 and 24, inclusive")]))]
          ['BlackBar (if (= (Board-black-bar board) 0) dice-board
           (underlay/offset dice-board 0 (- (* (/ 23 4) c-rad)) highlight))]
         ['WhiteBar (if (= (Board-white-bar board) 0) dice-board
          (underlay/offset dice-board 0 (* (/ 17 4) c-rad) highlight))]
  [_ dice-board])))])])) ;; accounts for if (World-h? w) is 'Nowhere
 ;; eyeball-tests below:    (w is World)
(draw-world initial-world)                           ;; 'Nowhere
(draw-world (World (Game starting-board 'Black (list 3 4)) samplestyle1
                                    (PointNum 13) 1 2 3 4 '())) ;; (PointNum 13)
(draw-world (World (Game starting-board 'Black (list 3 4)) samplestyle1
                                    (PointNum 24) 1 2 3 4 '())) ;; (PointNum 24)
(draw-world (World (Game starting-board 'Black (list 3 4)) samplestyle1
                                    (PointNum 11) 1 2 3 4 '())) ;; (PointNum 11)
(draw-world (World (Game starting-board 'Black(list 3 4)) samplestyle1
                                     (PointNum 1) 1 2 3 4 '())) ;; (PointNum 1)
(draw-world (World (Game starting-board 'Black(list 3 4)) samplestyle1
                                     (PointNum 1) 1 2 3 4 '())) ;; (PointNum 1)
(draw-world (World (Game
                    (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         (OccupiedPoint 'Black 5) (OccupiedPoint 'Black 5)
         (OccupiedPoint 'Black 5) (OccupiedPoint 'White 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)) 0 0 15 0)
                    'Black (list 3 4)) samplestyle1
                               (PointNum 1) 1 2 3 4 '())) ;; Black Wins!!!
(draw-world (World (Game
                    (Board
   (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint 'EmptyPoint
         (OccupiedPoint 'Black 5) (OccupiedPoint 'Black 5)
         (OccupiedPoint 'Black 5) (OccupiedPoint 'White 5)
         (OccupiedPoint 'White 5) (OccupiedPoint 'White 5)) 0 0 0 15)
                    'Black (list 3 4)) samplestyle1
                               (PointNum 1) 1 2 3 4 '())) ;; White Wins!!!
;; No need to highlight 'BlackOff or 'WhiteOff, since those locations are not
;; valid origin click locations.

;; Starting dice rolls for each player (for starting the game)
(define not-equals2 : (Listof Integer)
  (local
    {(define b1 : Integer (updated-random 7))
    (define w1 : Integer (re-roll b1 (updated-random 7)))}
    (list b1 w1)))

(: run : Style -> World)
;; run function incorporating big-bang and an initial-world, with input Style
;; that runs the program as desired (Backgammon game functionality).
;; Incorporates the Universe package "cs151-universe.rkt"
(define (run style)
  (big-bang
        (World (Game starting-board
                     (if (> (first not-equals2) (first (rest not-equals2)))
                         'Black 'White) not-equals2) style
               'Nowhere (first (rest not-equals2)) 0 0 (first not-equals2) '())
      : World
    [to-draw draw-world]
    [on-mouse click]
    [on-key key]
    [on-tick tick]))

(run samplestyle1)

(test)
