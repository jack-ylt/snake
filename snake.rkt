;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |219|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; constance
(define WIDTH 400)
(define HEIGHT 300)
(define SIZE 8)

(define WORM (square SIZE "solid" "red"))
(define FOOD (square SIZE "solid" "green"))
(define MT (empty-scene WIDTH HEIGHT))


(define-struct pos [x y])
(define-struct dir1 [dx dy])
(define-struct food [x y])
; world state
(define-struct ws [poss dir1 food])
; poss
; - pos
; - (cons pos poss)



(define S0 (make-ws
            (list (make-pos 3 5) (make-pos 2 5))
            (make-dir1 1 0)
            (make-food 20 20)))
(define S1 (make-ws
            (list (make-pos 3 5) (make-pos 2 5))
            (make-dir1 0 1)
            (make-food 20 20)))



; main
(define (run s)
  (big-bang s
            [to-draw render]
            [on-key keyh]
            [on-tick tock 0.1]
            [stop-when over? last-pic]
            [state #false]))



; ws -> img
; render the game
(define (render s)
  (draw-food (ws-food s)
             (draw-worms (ws-poss s))))

; pos img -> img
(define (draw-food p img)
  (local ((define x (* (food-x p) SIZE))
          (define y (* (food-y p) SIZE)))
    (place-image FOOD x y img)))

; list-of posn img -> img
(define (draw-worms lop)
  (cond
    [(empty? lop) MT]
    [else (draw-worm (first lop) (draw-worms (rest lop)))]))
          
; posn img -> img
(check-expect (draw-worm (make-pos 10 10) MT)
              (place-image WORM (* 10 SIZE) (* 10 SIZE) MT))

(define (draw-worm p img)
  (local ((define px (* (pos-x p) SIZE))
          (define py (* (pos-y p) SIZE)))
    (place-image WORM px py img)))

; ws -> img
; render the last picture
(define (last-pic s)
  (local ((define msg
            (if (hit-wall? (first (ws-poss s)))
                "worm hit border"
                "worm hit itself")))
    (render-msg msg (render s))))

; msg ws -> img
(define (render-msg msg img)
  (place-image (text msg 16 "red")
               80 (- HEIGHT 30) img))



; ke, ws -> ws
; change the direction by the key
(check-expect (keyh S0 "down") S1)

(define (keyh s ke)
  (cond
    [(string=? ke "left") (if (equal? (ws-dir1 s) (make-dir1 1 0))
                              s
                              (update-dir1 s (make-dir1 -1 0)))]
    [(string=? ke "right") (if (equal? (ws-dir1 s) (make-dir1 -1 0))
                              s
                              (update-dir1 s (make-dir1 1 0)))]
    [(string=? ke "up") (if (equal? (ws-dir1 s) (make-dir1 0 1))
                              s
                              (update-dir1 s (make-dir1 0 -1)))]
    [(string=? ke "down") (if (equal? (ws-dir1 s) (make-dir1 0 -1))
                              s
                              (update-dir1 s (make-dir1 0 1)))]
    [else s]))

; ws, dir -> ws
(define (update-dir1 s dir1)
  (make-ws (ws-poss s)
           dir1
           (ws-food s)))



; ws -> ws
; ws update by click
(define (tock s)
  (if (eat? s)
      (local ((define new-food (food-create (ws-food s)))
              (define new-poss (cons (update-pos (first (ws-poss s))
                                                 (ws-dir1 s))
                                     (ws-poss s))))
      (make-ws new-poss (ws-dir1 s) new-food))          
  (make-ws (update-poss (ws-poss s) (ws-dir1 s))
           (ws-dir1 s)
           (ws-food s))))

; ws -> boolean
(define (eat? s)
  (local ((define worm-head (first (ws-poss s)))
          (define food (ws-food s)))
    (and (= (pos-x worm-head) (food-x food))
         (= (pos-y worm-head) (food-y food)))))

; non-empty list-of pos, dir1 -> list-of pos
(define (update-poss lop d)
  (cons (update-pos (first lop) d)
        (drop-last lop)))

; non-empyt list -> list
(check-expect (drop-last (list 1 2 3 4)) (list 1 2 3))

(define (drop-last l)
  (cond
    [(empty? (rest l)) '()]
    [else (cons (first l) (drop-last (rest l)))]))

; pos, dir1 -> pos
(check-expect (update-pos (make-pos 1 1) (make-dir1 1 0))
              (make-pos 2 1))

(define (update-pos p d)
  (make-pos (+ (pos-x p) (dir1-dx d))
            (+ (pos-y p) (dir1-dy d))))



; ws -> boolean
(define (over? s)
  (or (hit-wall? (first (ws-poss s)))
      (hit-self? (first (ws-poss s))
                 (rest (ws-poss s)))))

; pos -> boolean
(check-expect (hit-wall? (make-pos 0 10)) #true)
(check-expect (hit-wall? (make-pos 1 (/ HEIGHT SIZE))) #true)

(define (hit-wall? p)
  (or (<= (pos-x p) 0)
      (>= (pos-x p) (/ WIDTH SIZE))
      (<= (pos-y p) 0)
      (>= (pos-y p) (/ HEIGHT SIZE))))


; pos, list-of pos -> boolean
(define (hit-self? p lop)
  (member? p lop))



; Posn -> Posn 
; create a food
(check-satisfied (food-create (make-food 3 3)) not=-3-3?)
(define (food-create p)
  (food-check-create
   p (make-food (random 2 (round (- (/ WIDTH SIZE) 2)))  ; -2, make the food not too near the board
                (random 2 (round (- (/ HEIGHT SIZE) 2))))))
 
; Posn Posn -> Posn 
; generative recursion 
; make sure the food at new postion
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))
 
; Posn -> Boolean
; use for testing only 
(define (not=-3-3? p)
  (not (and (= (food-x p) 3) (= (food-y p) 3))))



; run
(run S0)
