#lang racket

(require racket/draw)
(require racket/gui/base)

(define MODE 'mode-select)

(define menu-option-current 0)

(define number-of-players 0)

(define turn-hash (hash))

(define (menu option)
  (cond
    [(equal? option 'left) (set! menu-option-current (modulo (sub1 menu-option-current) 4))]
    [(equal? option 'right) (set! menu-option-current (modulo (add1 menu-option-current) 4))]
    [(equal? option 'select) (cond
			       [(= menu-option-current 0) (set! turn-hash (for/hash ([k (range 0 2)]
										     [v (list 1 4)])
									    (values k v)))
							  (set! number-of-players 2)]
			       [(= menu-option-current 1) (set! turn-hash (for/hash ([k (range 0 3)]
										     [v (list 1 3 5)])
									    (values k v)))
							  (set! number-of-players 3)]
			       [(= menu-option-current 2) (set! turn-hash (for/hash ([k (range 0 4)]
										     [v (list 2 3 5 6)])
									    (values k v)))
							  (set! number-of-players 4)]
			       [(= menu-option-current 3) (set! turn-hash (for/hash ([k (range 0 6)]
										     [v (list 1 2 3 4 5 6)])
									    (values k v)))
							  (set! number-of-players 6)])
			     (vector-map! (curry vector-map! (lambda (el) (if (and (number? el) (not (member el (hash-values turn-hash))))
									    #t
									    el))) board)
			     (set! MODE 'ready-to-play)]))

(define color-hash
  (hash #t "white"
        1 "green"
        2 "yellow"
        3 "orange"
        4 "red"
        5 "purple"
        6 "blue"))

(define last-pos-hash (make-hash))

(define last-rot-hash (make-hash))

(define triangle-positions
  (list (cons -5 (* -5 (sqrt 3)))
        (cons 10 (* -10 (sqrt 3)))
        (cons 10 0)
        (cons 5 (* -5 (sqrt 3)))
        (cons 20 0)
        (cons 5 (* 5 (sqrt 3)))
        (cons 10 0)
        (cons 10 (* 10 (sqrt 3)))
        (cons -5 (* 5 (sqrt 3)))
        (cons 5 (* 5 (sqrt 3)))
        (cons -10 (* 10 (sqrt 3)))
        (cons -10 0)
        (cons -5 (* 5 (sqrt 3)))
        (cons -20 0)
        (cons -5 (* -5 (sqrt 3)))
        (cons -10 0)
        (cons -10 (* -10 (sqrt 3)))
        (cons 5 (* -5 (sqrt 3)))))

(define corner-hash
  (hash
    1 (cons 0 6)
    2 (cons 4 12)
    3 (cons 12 12)
    4 (cons 16 6)
    5 (cons 12 0)
    6 (cons 4 0)))

(define rotation-hash
  (hash
    1 150
    2 210
    3 270
    4 330
    5 30
    6 90))

(define win-boxes
  (hash
    1 (list (cons 0 6)
	    (cons 1 5) (cons 1 6)
	    (cons 2 5) (cons 2 6) (cons 2 7)
	    (cons 3 4) (cons 3 5) (cons 3 6) (cons 3 7))
    2 (list (cons 4 9) (cons 4 10) (cons 4 11) (cons 4 12)
	    (cons 5 9) (cons 5 10) (cons 5 11)
	    (cons 6 10) (cons 6 11)
	    (cons 7 10))
    3 (list (cons 9 10)
	    (cons 10 10) (cons 10 11)
	    (cons 11 9) (cons 11 10) (cons 11 11)
	    (cons 12 9) (cons 12 10) (cons 12 11) (cons 12 12))
    4 (list (cons 13 4) (cons 13 5) (cons 13 6) (cons 13 7)
	    (cons 14 5) (cons 14 6) (cons 14 7)
	    (cons 15 5) (cons 15 6)
	    (cons 16 6))
    5 (list (cons 12 0) (cons 12 1) (cons 12 2) (cons 12 3)
	    (cons 11 0) (cons 11 1) (cons 11 2)
	    (cons 10 1) (cons 10 2)
	    (cons 9 1))
    6 (list (cons 7 1)
	    (cons 6 1) (cons 6 2)
	    (cons 5 0) (cons 5 1) (cons 5 2)
	    (cons 4 0) (cons 4 1) (cons 4 2) (cons 4 3))))

(define turn -1)

(define move-possibilities (list))

(define pos (cons -1 -1))

(define rotationDEG -1)

(define poised-for-move? #f)

(define poised-for-move-backward? #f)

(define to-move #f)

(define start-point #f)

(define board
  (for/vector ([len-invalid-left '(6 5 5 4 0 0 1 1 2 1 1 0 0 4 5 5 6)]
               [len-invalid-right '(6 6 5 5 0 1 1 2 2 2 1 1 0 5 5 6 6)]
               [center '((1 . 1) (1 . 2) (1 . 3) (1 . 4) 5 6 7 8 9 8 7 6 5 (4 . 4) (4 . 3) (4 . 2) (4 . 1))]
               [left '(#f #f #f #f (6 . 4) (6 . 3) (6 . 2) (6 . 1) #f (5 . 1) (5 . 2) (5 . 3) (5 . 4) #f #f #f #f)]
               [right '(#f #f #f #f (2 . 4) (2 . 3) (2 . 2) (2 . 1) #f (3 . 1) (3 . 2) (3 . 3) (3 . 4) #f #f #f #f)])
    (vector-append (make-vector len-invalid-left #f) 
                   (if left (make-vector (cdr left) (car left)) (vector))
                   (if (pair? center) (make-vector (cdr center) (car center)) (make-vector center #t))
                   (if right (make-vector (cdr right) (car right)) (vector))
                   (make-vector len-invalid-right #f))))

(define (check-for-win t)
  (for/and ([loc (hash-ref win-boxes t)])
    (equal? (board-ref loc) t)))

(define (move-piece to-move dest)
  (let ([contents (board-ref to-move)])
    (board-set (car to-move) (cdr to-move) #t)
    (board-set (car dest) (cdr dest) contents)))

(define (board-set row col contents)
  (vector-set! (vector-ref board row) col contents))

(define (board-ref position)
  (vector-ref (vector-ref board (car position)) (cdr position)))

(define (within-range destination piece)
  (let* ([piece-row (car piece)]
	 [piece-col (cdr piece)]
	 [even (even? piece-row)]
	 [inner-outer-circle (hash
			       (cons piece-row (sub1 (sub1 piece-col))) (cons piece-row (sub1 piece-col))
			       (cons (sub1 (sub1 piece-row)) (sub1 piece-col)) (cons (sub1 piece-row) (if even (sub1 piece-col) piece-col))
			       (cons (sub1 (sub1 piece-row)) (add1 piece-col)) (cons (sub1 piece-row) (if even piece-col (add1 piece-col)))
			       (cons piece-row (add1 (add1 piece-col))) (cons piece-row (add1 piece-col))
			       (cons (add1 (add1 piece-row)) (add1 piece-col)) (cons (add1 piece-row) (if even piece-col (add1 piece-col)))
			       (cons (add1 (add1 piece-row)) (sub1 piece-col)) (cons (add1 piece-row) (if even (sub1 piece-col) piece-col)))])
    (or (and (member 2 move-possibilities)
	     (let ([res (hash-ref inner-outer-circle destination #f)])
	       (and res (number? (board-ref res))))
	     (set! move-possibilities (list 2)))
	(and (member 1 move-possibilities)
	     (member destination (hash-values inner-outer-circle))
	     (set! move-possibilities (list -1)))
	(and (member -1 move-possibilities)
	     (equal? destination start-point)))))

(define (select)
  (let ([pos-contents (board-ref pos)])
    (cond
      [(and to-move (equal? pos-contents #t) (within-range pos to-move))
       (begin (move-piece to-move pos) (set! to-move pos) (when (equal? to-move start-point) (set! move-possibilities (list 1 2))))]
      [(and (not to-move) (equal? pos-contents (hash-ref turn-hash turn #f)))
       (begin (set! start-point pos) (set! to-move pos) (set! move-possibilities (list 1 2)))])))

(define (unselect)
  (when to-move 
    (when (not (equal? to-move start-point)) (move-piece to-move start-point))
    (set! pos start-point)
    (move 'for-sake-of-poised-for-move)
    (set! to-move #f)
    (set! start-point #f)
    (set! move-possibilities (list))))

(define (solidify)
  (when (and to-move (not (equal? to-move start-point)))
    (begin (set! to-move #f)
	   (set! move-possibilities (list))
	   (set! start-point #f)
	   (hash-set! last-pos-hash turn pos)
	   (hash-set! last-rot-hash turn rotationDEG)
	   (if (check-for-win (hash-ref turn-hash turn #f))
	     (set! turn -1)
	     (begin (set! turn (modulo (add1 turn) number-of-players))
		    (set! pos (or (hash-ref last-pos-hash turn #f) (hash-ref corner-hash (hash-ref turn-hash turn))))
		    (set! rotationDEG (or (hash-ref last-rot-hash turn #f) (hash-ref rotation-hash (hash-ref turn-hash turn))))
		    (move 'for-sake-of-poised-for-move))))))

(define (exists? row col)
  (and (>= row 0) (<= row 16) (>= col 0) (<= col 12)))

(define (coterminal angl)
  (cond
    [(< angl 0) (+ angl 360)]
    [(>= angl 360) (- angl 360)]
    [else angl]))

(define (adjust-pos row col rot backward?)
  (cond
    [(or (and (not backward?) (= rot 30)) (and backward? (= rot 210)))
     (values (sub1 row)
             (if (even? row)
                 col
                 (add1 col)))]
    [(or (and (not backward?) (= rot 90)) (and backward? (= rot 270)))
     (values row
             (add1 col))]
    [(or (and (not backward?) (= rot 150)) (and backward? (= rot 330)))
     (values (add1 row)
             (if (even? row)
                 col
                 (add1 col)))]
    [(or (and (not backward?) (= rot 210)) (and backward? (= rot 30)))
     (values (add1 row)
             (if (even? row)
                 (sub1 col)
                 col))]
    [(or (and (not backward?) (= rot 270)) (and backward? (= rot 90)))
     (values row
             (sub1 col))]
    [(or (and (not backward?) (= rot 330)) (and backward? (= rot 150)))
     (values (sub1 row)
             (if (even? row)
                 (sub1 col)
                 col))]))

(define (move dir)
  (cond
    [(equal? dir 'left) (set! rotationDEG (coterminal (- rotationDEG 60)))]
    [(equal? dir 'right) (set! rotationDEG (coterminal (+ rotationDEG 60)))]
    [(equal? dir 'forward) (when poised-for-move? (set! pos poised-for-move?))]
    [(equal? dir 'backward) (when poised-for-move-backward? (set! pos poised-for-move-backward?))])
  (let-values ([(new-row new-col) (adjust-pos (car pos) (cdr pos) rotationDEG #f)]
               [(new-row-back new-col-back) (adjust-pos (car pos) (cdr pos) rotationDEG #t)])
    (if (and (exists? new-row new-col) (board-ref (cons new-row new-col)))
        (set! poised-for-move? (cons new-row new-col))
        (set! poised-for-move? #f))
    (if (and (exists? new-row-back new-col-back) (board-ref (cons new-row-back new-col-back)))
        (set! poised-for-move-backward? (cons new-row-back new-col-back))
        (set! poised-for-move-backward? #f))))

(define frame (new frame%
                   [label "Chinese Checkers"]
                   [width 500]
                   [height 500]))

(define my-canvas%
  (class canvas%
    (inherit refresh get-dc get-client-size)
    (define/override (on-paint)
      (define dc (get-dc))
      (define x-pos-current #f)
      (define y-pos-current #f)
      (define color-current #f)
      (define alpha-current #f)
      (define sub 0)
      (if (equal? MODE 'mode-select)
	(begin (send dc set-text-foreground "black")
	       (for ([i 4]
		     [player (list "2 Players" "3 Players" "4 Players" "6 Players")])
		 (if (= i menu-option-current)
		   (begin (send dc set-font (make-font #:size 12 #:family 'roman #:weight 'bold)) (set! sub -3))
		   (begin (send dc set-font (make-font #:size 12 #:family 'roman #:weight 'normal)) (set! sub 0)))
		 (send dc draw-text player (+ 10 (* i 134) sub) 25)))
	(begin (send dc set-text-foreground "black")
	       (send dc set-font (make-font #:size 8 #:family 'roman #:weight 'bold))
	       (send dc set-pen "black" 1 'solid)
	       (for ([cnt number-of-players]
		     [player (take (list "Player 1" "Player 2" "Player 3" "Player 4" "Player 5" "Player 6") number-of-players)]
		     [xp (range (/ 500 (add1 number-of-players)) 500 (/ 500 (add1 number-of-players)))])
		 (send dc draw-text player (- xp 35) 10)
		 (send dc set-brush (hash-ref color-hash (hash-ref turn-hash cnt)) 'solid)
		 (send dc draw-rectangle (+ xp 20) 10 10 10))
	       (when (equal? MODE 'ready-to-play)
		 (send dc set-font (make-font #:size 16 #:family 'roman #:weight 'bold))
		 (send dc draw-text "PRESS ENTER TO START" 100 450))))
      (send dc set-pen "black" 2 'solid)
      (send dc set-brush "burlywood" 'solid)
      (send dc draw-ellipse 75 75 350 350)
      (for ([row board]
	    [y-pos (range 80 420 20)]
	    [row-number 17]
	    [col-start '(6 5 5 4 0 0 1 1 2 1 1 0 0 4 5 5 6)])
	(let* ([row-valid (vector-filter (lambda (el) el) row)]
	       [len (vector-length row-valid)]
	       [start-x-pos (- 250 (* (/ len 2) 23))])
	  (for ([x-pos (range start-x-pos (+ start-x-pos (* 23 len)) 23)]
		[el row-valid]
		[col-number (range col-start (+ col-start (vector-length row-valid)))])
	    (let ([br (board-ref (cons row-number col-number))])
	      (if (and (= (car pos) row-number) (= (cdr pos) col-number))
		(set!-values (x-pos-current y-pos-current color-current alpha-current)
			     (values x-pos y-pos (hash-ref color-hash (if (pair? el) (car el) el)) (if (or (equal? br (hash-ref turn-hash turn #f)) 
												           (equal? br #t))
												     1.0 
												     .5)))
		(begin (if (equal? (cons row-number col-number) to-move) (send dc set-pen "black" 4 'solid) (send dc set-pen "black" 2 'solid))
		       (if (or (equal? br (hash-ref turn-hash turn #f)) (equal? br #t))
			   (send dc set-alpha 1.0)
			   (send dc set-alpha .5))
		       (send dc set-brush (hash-ref color-hash (if (pair? el) (car el) el)) 'solid)
		       (send dc draw-ellipse (+ 1.5 x-pos) y-pos 20 20)))))))
      (when (equal? MODE 'playing) (let ([center-x (+ 11.5 x-pos-current)]
					 [center-y (+ 10 y-pos-current)]
					 [el-start (* 3 (quotient rotationDEG 60))])
				     (send dc set-pen "black" 0 'transparent)
				     (if poised-for-move?
				         (send dc set-brush "black" 'solid)
				         (send dc set-brush "black" 'hilite))
				     (send dc draw-polygon (map (lambda (el)
								  (cons (+ (car el) center-x) (+ (cdr el) center-y)))
								(take (drop triangle-positions el-start) 3)))
				     (send dc set-pen "black" 4 'solid)
				     (send dc set-brush color-current 'solid)
				     (send dc set-alpha alpha-current)
				     (send dc draw-ellipse (+ 1.5 x-pos-current) y-pos-current 20 20)))
      (send dc set-alpha 1.0))
    (define/override (on-char evt)
      (case (send evt get-key-code)
	[(left) (cond
		  [(equal? MODE 'mode-select) (menu 'left)]
		  [(equal? MODE 'playing) (move 'left)])]
	[(up) (when (equal? MODE 'playing) (move 'forward))]
	[(right) (cond
		   [(equal? MODE 'mode-select) (menu 'right)]
		   [(equal? MODE 'playing) (move 'right)])]
	[(down) (when (equal? MODE 'playing) (move 'backward))]
	[(#\space) (cond 
		     [(equal? MODE 'mode-select) (menu 'select)]
		     [(equal? MODE 'playing) (select)])]
	[(#\return) (cond 
		      [(equal? MODE 'ready-to-play) (set! pos (hash-ref corner-hash (hash-ref turn-hash 0))) 
						    (set! rotationDEG (hash-ref rotation-hash (hash-ref turn-hash 0))) 
						    (set! turn 0)
						    (move 'for-sake-of-poised-for-move)
						    (set! MODE 'playing)]
		      [(equal? MODE 'playing) (solidify)])]
	[(escape) (when (equal? MODE 'playing) (unselect))])
      (refresh))
    (super-new)))

(new my-canvas% [parent frame])

(send frame show #t)


;   \   /
;    OOO
; --OOOOO--
;    OOO
;   /   \

;#f #f #f #f #f #f () #f #f #f #f #f #f                            O
;#f #f #f #f #f () () #f #f #f #f #f #f                           O O
;#f #f #f #f #f () () () #f #f #f #f #f                          O O O
;#f #f #f #f () () () () #f #f #f #f #f                         O O O O
;() () () () () () () () () () () () ()                O O O O O O O O O O O O O
;() () () () () () () () () () () () #f                 O O O O O O O O O O O O
;#f () () () () () () () () () () () #f                  O O O O O O O O O O O
;#f () () () () () () () () () () #f #f                   O O O O O O O O O O
;#f #f () () () () () () () () () #f #f                    O O O O O O O O O
;#f () () () () () () () () () () #f #f                   O O O O O O O O O O
;#f () () () () () () () () () () () #f                  O O O O O O O O O O O
;() () () () () () () () () () () () #f                 O O O O O O O O O O O O
;() () () () () () () () () () () () ()                O O O O O O O O O O O O O
;#f #f #f #f () () () () #f #f #f #f #f                         O O O O
;#f #f #f #f #f () () () #f #f #f #f #f                          O O O
;#f #f #f #f #f () () #f #f #f #f #f #f                           O O
;#f #f #f #f #f #f () #f #f #f #f #f #f                            O
;
;
;
; ************ GOALS *************
; * 1. Fade NPC's.            [] *
; * 2. Show turn.                *
; * 3. Add modes.                *
; * 4. Handle win scenario.      *
; * 5. Remove piece numbers.  [] *
; * 6. Allow cancel moves.    [] *
; ********************************
