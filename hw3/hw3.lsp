;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-g/oal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
	(cond ((null s) t) ; s is empty, return true becsue we finished going over the grid
		((not (check-row-is-valid (first s))) nil )
		( t (goal-test (rest s)))
	);end cond
);end defun


; helper function to goal-state
; input: list (one row)
; output: nil if there is a keeper or a box not on goal
; true otherwise (this row is all good, continue to next one)
(defun check-row-is-valid (row)
	(cond ((null row) t) ; we are done going over the list
		((isKeeper (first row)) nil)
		((isBox (first row)) nil)
		(t (check-row-is-valid (rest row) ))
	);end cond
);end defun


; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)

 (let* ((pos (getKeeperPosition s 0))
	 (col (car pos))
	 (row (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move-up s) (try-move-down s) (try-move-left s) (try-move-right s)))
	 )
    (cleanUpList result);end
   );end let
  );

; helper function for next-state
; input: current state, next move for the keeper
; output: state if it is possible, nil otherwise
(defun try-move-up (s)
	(let* ((pos (getKeeperPosition s 0))
         (col (first pos))
         (row (second pos))
	(curr (get-value s col row))
	(up-one (get-value s col (- row 1)))
	(up-two (get-value s col (- row 2))))
	(cond ; if up-one is nil, return nil 
		((null up-one) nil) ; if the one above is nill, you can't move - return nill
		((isKeeperStar curr) ; check if the keeper is on a goal
			(cond
				((isBlank up-one) (update-3-up s row col star keeper nil))
				((isStar up-one) (update-3-up s row col star keeperstar nil))
				((isBoxStar up-one) 
					(cond 
						((null up-two) nil)
						((isBlank up-two) (update-3-up s row col star keeperstar box))
						((isStar up-two) (update-3-up s row col star keeperstar boxstar))
						(t nil)
					);close cond
				)
				((isBox up-one)
                                        (cond
						((null up-two) nil)
                                                ((isBlank up-two) (update-3-up s row col star keeper box))
                                                ((isStar up-two) (update-3-up s row col star keeper boxstar))
                                                (t nil)
                                        );close cond
				)
				(t nil)
			);close cond
		)
		(t ; else, keeper is not on goal
                        (cond
                                ((isBlank up-one) (update-3-up s row col blank keeper nil))
                                ((isStar up-one) (update-3-up s row col blank keeperstar nil))
                                ((isBoxStar up-one)
                                        (cond
						((null up-two) nil)
                                                ((isBlank up-two) (update-3-up s row col blank keeperstar box))
                                                ((isStar up-two) (update-3-up s row col blank keeperstar boxstar))
                                                (t nil)
                                        );close cond
                                )
                                ((isBox up-one)
                                        (cond
						((null up-two) nil)
                                                ((isBlank up-two) (update-3-up s row col blank keeper box))
                                                ((isStar up-two) (update-3-up s row col blank keeper boxstar))
                                                (t nil)
                                        );close cond
                                )
                                (t nil)
                        );close cond
		)
		
	
	);close cond
	);close let
);close defun


; helper function for next-state
; input: current state, next move for the keeper
; output: state if it is possible, nil otherwise
(defun try-move-down (s)
	(let* ((pos (getKeeperPosition s 0))
         (col (first pos))
         (row (second pos))
	(curr (get-value s col row))
	(down-one (get-value s col (+ row 1)))
	(down-two (get-value s col (+ row 2))))
	(cond 
		((null down-one) nil)
		((isKeeperStar curr) ; check if the keeper is on a goal
			(cond
				((isBlank down-one) (update-3-down s row col star keeper nil))
				((isStar down-one) (update-3-down s row col star keeperstar nil))
				((isBoxStar down-one) 
					(cond 
						((null down-two) nil)
						((isBlank down-two) (update-3-down s row col star keeperstar box))
						((isStar down-two) (update-3-down s row col star keeperstar boxstar))
						(t nil)
					);close cond
				)
				((isBox down-one)
                                        (cond
                                                ((null down-two) nil)
                                                ((isBlank down-two) (update-3-down s row col star keeper box))
                                                ((isStar down-two) (update-3-down s row col star keeper boxstar))
                                                (t nil)
                                        );close cond
				)
				(t nil)
			);close cond
		)
		(t 
                        (cond
                                ((isBlank down-one) (update-3-down s row col blank keeper nil))
                                ((isStar down-one) (update-3-down s row col blank keeperstar nil))
                                ((isBoxStar down-one)
                                        (cond
                                                ((null down-two) nil)
                                                ((isBlank down-two) (update-3-down s row col blank keeperstar box))
                                                ((isStar down-two) (update-3-down s row col blank keeperstar boxstar))
                                                (t nil)
                                        );close cond
                                )
                                ((isBox down-one)
                                        (cond
                                                ((null down-two) nil)
                                                ((isBlank down-two) (update-3-down s row col blank keeper box))
                                                ((isStar down-two) (update-3-down s row col blank keeper boxstar))
                                                (t nil)
                                        );close cond
                                )
                                (t nil)
                        );close cond
		)
		
	
	);close cond
	);close let
);close defun



; helper function for next-state
; input: current state, next move for the keeper
; output: state if it is possible, nil otherwise
(defun try-move-left (s)
	(let* ((pos (getKeeperPosition s 0))
         (col (first pos))
         (row (second pos))
	(curr (get-value s col row))
	(left-one (get-value s (- col 1) row))
	(left-two (get-value s (- col 2) row)))
	(cond 
		((null left-one) nil)
		((isKeeperStar curr) ; check if the keeper is on a goal
			(cond
				((isBlank left-one) (update-3-left s row col star keeper nil))
				((isStar left-one) (update-3-left s row col star keeperstar nil))
				((isBoxStar left-one);if there is a box on a goal on the left 
					(cond 
                                                ((null left-two) nil)
						((isBlank left-two) (update-3-left s row col star keeperstar box))
						((isStar left-two) (update-3-left s row col star keeperstar boxstar))
						(t nil)
					);close cond
				)
				((isBox left-one)
                                        (cond
                                                ((null left-two) nil)
                                                ((isBlank left-two) (update-3-left s row col star keeper box))
                                                ((isStar left-two) (update-3-left s row col star keeper boxstar))
                                                (t nil)
                                        );close cond
				)
				(t nil)
			);close cond
		)
		(t 
                        (cond
                                ((isBlank left-one) (update-3-left s row col blank keeper nil))
                                ((isStar left-one) (update-3-left s row col blank keeperstar nil))
                                ((isBoxStar left-one)
                                        (cond
                                                ((null left-two) nil)
                                                ((isBlank left-two) (update-3-left s row col blank keeperstar box))
                                                ((isStar left-two) (update-3-left s row col blank keeperstar boxstar))
                                                (t nil)
                                        );close cond
                                )
                                ((isBox left-one)
                                        (cond
                                                ((null left-two) nil)
                                                ((isBlank left-two) (update-3-left s row col blank keeper box))
                                                ((isStar left-two) (update-3-left s row col blank keeper boxstar))
                                                (t nil)
                                        );close cond
                                )
                                (t nil)
                        );close cond
		)
		
	
	);close cond
	);close let
);close defun



; helper function for next-state
; input: current state, next move for the keeper
; output: state if it is possible, nil otherwise
(defun try-move-right (s)
	(let* ((pos (getKeeperPosition s 0))
         (col (first pos))
         (row (second pos))
	(curr (get-value s col row))
	(right-one (get-value s (+ col 1) row))
	(right-two (get-value s (+ col 2) row)))
	(cond 
		((null right-one) nil)
		((isKeeperStar curr) ; check if the keeper is on a goal
			(cond
				((isBlank right-one) (update-3-right s row col star keeper nil))
				((isStar right-one) (update-3-right s row col star keeperstar nil))
				((isBoxStar right-one) 
					(cond 
                                                ((null right-two) nil)
						((isBlank right-two) (update-3-right s row col star keeperstar box))
						((isStar right-two) (update-3-right s row col star keeperstar boxstar))
						(t nil)
					);close cond
				)
				((isBox right-one)
                                        (cond
                                                ((null right-two) nil)
                                                ((isBlank right-two) (update-3-right s row col star keeper box))
                                                ((isStar right-two) (update-3-right s row col star keeper boxstar))
                                                (t nil)
                                        );close cond
				)
				(t nil)
			);close cond
		)
		(t 
                        (cond
                                ((isBlank right-one) (update-3-right s row col blank keeper nil))
                                ((isStar right-one) (update-3-right s row col blank keeperstar nil))
                                ((isBoxStar right-one)
                                        (cond
                                                ((null right-two) nil)
                                                ((isBlank right-two) (update-3-right s row col blank keeperstar box))
                                                ((isStar right-two) (update-3-right s row col blank keeperstar boxstar))
                                                (t nil)
                                        );close cond
                                )
                                ((isBox right-one)
                                        (cond
                                                ((null right-two) nil)
                                                ((isBlank right-two) (update-3-right s row col blank keeper box))
                                                ((isStar right-two) (update-3-right s row col blank keeper boxstar))
                                                (t nil)
                                        );close cond
                                )
                                (t nil)
                        );close cond
		)
		
	
	);close cond
	);close let
);close defun

;helper function to try move
;input: state, row and column of starting value, and then 3 values you want to update
;note: 3rd value can by nil if you only want to update 2 values
(defun update-3-up (s row col curr up1 up2)
	(cond ((null up2) (update-state (update-state s row col curr) (- row 1) col up1))
		( t (update-state (update-state (update-state s row col curr) (- row 1) col up1) (- row 2) col up2))))

(defun update-3-down (s row col curr down1 down2)
	 (cond ((null down2) (update-state (update-state s row col curr) (+ row 1) col down1))
		(t (update-state (update-state (update-state s row col curr) (+ row 1) col down1) (+ row 2) col down2))))

(defun update-3-left (s row col curr left1 left2)
	 (cond ((null left2) (update-state (update-state s row col curr) row (- col 1) left1))
		(t (update-state (update-state (update-state s row col curr) row (- col 1) left1) row (- col 2) left2))))

(defun update-3-right (s row col curr right1 right2)
	 (cond ((null right2) (update-state (update-state s row col curr)  row (+ col 1) right1))
		(t (update-state (update-state (update-state s row col curr) row (+ col 1) right1) row (+ col 2) right2))))

; helper fucntion 
; input: row and column
; output: what is in that position
; return nil if position is not possible
(defun get-value (s col row)
	(cond ((OR (< row 0) (< col 0)) nil) ; return nil if position is out of the borders
		((OR (> row (- (length s) 1)) (> col (- (length (first s)) 1))) nil) ; if they are out of the border 
		((> row 0) (get-value (rest s) col (- row 1))) ; recursively delete rows until we get to the row we need
		((= row 0) (get-value-from-list (first s) col)) ; we are at the row we need, return a list
	);close cond
);close defun


; function to update to a new state
; input: current state, new value and position using row and col
; output: new state
(defun update-state (s row col val)
	(cond ((null s) nil)
		((> row (length s)) nil)
		((> col (length (first s))) nil)
		((= row 0)  (append (list (change-row (first s) col val)) (rest s)))
		((> row 0) (append (list (first s)) (update-state (rest s) (- row 1) col val)))
	);close cond
);close defun

; helper fucntion
; input: row
; output: row with changed value in position col
(defun change-row (row col val)
	(cond ((< col 0) nil)
		((> col (- (length row) 1)) nil)
		((= col 0) (cons val (rest row)))
		( t (cons (first row) (change-row (rest row) (- col 1) val)))
	);close cond
); close defun

; helper function for get-value: traveses the list and returns the element
(defun get-value-from-list (row num)
	(cond ((= num 0) (first row))
		((> num 0) (get-value-from-list (rest row) (- num 1)))
	);close cond
); close defun




; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	(cond (t 0))
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.

; Write a heuristic function called h1 that returns the number of boxes which are not on
; goal positions in the given state. Is this heuristic admissible? (Put you answer in the header
; comment of the function)

; answer: This heuristic is addmissible becasue it returns the optimal depth
; of the tree! It it never overestimates the cost of reaching the goal.
(defun h1 (s)
	(length (find-num-boxes s))
);end defun

; helper fucntion to h1
; input: state
; output: list. delete all elements and leave only boxes. Length of this list is the number of
; boxes at that state.
; Use the helper fucntion find-num-boxes-row to construct the list
(defun find-num-boxes (s)
	(cond ((null s) nil)
		(t (cleanUpList (append (find-num-boxes-row (first s)) (find-num-boxes (rest s)))))
	);close cond
);end defun

; helper function to h1
; Get a number of boxes in each row
; input: list (one row)
; output: a list that has the length of as the number of boxes in that row
(defun find-num-boxes-row (row)
        (cond ((null row) nil) ; we are done going over the list
                (t (cleanUpList (cons (if-box-get-one (first row)) (find-num-boxes-row (rest row) ))))
        );end cond
);end defun

(defun if-box-get-one (val)
	(cond ((isBox val) val)
		(t nil)
	); close cond
); close defun

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h604955977 (s)
;	( + (all-boxes-to-all-goals (get-all-box-coordinates s 0) (get-all-goal-coordinates s 0)) (keeper-to-all-goals s (get-all-goal-coordinates s 0)))
 
	(all-boxes-to-all-goals (get-all-box-coordinates s 0) (get-all-goal-coordinates s 0))
 )


;find dispance from keeper to all goals
(defun keeper-to-all-goals (s all-goal-coor)
	(dist-box-to-all-goals (list (second (getKeeperPosition s 0)) (first (getKeeperPosition s 0))) all-goal-coor)
);end defun

; input: 2 lists that have all box coordinates and all goal coordinates
; output: total distance from all boxes to all goals 
; This is an addmissible heuristic becasue the closer we get to the goal,
; The closer we are to 0.
(defun all-boxes-to-all-goals (all-box-coor all-goal-coor)
	(cond ((null all-box-coor) 0); we finished going over boxes
		(t (+ (dist-box-to-all-goals (first all-box-coor) all-goal-coor) (all-boxes-to-all-goals (rest all-box-coor) all-goal-coor)))
	);end cond
);end defun

(defun dist-box-to-all-goals (box-coor all-goal-coor)
	(cond ((null all-goal-coor) 0); we finished going over all goals
		(t (+ (keeper-dist-2points box-coor (first all-goal-coor)) (dist-box-to-all-goals box-coor (rest all-goal-coor))))
	);end cond
);end defun


; find the distance between a box and a goal
; input: 2 lists showing coordinates of each
; output: distance (using keeper steps) from box to goal
(defun keeper-dist-2points (box-coor goal-coor)
	(+  (absnum (- (first box-coor) (first goal-coor))) (absnum (- (second box-coor) (second goal-coor)))  )
);end defun

; helper function to distance calculation 
(defun absnum (num)
	(cond ((< num 0) (- 0 num))
		(t num)
	);end cond
);end defun


; Function to get all the coordinates of a box
; Input: a state
; Output: a list of list. Each list in the list has 2 elements,
; the row and the column of the box. Length of this list is how
; many boxes are there. 
(defun get-all-box-coordinates (s row-num)
	(cond ((null s) nil) ; we are done traversing the state
		(t (append (get-box-coordinates-row (first s) row-num 0) (get-all-box-coordinates (rest s) (+ 1 row-num))))
	); end cond
);end defun

; same as the above but with goal
(defun get-all-goal-coordinates (s row-num)
        (cond ((null s) nil) ; we are done traversing the state
                (t (append (get-goal-coordinates-row (first s) row-num 0) (get-all-goal-coordinates (rest s) (+ 1 row-num))))
        ); end cond
);end defun

; helper function to get-all-box-coordinates
; Return all box coordinates in a row
(defun get-box-coordinates-row (row row-num counter)
	(cond ((null row) nil) ; we are done going over the row
		((isBox (first row)) (cons (list row-num counter) (get-box-coordinates-row (rest row) row-num (+ counter 1))))
		(t (get-box-coordinates-row (rest row) row-num (+ counter 1)))
	);close cond
);end defun

; same function as above but look for goals
(defun get-goal-coordinates-row (row row-num counter)
        (cond ((null row) nil) ; we are done going over the row
                ((isStar (first row)) (cons (list row-num counter) (get-goal-coordinates-row (rest row) row-num (+ counter 1))))
                (t (get-goal-coordinates-row (rest row) row-num (+ counter 1)))
        );close cond
);end defun


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; !!!!!!!!!  DONT FORGET TO DO THIS!

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
