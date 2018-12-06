; NAME: Nina Maller
; EMAIL: nina.maller@gmail.com


; Problem 1:
; input: 2 arguments, N is a number, and TREE is an ordered tree
; output: T if number N appears in the ordered tree TREE
; 	  NIL if number N does not appear in the ordered tree TREE
; algorithm: First, I check if the tree is an atom. If it is, compare
; it to the value N and return t or nil. Then, I check if the number
; is greater or smaller than the center of the tree, and recursively
; call the function again with either the left or right sides of the
; tree.
(defun TREE-CONTAINS (N TREE)
	(cond ((atom TREE) (equal N TREE))
		((equal N (second TREE)) t)
		((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
		( t  (TREE-CONTAINS N (third TREE)))))


; Problem 2:
; input: one argument, TREE is an ordered tree
; output: The minimum number appearing in the ordered tree TREE
; algorithm: check if TREE is an atom, if it is then return it.
; Otherwise, recurse to the leftmost element of the tree until
; you get an atom and return that.
(defun TREE-MIN (TREE)
	(cond ((atom TREE) TREE)
		(t (TREE-MIN (first TREE))))) 


; Problem 3:
; input: one argument, TREE is an ordered tree
; output: pre-ordered list of the number in the ordered tree TREE
; algorithm: base case checks if there is only one element in
; TREE and returns it as a list. Then, recursively append the first
; the middle element, then the left element, then the right one.
(defun TREE-ORDER (TREE)
	(cond ((atom TREE) (cons TREE NIL)) ; return value should be a list
		(t (cons (second TREE) (append (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE)))))))

; Problem 4:
; input: list L, 2 non-negative integers START and LEN
; output: sub-list of L that starts at position START and has lenth L
; algorithm: base case checks if length is 0, and returns the empty list.
; Then remove the first element until starting index is 0. Lastly, 
; continue recursing by appending to a new list until length is 0.
(defun SUB-LIST (L START LEN)
	(cond ((= LEN 0) NIL)
		((> START 0) (SUB-LIST (rest L) (- START 1) LEN))
		(t (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))))

; Problem 5:
; input: list L
; output: two lists, L1 and L2 such that L is the result of appending 
; L1 and L2, and length of L1 minus length of L2 is 0 or 1
; algorithm: there are two cases here. The first is that the length is 
; even, and the second is that the length is odd. There are adjusted
; starting and ending points to each one.
(defun SPLIT-LIST (L)
	(cond ((evenp (length L)) (list (SUB-LIST L 0 (/ (length L) 2)) (SUB-LIST L (/ (length L) 2) (/ (length L) 2))))
		(t (list (SUB-LIST L 0 (/ (+ (length L) 1) 2)) (SUB-LIST L (/ (+ (length L) 1) 2) (/ (- (length L) 1) 2))))))

; Problem 6:
; input: binary tree TREE
; output: height of binary tree TREE
; algorithm: base case is if we are looking at a leaf node, so we
; reaturn 0. Otherwise, check if the left or right is portions of the
; binary tree is bigger, and return 1 + the length of that
(defun BTREE-HEIGHT (TREE)
	(cond ((atom TREE) 0)
		((> (BTREE-HEIGHT (first TREE)) (BTREE-HEIGHT (second TREE))) (+ 1 (BTREE-HEIGHT (first TREE))))
		( t (+ 1 (BTREE-HEIGHT (second TREE))))))

; Problem 7:
; input: a non-empty list of atoms LEAVES
; output: binary tree such that the tree leaves are the elements of
; LEAVES, for any internal (non-leaf) node in the tree, the number of
; leaves in its left branch minus the number of leaves in its right 
; branch is 0 or 1.
; algorithm: Base cases: if the length is 1, return the element as not a
; list. If there are 2 elements, put them in a list and return. The 
; recursive case recursively splits the list to two and make a list from
; the newly created lists.
(defun LIST2BTREE (LEAVES)
	(cond ((equal (length LEAVES) 1) (first LEAVES))
		((equal (length LEAVES) 2) (list (first LEAVES) (second LEAVES))) 
		(t (list (LIST2BTREE (first (SPLIT-LIST LEAVES))) (LIST2BTREE (second (SPLIT-LIST LEAVES))))))) 


; Problem 8: 
; input: a binary tree TREE
; output: list of atoms such that it is the inverse of LIST2BTREE
; algorithm: first handle the case where there is only one element, and return
; it as a list. Then, base case check if the first element is an atom, if it is,
; we can stop. Recursive case recursively appends both halves of the list 
; until the base case is fulfilled.
; (If the list has an odd number of number, we make the last element a list
; and not an atom in order to use append)
(defun BTREE2LIST (TREE)   
	(cond ((NOT (listp TREE)) (cons TREE nil)) ; if TREE is one element, return it as a list...
		((atom (first TREE)) TREE) ; base case: if the first item is an atom, stop
		((atom (second TREE)) (append (BTREE2LIST (first TREE)) (BTREE2LIST (rest TREE))))
		(t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))))

; Problem 9:
; input: two LISP expressions E1 and E2 whose atoms are all numbers
; output: T if E1 and E2 are identical
; 	  NIL otherwise
; algorithm: handle all weird cases first (if both are nil, if only one is nil,
; if only one of them is an atom). If they are both atoms, return either true
; or false, depending on if they are equal (base case). Else, recursively check
; if first atom if equal, and if it is, check if the rest is equal.
(defun IS-SAME (E1 E2)
	(cond ((NOT (OR E1 E2)) t) ; if both are nil, return true
		((NOT (AND E1 E2)) NIL) ; if one of them is nil, return false
		((AND (atom E1) (atom E2)) (= E1 E2)) ; if they are both atoms, check if they are equal
		((OR (atom E1) (atom E2)) NIL) ; if one of them is an atom, return false (can't be both)
		((IS-SAME (first E1) (first E2)) (IS-SAME (rest E1) (rest E2)))
		(t NIL)))












