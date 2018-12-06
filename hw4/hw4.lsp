; code given to parse the cnf:
(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

; input: a clause and a solution
; output: t is it satisfies
; 	  nil otherwise
; This helper function goes over the clause and the solution, and checks
; if one of the variables in the solution is equal to one of the variables
; in the clause (and will therefore satisfy the clause)
(defun check-clause (clause sol) 
	(cond ((null clause) nil)
		((check-literal sol (first clause)) t)
		(t (check-clause  (rest clause) sol))
	);end cond
);end defun

; input: a clause and a literal
; output: t if the the literal satisfies any of the literals in the clause
; 	nil otherwise
(defun check-literal (clause literal)
	(cond ((null clause) t) 
		((= (first clause) literal) t)
		((= (absnum (first clause)) (absnum literal)) nil)
		(t (check-literal (rest clause) literal))
	);end cond
);end defun


; get absolute value:
; helper function for check-literal
(defun absnum (num)
        (cond ((< num 0) (- 0 num))
                (t num)
        );end cond
);end defun


; input: all clauses, and an assunment
; output: t if it satisfies all caluses, nil otherwise
; This function goes over a clause at a time and check if the given
; asignment satisfies the clause. If any clause is not satisfied,
; return nil
(defun check-all-clauses (clauses sol) 
	(cond ((null clauses) t)
		((NOT (check-clause (first clauses) sol)) nil)
		(t (check-all-clauses (rest clauses) sol))
	);end cond
); end defun

; input: a clause (few varibales) and a sign 
; 	-1 for -, and 1 for positive
; output: the clause with another variable appended to it, or its negeation,
; depending on the sign
; This is a helper fucntion to find-sol which explores the next literal assignment
(defun add-literal (clause sign)
	(append clause (list (* sign (+ 1 (length clause)))))
)


; input: the clases, a solution, and n: the number of variables
; output: the solution for the clauses
; This algorithm starts from an empty solution, and tries to build a solution
; by adding one variable at a time (or its negative)
(defun find-sol (clauses sol n)
	(cond ((check-all-clauses clauses sol) 
		(cond ((= (length sol) n) sol)
			(t (OR (find-sol clauses (add-literal sol 1) n) (find-sol clauses (add-literal sol -1) n)))
		))
	(t nil)
	);close cond
);end defun


; Call the find-sol function from here
(defun sat? (n delta) 
  (find-sol delta '() n)
)












