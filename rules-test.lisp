;;;; Run tests for the functions in the file rules.lisp
(load "~/TsumeLi/rules.lisp")

(let ((board (make-array '(9 9)
	 :initial-contents
	 ;  0   1   2   3   4   5   6   7   8
	 '(("_" "_" "_" "_" "_" "_" "_" "_" "_") ;0
	 ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;1
	 ("_" "_" "_" "_" "_" "_" "L" "_" "R") ;2
	 ("_" "_" "_" "_" "_" "_" "_" "-J" "S") ;3
	 ("_" "_" "_" "_" "_" "_" "P" "P" "_") ;4
	 ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;5
	 ("_" "_" "_" "_" "_" "_" "K" "_" "_") ;6
	 ("_" "_" "_" "_" "_" "_" "P" "_" "_") ;7
	   ("_" "_" "_" "_" "_" "_" "_" "_" "_")))));8
  (cond ((and (equal (get-move-pawn-ally 4 6 board) '((3 6))) (string-equal (get-move-pawn-ally 7 6 board) "+P"))
	 (print "get-move-pawn-ally: OK"))
	(t (print "get-move-pawn-enemy: NIL"))))
  
      
