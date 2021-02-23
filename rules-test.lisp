;;;; Run tests for the functions in the file rules.lisp
;(load "~/TsumeLi/rules.lisp")  ;Laptop path
(load  "e:/Mes docs/Documents/Mes documents/Cours/ENSC/2A/PII/TsumeLi/rules.lisp") ;Desktop path

(format t "~%------------- Début ---------------~%~%Test des get-move-...-ally:~%")

(let ((board (make-array '(9 9)
	 :initial-contents
	 ;   0   1   2   3   4   5   6   7   8
	 '(("L" "+B" "_" "_" "_" "_" "_" "_" "_") ;0
	   ("_" "_" "N" "_" "_" "_" "_" "_" "_") ;1
	   ("_" "_" "-P" "P" "_" "_" "L" "_" "R") ;2
	   ("_" "_" "_" "_" "_" "_" "_" "-J" "S") ;3
	   ("_" "_" "_" "_" "B" "_" "P" "P" "_") ;4
	   ("_" "_" "L" "L" "_" "-P" "_" "_" "_") ;5
	   ("_" "_" "_" "_" "_" "_" "K" "_" "-P") ;6
	   ("R" "_" "_" "_" "N" "_" "P" "_" "G") ;7
	   ("-P" "_" "_" "+R" "_" "_" "_" "_" "P")))));8
  (cond ((and (equal (get-move-pawn-ally 4 6 board) '((3 6))) (string-equal (get-move-pawn-ally 7 6 board) "+P"))
	 (format t "~%get-move-pawn-ally: OK"))
	(t (format t "~%get-move-pawn-ally: NIL")))
  (cond ((and (equal (set-difference (get-move-lance-ally 5 2 board)
				     (list '(4 2) '(3 2) '(2 2)) :test #'equal)
		     NIL)
	      (equal (set-difference (get-move-lance-ally 5 3 board)
				     (list '(4 3) '(3 3)) :test #'equal)
		     NIL)
	      (string-equal (get-move-lance-ally 0 0 board) "+L"))
	 (format t "~%get-move-lance-ally: OK"))
	(t (format t "~%get-move-lance-ally: NIL")))
  (cond ((and (equal (get-move-knight-ally 7 4 board)
		     '((5 5)))
	      (string-equal (get-move-knight-ally 1 2 board) "+N"))
	 (format t "~%get-move-knight-ally: OK"))
	(t (format t "~%get-move-knight-ally: NIL")))
  (cond ((equal (set-difference (get-move-king-ally 6 6 board)
				(list '(5 5) '(5 6) '(5 7) '(6 5) '(6 7) '(7 5) '(7 7)) :test #'equal)
		NIL)
	 (format t "~%get-move-king-ally: OK"))
	(t (format t "~%get-move-king-ally: NIL")))
  (cond ((equal (set-difference (get-move-rook-ally 7 0 board)
				(list '(8 0) '(7 1) '(7 2) '(7 3) '(6 0) '(5 0) '(4 0) '(3 0) '(2 0) '(1 0)) :test #'equal)
		NIL)
	 (format t "~%get-move-rook-ally: OK"))
	(t (format t "~%get-move-rook-ally: NIL")))
  (cond ((equal (set-difference (get-move-bishop-ally 4 4 board)
				(list '(5 5) '(3 5) '(3 3) '(2 2)) :test #'equal)
		NIL)
	 (format t "~%get-move-bishop-ally: OK"))
	(t (format t "~%get-move-bishop-ally: NIL")))
  (cond ((equal (set-difference (get-move-silver-ally 3 8 board)
				(list '(2 7)) :test #'equal)
		NIL)
	 (format t "~%get-move-silver-ally: OK"))
	(t (format t "~%get-move-silver-ally: NIL")))
  (cond ((equal (set-difference (get-move-gold-ally 7 8 board)
				(list '(6 8) '(6 7) '(7 7)) :test #'equal)
		NIL)
	 (format t "~%get-move-gold-ally: OK"))
	(t (format t "~%get-move-gold-ally: NIL")))
  (cond ((equal (set-difference (get-move-horse-ally 0 1 board)
				(list '(1 0) '(1 1) '(0 2)) :test #'equal)
		NIL)
	 (format t "~%get-move-horse-ally: OK"))
	(t (format t "~%get-move-horse-ally: NIL")))
  (cond ((equal (set-difference (get-move-dragon-ally 8 3 board)
				(list '(8 0) '(8 1) '(8 2) '(8 4) '(8 5) '(8 6) '(8 7) '(7 3) '(6 3) '(7 2)) :test #'equal)
		NIL)
	 (format t "~%get-move-dragon-ally: OK"))
	(t (format t "~%get-move-dragon-ally: NIL"))))

(format t "~%~%~%Test des get-move-...-enemy:~%")

(let ((board (make-array '(9 9)
	 :initial-contents
	 ;   0   1   2   3   4   5   6   7   8
	 '(("_" "_" "_" "_" "-+B" "_" "_" "-P" "_") ;0
	   ("_" "-B" "_" "_" "_" "_" "_" "_" "-S") ;1
	   ("_" "_" "-P" "_" "_" "-R" "_" "_" "_") ;2
	   ("_" "_" "P" "_" "_" "-L" "_" "-G" "_") ;3
	   ("_" "_" "_" "_" "_" "_" "_" "P" "-P") ;4
	   ("_" "_" "_" "_" "-N" "_" "_" "_" "_") ;5
	   ("_" "-L" "_" "_" "_" "_" "-K" "_" "-+R") ;6
	   ("_" "P" "_" "-N" "_" "_" "-P" "_" "_") ;7
	   ("_" "_" "_" "_" "_" "-P" "_" "_" "-L")))));8
  (cond ((and (equal (get-move-pawn-enemy 2 3 board) '((3 3))) (string-equal (get-move-pawn-enemy 8 5 board) "-+P"))
	 (format t "~%get-move-pawn-enemy: OK"))
	(t (format t "~%get-move-pawn-enemy: NIL")))
  (cond ((and (equal (set-difference (get-move-lance-enemy 3 5 board)
				     (list '(4 5) '(5 5) '(6 5) '(7 5)) :test #'equal)
		     NIL)
	      (equal (set-difference (get-move-lance-enemy 6 1 board)
				     (list '(7 1)) :test #'equal)
		     NIL)
	      (string-equal (get-move-lance-enemy 8 8 board) "-+L"))
	 (format t "~%get-move-lance-enemy: OK"))
	(t (format t "~%get-move-lance-enemy: NIL")))
  (cond ((and (equal (get-move-knight-enemy 5 4 board)
		     '((7 5)))
	      (string-equal (get-move-knight-enemy 7 3 board) "-+N"))
	 (format t "~%get-move-knight-enemy: OK"))
	(t (format t "~%get-move-knight-enemy: NIL")))
  (cond ((equal (set-difference (get-move-king-enemy 6 6 board)
				(list '(5 5) '(5 6) '(5 7) '(6 5) '(6 7) '(7 5) '(7 7)) :test #'equal)
		NIL)
	 (format t "~%get-move-king-enemy: OK"))
	(t (format t "~%get-move-king-enemy: NIL")))
  (cond ((equal (set-difference (get-move-rook-enemy 2 5 board)
				(list '(2 6) '(2 7) '(2 8) '(1 5) '(0 5) '(2 4) '(2 3)) :test #'equal)
		NIL)
	 (format t "~%get-move-rook-enemy: OK"))
	(t (format t "~%get-move-rook-enemy: NIL")))
  (cond ((equal (set-difference (get-move-bishop-enemy 1 1 board)
				(list '(0 0) '(0 2) '(2 0)) :test #'equal)
		NIL)
	 (format t "~%get-move-bishop-enemy: OK"))
	(t (format t "~%get-move-bishop-enemy: NIL")))
  (cond ((equal (set-difference (get-move-silver-enemy 1 8 board)
				(list '(2 7) '(2 8)) :test #'equal)
		NIL)
	 (format t "~%get-move-silver-enemy: OK"))
	(t (format t "~%get-move-silver-enemy: NIL")))
  (cond ((equal (set-difference (get-move-gold-enemy 3 7 board)
				(list '(3 8) '(2 7) '(3 6) '(4 7) '(4 6)) :test #'equal)
		NIL)
	 (format t "~%get-move-gold-enemy: OK"))
	(t (format t "~%get-move-gold-enemy: NIL")))
  (cond ((equal (set-difference (get-move-horse-enemy 0 4 board)
				(list '(0 3)'(0 5)'(1 4) '(1 3) '(1 5) '(2 6)) :test #'equal)
		NIL)
	 (format t "~%get-move-horse-enemy: OK"))
	(t (format t "~%get-move-horse-enemy: NIL")))
  (cond ((equal (set-difference (get-move-dragon-enemy 6 8 board)
				(list '(7 8) '(5 8) '(5 7) '(7 7) '(6 7)) :test #'equal)
		NIL)
	 (format t "~%get-move-dragon-enemy: OK"))
	(t (format t "~%get-move-dragon-enemy: NIL"))))




  
      
