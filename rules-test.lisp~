;;;; Run tests for the functions in the file rules.lisp
(load "~/TsumeLi/rules.lisp")

(format t "~%------------- Début ---------------~%~%Test des get-move-...-ally:~%")

(let ((board (make-array '(9 9)
	 :initial-contents
	 ; 0   1   2   3   4   5   6   7   8
	 '(("L" "_" "_" "_" "_" "_" "_" "_" "_") ;0
	 ("_" "_" "N" "_" "_" "_" "_" "_" "_") ;1
	 ("_" "_" "-P" "P" "_" "_" "L" "_" "R") ;2
	 ("_" "_" "_" "_" "_" "_" "_" "-J" "S") ;3
	 ("_" "_" "_" "_" "B" "_" "P" "P" "_") ;4
	 ("_" "_" "L" "L" "_" "-P" "_" "_" "_") ;5
	 ("_" "_" "_" "_" "_" "_" "K" "_" "-P") ;6
	 ("R" "_" "_" "_" "N" "_" "P" "_" "G") ;7
	 ("-P" "_" "_" "_" "_" "_" "_" "_" "P")))));8
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
	(t (format t "~%get-move-gold-ally: NIL"))))
; todo: dragon + horse



  
      
