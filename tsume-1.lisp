;;;; Programm to solve tsume in 1 turn

(let* ((board
	 (list
	  :board (make-array '(9 9)
		    :initial-contents ;Tsume 20, soluce (R 3 3) -> (+R 1 3)
		      ; 0   1   2   3   4   5   6   7   8
		    '(("_" "_" "_" "_" "_" "_" "+P" "_" "_") ;0
		      ("_" "_" "_" "_" "_" "-J" "_" "_" "_") ;1
		      ("_" "_" "_" "_" "_" "-P" "-P" "_" "_") ;2
		      ("_" "_" "_" "R" "_" "_" "_" "_" "_") ;3
		      ("_" "_" "B" "_" "_" "_" "_" "_" "_") ;4
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;5
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;6
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;7
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_")));8
	  :drops-ally
	  (list)
	  :drops-enemy
	  (list "-P" "-L" "-N" "-S" "-G" "-B" "-R")))
       (all (get-all-ally (getf board :board) (getf board :drops-ally))))
  (format-game board)
  (dolist (piece-on (getf all :moves))
    (dolist (movement (second piece-on))
      (if (checkmate (move-piece (getf board :board) (first piece-on) movement (getf piece-on :initial)) (getf board :drops-ally) (getf board :drops-enemy))
	  (format t "~%Piece ~A in ~A move to ~A~%" (first piece-on) (getf piece-on :initial) movement))
      (if (and (<= (first movement) 2) ;promotion available
	       (not (equal (subseq (first piece-on) 0 1) "+"))
	       (checkmate (move-piece (getf board :board) (promote (first piece-on)) movement (getf piece-on :initial)) (getf board :drops-ally) (getf board :drops-enemy)))
	  (format t "~%Piece ~A in ~A move to ~A and promote~%" (first piece-on) (getf piece-on :initial) movement))))	  
  (dolist (piece-off (getf all :drops))
    (dolist (drop (second piece-off))
      (if (checkmate (drop-piece (getf board :board) (first piece-off) drop) (remove (first piece-off) (getf board :drops-ally) :test #'equal) (getf board :drops-enemy))
	  (format t "~%Drop piece ~A to ~A~%" (first piece-off) drop)))))
