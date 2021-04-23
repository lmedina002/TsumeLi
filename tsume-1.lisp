;;;; Programm to solve tsume in 1 turn
(in-package :tsumeli)

(let* ((board
	 (list
	  :board (make-array '(9 9)
		    :initial-contents ;Tsume 486, soluce (+B 0 3) -> (+B 3 0) | (-J 0 2) -> (-J 0 3) | (G 3 1)
		      ; 0   1   2   3   4   5   6   7   8
		    '(("_" "_" "_" "_" "_" "_" "_" "_" "_") ;0
		      ("-J" "-P" "_" "_" "_" "_" "_" "_" "_") ;1
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;2
		      ("+B" "_" "+B" "_" "_" "_" "_" "_" "_") ;3
		      ("-P" "_" "_" "_" "_" "_" "_" "_" "_") ;4
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;5
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;6
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;7
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_")));8
	  :drops-ally
	  (list "G")
	  :drops-enemy
	  (list "-P" "-L" "-N" "-S" "-G" "-R")))
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
