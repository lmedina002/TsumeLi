;;;; Programm to solve tsume in 1 turn

(load  "e:/Mes docs/Documents/Mes documents/Cours/ENSC/2A/PII/TsumeLi/rules.lisp") ;Desktop path

;; board -> pour chaque move -> test checkmate

(let* ((board
	 (list
	  :board (make-array '(9 9)
		    :initial-contents ;Tsume 489, soluce (R 1 7) -> (R 2 7)
		      ; 0   1   2   3   4   5   6   7   8
		    '(("_" "_" "_" "_" "_" "_" "R" "_" "-J") ;0
		      ("_" "_" "_" "_" "_" "P" "_" "R" "_") ;1
		      ("_" "_" "_" "_" "_" "_" "+B" "_" "_") ;2
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;3
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;4
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;5
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;6
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;7
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_")));8
	  :drops-ally
	  (list)
	  :drops-enemy
	  (list)))
       (all (get-all-ally (getf board :board) (getf board :drops-ally))))
  (print all)
  (dolist (piece-on (getf all :moves))
    (dolist (movement (second piece-on))
      (if (checkmate (move-piece (getf board :board) (first piece-on) movement (getf piece-on :initial)) (getf board :drops-ally) (getf board :drops-enemy))
	  (format t "Piece ~A in ~A move to ~A" (first piece-on) (getf piece-on :initial) movement))))
  (dolist (piece-off (getf all :drops))
    (dolist (drop (second piece-off))
      (if (checkmate (drop-piece (getf board :board) (first piece-off) drop) (getf board :drops-ally) (getf board :drops-enemy))
	  (format t "Drop piece ~A to ~A" (first piece-off) drop)))))
