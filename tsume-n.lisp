;;;; Program to solve tsume in n-turn

;;Evaluation of a board configuration
(defun value-enemy-piece (piece)
  (let ((result 0))
    (cond ((string-equal piece "-P") (setq result (+ result 1)))
	  ((string-equal piece "-+P") (setq result (+ result 7)))
	  ((string-equal piece "-L") (setq result (+ result 3)))
	  ((string-equal piece "-N") (setq result (+ result 4)))
	  ((string-equal piece "-S") (setq result (+ result 5)))
	  ((or (string-equal piece "-G")
	       (string-equal piece "-+L")
	       (string-equal piece "-+N")
	       (string-equal piece "-+S"))
	   (setq result (+ result 6)))
	  ((string-equal piece "-B") (setq result (+ result 8)))
	  ((or (string-equal piece "-+B")
	       (string-equal piece "-R"))
	   (setq result (+ result 10)))
	  ((string-equal piece "-+R") (setq result (+ result 12))))
    (return-from value-enemy-piece result)))
  
(defun evaluation-enemy (board drops-enemy drops-ally)
  "Attribute a value to a board configuration"
  (let ((result 0))
    (when (mate board drops-ally)
      (return-from evaluation-enemy result))
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (aref board row column)))
	  (when (string-equal (subseq square 0 1) "-")
	    (setq result (+ result (value-enemy-piece square)))))))
    (dolist (piece drops-enemy)
      (setq result (+ result (value-enemy-piece piece))))
    (return-from evaluation-enemy result)))

(defun value-ally-piece (piece)
  (let ((result 0))
    (cond ((string-equal piece "P") (setq result (+ result 1)))
	  ((string-equal piece "+P") (setq result (+ result 7)))
	  ((string-equal piece "L") (setq result (+ result 3)))
	  ((string-equal piece "N") (setq result (+ result 4)))
	  ((string-equal piece "S") (setq result (+ result 5)))
	  ((or (string-equal piece "G")
	       (string-equal piece "+L")
	       (string-equal piece "+N")
	       (string-equal piece "+S"))
	   (setq result (+ result 6)))
	  ((string-equal piece "B") (setq result (+ result 8)))
	  ((or (string-equal piece "+B")
	       (string-equal piece "R"))
	   (setq result (+ result 10)))
	  ((string-equal piece "+R") (setq result (+ result 12))))
    (return-from value-ally-piece result)))
  
(defun evaluation-ally (board drops-ally)
  "Attribute a value to a board configuration"
  (let ((result 0))
    (when (mate board drops-ally)
      (setq result (+ result 50)))
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (aref board row column)))
	  (when (and (string-not-equal (subseq square 0 1) "-")
		     (string-not-equal square "_"))
	    (setq result (+ result (value-ally-piece square)))))))
    (dolist (piece drops-ally)
      (setq result (+ result (value-ally-piece piece))))
    (return-from evaluation-ally result)))

;; Main
(defun main (n full-board)
  (let ((drops-ally (getf full-board :drops-ally))
	(drops-enemy (getf full-board :drops-enemy))
	(board (getf full-board :board)))
    (dotimes (k n)
      (format-board (list :board board :drops-ally drops-ally :drops-enemy drops-enemy))
      (cond ((evenp k) ;Ally turn
	     (let ((all (get-all-ally board drops-ally))
		   (min-eval)
		   (play))
	       (dolist (piece-on (getf all :moves))
		 (dolist (movement (second piece-on))
		   (when (< (evaluation-enemy (move-piece board (first piece-on) movement (getf piece-on :initial)) drops-enemy drops-ally) min-eval) ;Pb ajout pieces prises
		     (progn
		       (setq min-eval (evaluation-enemy (move-piece board (first piece-on) movement (getf piece-on :initial)) drops-enemy drops-ally))
		       (setq play (list :piece (first piece-on) :movement movement :initial (getf piece-on :initial) :type "move"))))))
	       (dolist (piece-off (getf all :drops))
		 (dolist (drop (second piece-off))
		   (when (< (evaluation-enemy (drop-piece board (first piece-off) drop) drops-enemy (remove (first piece-off) drops-ally :test #'equal)) min-eval)
		     (progn
		       (setq min-eval (evaluation-enemy (drop-piece board (first piece-off) drop) drops-enemy (remove (first piece-off) drops-ally :test #'equal)))
		       (setq play (list :piece (first piece-off) :movement drop :type "drop"))))))
	       (if (string-equal (getf play :type) "move") ;on joue le min
		   (progn
		     (if (string-equal (subseq (aref board (first (getf play :movement)) (second (getf play :movement))) 0 1) "-")
			 (push drops-ally (subseq (aref board (first (getf play :movement)) (second (getf play :movement))) 1)))
		     (setq board (move-piece board (getf play :piece) (getf play :movement) (getf play :initial)))) ;move-piece		     		     
		   (progn
		     (setq board (drop-piece board (getf play :piece) (getf play :location)))
		     (setq drops-ally (remove (getf play :piece) drops-ally :test #'equal)))))) ;drop-piece
	    ((oddp k) ;Enemy turn
	     (let ((all (get-all-enemy board drops-enemy))
		   (max-eval)
		   (play))
	       (dolist (piece-on (getf all :moves))
		 (dolist (movement (second piece-on))
		   (when (> (evaluation-enemy (move-piece board (first piece-on) movement (getf piece-on :initial)) drops-enemy drops-ally) max-eval) ;Pb ajout pieces prises
		     (progn
		       (setq max-eval (evaluation-enemy (move-piece board (first piece-on) movement (getf piece-on :initial)) drops-enemy drops-ally))
		       (setq play (list :piece (first piece-on) :movement movement :initial (getf piece-on :initial) :type "move"))))))
	       (dolist (piece-off (getf all :drops))
		 (dolist (drop (second piece-off))
		   (when (> (evaluation-enemy (drop-piece board (first piece-off) drop) (remove (first piece-off) drops-enemy :test #'equal) drops-ally) max-eval)
		     (progn
		       (setq max-eval (evaluation-enemy (drop-piece board (first piece-off) drop) (remove (first piece-off) drops-enemy :test #'equal) drops-ally))
		       (setq play (list :piece (first piece-off) :movement drop :type "drop"))))))
	       (if (string-equal (getf play :type) "move") ;on joue le max
		   (progn
		     (if (string-not-equal (aref board (first (getf play :movement)) (second (getf play :movement))) "_")
			 (push drops-enemy (concatenate 'string "-" (aref board (first (getf play :movement)) (second (getf play :movement))))))
		     (setq board (move-piece board (getf play :piece) (getf play :movement) (getf play :initial)))) ;move-piece		     		     
		   (progn
		     (setq board (drop-piece board (getf play :piece) (getf play :location)))
		     (setq drops-enemy (remove (getf play :piece) drops-enemy :test #'equal)))))))))) ;drop-piece
	     
		  
