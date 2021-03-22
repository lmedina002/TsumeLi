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
    (when (checkmate board drops-ally drops-enemy)
      (return-from evaluation-enemy -100))
    (when (mate board drops-ally)
      (return-from evaluation-enemy 0))
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
			 (push (concatenate 'string "-" (aref board (first (getf play :movement)) (second (getf play :movement)))) drops-enemy))
		     (setq board (move-piece board (getf play :piece) (getf play :movement) (getf play :initial)))) ;move-piece		     		     
		   (progn
		     (setq board (drop-piece board (getf play :piece) (getf play :location)))
		     (setq drops-enemy (remove (getf play :piece) drops-enemy :test #'equal)))))))))) ;drop-piece

(defun minimax (n full-board)
  (let ((drops-ally (getf full-board :drops-ally))
	(drops-enemy (getf full-board :drops-enemy))
	(board (getf full-board :board))
	(result))
    (cond ((or (= n 0)(checkmate board drops-ally drops-enemy)) ;Ally final turn end of recursion evaluation of the board
	   (let ((max-eval))
	     (setq max-eval (evaluation-enemy board drops-enemy drops-ally))
	     (setq result (list :score max-eval :board board :drops-ally drops-ally :drops-enemy drops-enemy))
	     (return-from minimax result)))
	  ((oddp n)
	   (let ((all (get-all-ally board drops-ally))
		   (min-eval))
	       (dolist (piece-on (getf all :moves))
		 (dolist (movement (second piece-on))
		   (if (string-equal (subseq (aref board (first movement) (second movement)) 0 1) "-")
		       (let ((new-drops (copy-list drops-ally)))
			 (push (minimax
				(- n 1)
				(list :board (move-piece board (first piece-on) movement (getf piece-on :initial))    
				      :drops-enemy drops-enemy
				      :drops-ally (push (subseq (aref board (first movement) (second movement)) 1) new-drops)))
			     min-eval))
		       (push (minimax
			      (- n 1)
			      (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
				    :drops-enemy drops-enemy
				    :drops-ally drops-ally))
			     min-eval))))
	       (dolist (piece-off (getf all :drops))
		 (dolist (drop (second piece-off))
		   (push (minimax
			  (- n 1)
			  (list :board (drop-piece board (first piece-off) drop)
				:drops-enemy drops-enemy
				:drops-ally (remove (first piece-off) (copy-list drops-ally) :test #'equal)))
			 min-eval)))
	     (return-from minimax (get-element min-eval #'< :score))))
	  ((evenp n)
	   (let ((all (get-all-enemy board drops-enemy))
		   (max-eval))
	       (dolist (piece-on (getf all :moves))
		 (dolist (movement (second piece-on))
		   (if (string-not-equal (aref board (first movement) (second movement)) "_")
		       (let ((new-drops (copy-list drops-enemy)))
			 (push (minimax
				(- n 1)
				(list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
				      :drops-enemy (push (concatenate 'string "-" (subseq (aref board (first movement) (second movement)) 1)) new-drops)
				      :drops-ally drops-ally))
			     max-eval))
		       (push (minimax
			      (- n 1)
			      (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
				    :drops-enemy drops-enemy
				    :drops-ally drops-ally))
			     max-eval))))
	       (dolist (piece-off (getf all :drops))
		 (dolist (drop (second piece-off))
		   (push (minimax
			  (- n 1)
			  (list :board (drop-piece board (first piece-off) drop)
				:drops-enemy (remove (first piece-off) (copy-list drops-enemy) :test #'equal)
				:drops-ally drops-ally))
			 max-eval)))
	     (return-from minimax (get-element max-eval #'> :score)))))
					;Todo: affichage terminal des états choisis
    ))

;;---------------------Alpha-beta pruning ---------------------------------------------

(defun alphabeta (n full-board alpha beta)
  (let ((drops-ally (getf full-board :drops-ally))
	(drops-enemy (getf full-board :drops-enemy))
	(board (getf full-board :board))
	(result))
    (cond ((or (= n 0) (checkmate board drops-ally drops-enemy)) ;Ally final turn or checkmate (terminal node)
	   (let ((max-eval))
	     (setq max-eval (evaluation-enemy board drops-enemy drops-ally))
	     (setq result (list :score max-eval :board board :drops-ally drops-ally :drops-enemy drops-enemy))
	     (return-from alphabeta result)))
	  ((oddp n)
	   (let ((all (get-all-ally board drops-ally))
		 (min-eval 200)
		 (variation))
	       (dolist (piece-on (getf all :moves))
		 (dolist (movement (second piece-on))
		   (if (string-equal (subseq (aref board (first movement) (second movement)) 0 1) "-")
		       (let ((new-drops (copy-list drops-ally)))
			 (if (> min-eval
				(getf (alphabeta
				       (- n 1)
				       (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))    
					     :drops-enemy drops-enemy
					     :drops-ally (push (subseq (aref board (first movement) (second movement)) 1) new-drops))
				       alpha
				       beta)
				      :score))
			     (progn
			       (setq variation (alphabeta
						(- n 1)
						(list :board (move-piece board (first piece-on) movement (getf piece-on :initial))    
						      :drops-enemy drops-enemy
						      :drops-ally (push (subseq (aref board (first movement) (second movement)) 1) new-drops))
						alpha
						beta))
			       
			       (setq min-eval (getf variation :score))))))			     
		   (if (> min-eval
			  (getf (alphabeta
				 (- n 1)
				 (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))    
				       :drops-enemy drops-enemy
				       :drops-ally drops-ally)
				 alpha
				 beta)
				:score))
		       (progn
			 (setq variation (alphabeta
					  (- n 1)
					  (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))    
						:drops-enemy drops-enemy
						:drops-ally drops-ally)
					  alpha
					  beta))			 
			 (setq min-eval (getf variation :score))))
		   (setq beta (min beta min-eval))
		   (if (<= beta alpha)
		       (return-from alphabeta variation))))
	       (dolist (piece-off (getf all :drops))
		 (dolist (drop (second piece-off))
		   (if (> min-eval 
			  (getf (alphabeta
				 (- n 1)
				 (list :board (drop-piece board (first piece-off) drop)
				       :drops-enemy drops-enemy
				       :drops-ally (remove (first piece-off) (copy-list drops-ally) :test #'equal))
				 alpha
				 beta)
				:score))
		       (progn
			 (setq variation (alphabeta
					  (- n 1)
					  (list :board (drop-piece board (first piece-off) drop)
						:drops-enemy drops-enemy
						:drops-ally (remove (first piece-off) (copy-list drops-ally) :test #'equal))
					  alpha
					  beta))
			 
			 (setq min-eval (getf variation :score))))
		   (setq beta (min beta min-eval))
		   (if (<= beta alpha)
		       (return-from alphabeta variation))))
	     (return-from alphabeta variation)))
	  ((evenp n)
	   (let ((all (get-all-enemy board drops-enemy))
		 (max-eval -200)
		 (variation))
	       (dolist (piece-on (getf all :moves))
		 (dolist (movement (second piece-on))
		   (if (string-not-equal (aref board (first movement) (second movement)) "_")
		       (let ((new-drops (copy-list drops-enemy)))
			 (if (< max-eval
				(getf (alphabeta
				       (- n 1)
				       (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
					     :drops-enemy (push (concatenate 'string "-" (subseq (aref board (first movement) (second movement)) 1)) new-drops)
					     :drops-ally drops-ally)
				       alpha
				       beta)
				      :score))
			     (progn
			       (setq variation (alphabeta
						(- n 1)
						(list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
						      :drops-enemy (push (concatenate 'string "-" (subseq (aref board (first movement) (second movement)) 1)) new-drops)
						      :drops-ally drops-ally)
						alpha
						beta))
			       (setq max-eval (getf variation :score)))))
		       (if (< max-eval
				(getf (alphabeta
				       (- n 1)
				       (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
					     :drops-enemy drops-enemy
					     :drops-ally drops-ally)
				       alpha
				       beta)
				      :score))
			     (progn
			       (setq variation (alphabeta
						(- n 1)
						(list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
						      :drops-enemy drops-enemy
						      :drops-ally drops-ally)
						alpha
						beta))
			       (setq max-eval (getf variation :score)))))
		   (setq alpha (max alpha max-eval))
		   (if (>= alpha beta)
		       (return-from alphabeta variation))))
	       (dolist (piece-off (getf all :drops))
		 (dolist (drop (second piece-off))
		   (if (< max-eval
			  (getf (alphabeta
				 (- n 1)
				 (list :board (drop-piece board (first piece-off) drop)
				       :drops-enemy (remove (first piece-off) (copy-list drops-enemy) :test #'equal)
				       :drops-ally drops-ally)
				 alpha
				 beta)
				:score))
		       (progn
			 (setq variation (alphabeta
					  (- n 1)
					  (list :board (drop-piece board (first piece-off) drop)
						:drops-enemy (remove (first piece-off) (copy-list drops-enemy) :test #'equal)
						:drops-ally drops-ally)
					  alpha
					  beta))
			 (setq max-eval (getf variation :score))))
		   (setq alpha (max alpha max-eval))
		   (if (>= alpha beta)
		       (return-from alphabeta variation))))
	     (return-from alphabeta variation))))))
					
		  
