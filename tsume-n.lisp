;;;; Program to solve tsume in n-turn

;;;------------------ Auxiliary functions for board evaluation -----------------------------

;; Raw

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
      (setf result (- result 100)))
    (when (mate board drops-ally)
      (setf result (- result 20)))
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (aref board row column)))
	  (when (string-equal (subseq square 0 1) "-")
	    (setf result (+ result (value-enemy-piece square)))))))
    (dolist (piece drops-enemy)
      (setf result (+ result (value-enemy-piece piece))))
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


;; Ponderation with distance
(defun distance-to-jewel (board row column)
  (let* ((j-position (find-piece "-J" board))	 
	 (result))
    
    (if (not (equal j-position nil))
	(let* ((coefficient 1)
	       (euclidian (sqrt (+ (expt (- row (first j-position)) 2) (expt (- column (second j-position)) 2))))
	       (euclidian-max (sqrt (+ (expt (- 0 8) 2) (expt (- 0 8) 2)))))
	  (setf result (* (/ (- euclidian-max euclidian) euclidian-max) coefficient)))
	
	(setf result 0))))	 
   
(defun ponderate-evaluation-enemy (board drops-enemy drops-ally)
  "Attribute a value to a board configuration"
  (let ((result 0))
    (when (checkmate board drops-ally drops-enemy)
      (setf result (- result 100)))
    
    (when (mate board drops-ally)
      (setf result (- result 20)))
    
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (aref board row column)))
	  (when (string-equal (subseq square 0 1) "-")
	    (if (or (string-equal square "-B")
		    (string-equal square "-R")
		    (string-equal square "-+B")
		    (string-equal square "-+R")
		    (string-equal square "-J"))
		;; No ponderation for these 4 pieces    
		(setf result (+ result (value-enemy-piece square)))
		
		(setf result (* (+ result (value-enemy-piece square)) (distance-to-jewel board row column))))))))
	  
    (dolist (piece drops-enemy)
      (setf result (+ result (value-enemy-piece piece))))
	  
    (return-from ponderate-evaluation-enemy result)))

(defun ponderate-evaluation-enemy-full (full-board)
  (let ((drops-ally (getf full-board :drops-ally))
	(drops-enemy (getf full-board :drops-enemy))
	(board (getf full-board :board)))
    (ponderate-evaluation-enemy board drops-enemy drops-ally)))

(defun ponderate-evaluation-ally (board drops-ally)
  "Attribute a value to a board configuration"
  (let ((result 0))
    (when (mate board drops-ally)
      (setq result (+ result 50)))
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (aref board row column)))
	  (when (and (string-not-equal (subseq square 0 1) "-")
		     (string-not-equal square "_"))
	    (if (or (string-equal square "B")
		    (string-equal square "R")
		    (string-equal square "+B")
		    (string-equal square "+R"))
		;; No ponderation		
		(setf result (+ result (value-ally-piece square)))

		(setf result (* (+ result (value-ally-piece square)) (distance-to-jewel board row column))))))))
    
    (dolist (piece drops-ally)
      (setq result (+ result (value-ally-piece piece))))
    (return-from ponderate-evaluation-ally result)))

  

;;;--------------------- Evaluation functions -------------------------------

(defun evaluation-enemy-full (full-board)
  "Evaluation function, consider only enemy piece quality and quantity"
  (let ((drops-ally (getf full-board :drops-ally))
	(drops-enemy (getf full-board :drops-enemy))
	(board (getf full-board :board)))
    (evaluation-enemy board drops-enemy drops-ally)))

(defun raw-material-balance (full-board)
  "Evaluation function, consider only piece quality and quantity from each side" 
  (let ((drops-ally (getf full-board :drops-ally))
	(drops-enemy (getf full-board :drops-enemy))
	(board (getf full-board :board))
	(result 0))
    ;;; Raw material balance
    (setf result (+ result (evaluation-enemy board drops-enemy drops-ally)))
    (setf result (- result (evaluation-ally board drops-ally)))))


(defun ponderate-material-balance (full-board)
  "Evaluation function, pieces values are modified according to their distance to jewel"
  (let ((drops-ally (getf full-board :drops-ally))
	(drops-enemy (getf full-board :drops-enemy))
	(board (getf full-board :board))
	(result 0))
    ;;; Ponderate material balance
    (setf result (+ result (ponderate-evaluation-enemy board drops-enemy drops-ally)))
    (setf result (- result (ponderate-evaluation-ally board drops-ally)))))
    
;;;--------------------- Minimax -----------------------------------
    
(defun minimax (n full-board)
  (let ((drops-ally (getf full-board :drops-ally))
	(drops-enemy (getf full-board :drops-enemy))
	(board (getf full-board :board)))
    (cond ((or (= n 0) (checkmate board drops-ally drops-enemy)) ;Ally final turn end of recursion evaluation of the board
	   (let ((max-eval))
	     (setq max-eval (evaluation-enemy board drops-enemy drops-ally))
	     (return-from minimax (list :score max-eval :board board :drops-ally drops-ally :drops-enemy drops-enemy))))
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
	(board (getf full-board :board)))
    ;(print n)
    ;(print full-board)
    (cond ((or (= n 0) (checkmate board drops-ally drops-enemy)) ;Ally final turn or checkmate (terminal node)
	   (let ((max-eval))
	     (setf max-eval (evaluation-enemy board drops-enemy drops-ally))
	     (return-from alphabeta (list :score max-eval :board board :drops-ally drops-ally :drops-enemy drops-enemy))))
	  
	  ((oddp n)
	   (let ((all (get-all-ally board drops-ally))
		 (min-eval 200)
		 (variation))
	     
	       (dolist (piece-on (getf all :moves))
		 (dolist (movement (second piece-on))
		   (let ((variation-1)
			 (variation-2))
		     
					;Move on an occupied square  
		     (if (string-equal (subseq (aref board (first movement) (second movement)) 0 1) "-")
			 (let ((new-drops (copy-list drops-ally)))
			   (setf variation-1 (alphabeta
					      (- n 1)
					      (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))    
						    :drops-enemy drops-enemy
						    :drops-ally (push (subseq (aref board (first movement) (second movement)) 1) new-drops))
					      alpha
					      beta))			  
			   (if (> min-eval (getf variation-1 :score))
			       (progn
				 (setf min-eval (getf variation-1 :score))
				 (setf variation variation-1))))
		     
					;Move on an empty square
			 (progn
			   (setf variation-2 (alphabeta
					    (- n 1)
					    (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))    
						  :drops-enemy drops-enemy
						  :drops-ally drops-ally)
					    alpha
					    beta))
			   (if (> min-eval (getf variation-2 :score))
			       (progn
				 (setf min-eval (getf variation-2 :score))
				 (setf variation variation-2)))))

					;Beta pruning  
		     (setf beta (min beta min-eval))
		     (if (<= beta alpha)
			 (return-from alphabeta variation)))))
	     
	       (dolist (piece-off (getf all :drops))
		 (dolist (drop (second piece-off))
		   (let ((variation-3))

					;Drop
		     (setf variation-3 (alphabeta
					(- n 1)
					(list :board (drop-piece board (first piece-off) drop)
					      :drops-enemy drops-enemy
					      :drops-ally (remove (first piece-off) (copy-list drops-ally) :test #'equal))
					alpha
					beta))
		     (if (> min-eval (getf variation-3 :score))
			 (progn
			   (setf variation variation-3)			 
			   (setf min-eval (getf variation :score))))
		     
					;Beta pruning
		     (setf beta (min beta min-eval))
		     (if (<= beta alpha)
			 (return-from alphabeta variation)))))
	     (return-from alphabeta variation)))
	  
	  ((evenp n)
	   (let ((all (get-all-enemy board drops-enemy))
		 (max-eval -200)
		 (variation))
	       (dolist (piece-on (getf all :moves))
		 (dolist (movement (second piece-on))
		   (let ((variation-1)
			 (variation-2))

					;Move on an occupied square
		     (if (string-not-equal (aref board (first movement) (second movement)) "_")
			 (let ((new-drops (copy-list drops-enemy)))
			   (setf variation-1 (alphabeta
					      (- n 1)
					      (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
						    :drops-enemy (push (concatenate 'string "-" (subseq (aref board (first movement) (second movement)) 1)) new-drops)
						    :drops-ally drops-ally)
					      alpha
					      beta))
			   (if (< max-eval (getf variation-1 :score))
			       (progn
				 (setf variation variation-1)
				 (setf max-eval (getf variation-1 :score)))))
		     
					;Move on an empty square
			 (progn
			   (setf variation-2 (alphabeta
					    (- n 1)
					    (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
						  :drops-enemy drops-enemy
						  :drops-ally drops-ally)
					    alpha
					    beta))
			   (if (< max-eval (getf variation-2 :score))
			       (progn
				 (setf variation variation-2)
				 (setf max-eval (getf variation-2 :score))))))

					;Alpha pruning
		     (setf alpha (max alpha max-eval))
		     (if (>= alpha beta)
			 (return-from alphabeta variation)))))
	     
	       (dolist (piece-off (getf all :drops))
		 (dolist (drop (second piece-off))
		   (let ((variation-3))
		     
					;Drop
		     (setf variation-3 (alphabeta
					(- n 1)
					(list :board (drop-piece board (first piece-off) drop)
					      :drops-enemy (remove (first piece-off) (copy-list drops-enemy) :test #'equal)
					      :drops-ally drops-ally)
					alpha
					beta))
		     (if (< max-eval (getf variation-3 :score))
			 (progn
			   (setf variation variation-3)
			   (setf max-eval (getf variation-3 :score))))

					;Alpha pruning
		     (setf alpha (max alpha max-eval))
		     (if (>= alpha beta)
			 (return-from alphabeta variation)))))
	     (return-from alphabeta variation))))))

(defun newmain (n full-board)
  (let* ((result (alphabeta n full-board -200 200))
	 (board (list :board (getf result :board)
		      :drops-enemy (getf result :drops-enemy)
		      :drops-ally (getf result :drops-ally))))
    (print board)
    (format-game board)
    (when (> n 1)
      (newmain (- n 1) board))))
					

;;;----------------------- Alpha beta V2 ----------------------------------------------

(defun get-childs (full-board ally)
  (let ((drops-ally (getf full-board :drops-ally))
	(drops-enemy (getf full-board :drops-enemy))
	(board (getf full-board :board)))
    
    (if ally
	(let ((all (get-all-ally board drops-ally))
	      (result))
	  ;(print all)
	     
	  (dolist (piece-on (getf all :moves))
	    (dolist (movement (second piece-on))
	      ;(print movement)
	      ;(print (subseq (aref board (first movement) (second movement)) 0 1))
		     
					;Move on an occupied square  
	      (if (string-equal (subseq (aref board (first movement) (second movement)) 0 1) "-")
		  (let ((new-drops (copy-list drops-ally)))		    
		    (push
		     (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))    
			   :drops-enemy drops-enemy
			   :drops-ally (push (subseq (aref board (first movement) (second movement)) 1) new-drops))
		     result))
		  
					;Move on an empty square
		  (push 
		   (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))    
			 :drops-enemy drops-enemy
			 :drops-ally drops-ally)
		   result))))
	     
	  (dolist (piece-off (getf all :drops))
	    (dolist (drop (second piece-off))
	      

					;Drop
	      (push 
	       (list :board (drop-piece board (first piece-off) drop)
		     :drops-enemy drops-enemy
		     :drops-ally (remove (first piece-off) (copy-list drops-ally) :test #'equal))
	       result)))
	  (return-from get-childs result))

	(let ((all (get-all-enemy board drops-enemy))
	      (result))
	       
	       (dolist (piece-on (getf all :moves))
		 (dolist (movement (second piece-on))
		   (when (not (mate (move-piece board (first piece-on) movement (getf piece-on :initial)) drops-ally))

		     ;;Move on an occupied square
		     (if (string-not-equal (aref board (first movement) (second movement)) "_")
			 (let ((new-drops (copy-list drops-enemy)))
			   (push 
			    (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
				  :drops-enemy (push (concatenate 'string "-" (subseq (aref board (first movement) (second movement)) 1)) new-drops)
				  :drops-ally drops-ally)
			    result))
		     
			 ;;Move on an empty square
			 (push 
			  (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
				:drops-enemy drops-enemy
				:drops-ally drops-ally)
			  result)))))
	     
	       (dolist (piece-off (getf all :drops))
		 (dolist (drop (second piece-off))
		     
		   ;;Drop
		   (push  
		    (list :board (drop-piece board (first piece-off) drop)
			  :drops-enemy (remove (first piece-off) (copy-list drops-enemy) :test #'equal)
			  :drops-ally drops-ally)
		    result)))
	  (return-from get-childs result)))))

(defun alphabeta-V2 (full-board n alpha beta evaluation-function)
  (when (or (= n 0) (checkmate-full full-board))
    (return-from alphabeta-V2 (list :score (funcall evaluation-function full-board) :full-board full-board)))
  (if (oddp n)
      ;;Ally turn, minimizing player
      (let ((value 300)
	    (tmp-child))
	(dolist (child (get-childs full-board t))
	  (let ((child-value (getf (alphabeta-V2 child (- n 1) alpha beta evaluation-function) :score)))
	    ;(print child)
	    ;(print child-value)
	    (when (< child-value value)
	      (progn
		(setf value child-value)
		(setf tmp-child child)))
	    (setf beta (min beta value))
	    (when (< beta alpha)
	      ;; Beta cut
	      (return-from alphabeta-V2 (list :score value :full-board tmp-child)))))
	(return-from alphabeta-V2 (list :score value :full-board tmp-child)))
      
      ;;Enemy turn, maximizing player
      (let ((value -300)
	    (tmp-child))
	(dolist (child (get-childs full-board nil))
	  (let ((child-value (getf (alphabeta-V2 child (- n 1) alpha beta evaluation-function) :score)))
	    (when (> child-value value)
	      (progn
		(setf value child-value)
		(setf tmp-child child)))
	    (setf alpha (max alpha value))
	    (when (< beta alpha)
	      ;; Alpha cut
	      (return-from alphabeta-V2 (list :score value :full-board tmp-child))))
	(return-from alphabeta-V2 (list :score value :full-board tmp-child))))))

(defun start (full-board n evaluation-function)
  (let ((result (alphabeta-V2 full-board n -300 300 evaluation-function)))
    (format-game (getf result :full-board))
    (print (getf result :score))
    (when (> n 1)
      (start (getf result :full-board) (- n 1) evaluation-function))))

(defun minimax-V2 (full-board n)
  (when (or (= n 0) (checkmate-full full-board))
    (return-from minimax-V2 (list :score (evaluation-enemy-full full-board) :full-board full-board)))
  (if (oddp n)
      ;;Ally turn, minimizing player
      (let ((value 300)
	    (tmp-child))
	(dolist (child (get-childs full-board t))
	  (let ((child-value (getf (minimax-V2 child (- n 1)) :score)))
	    ;(print child)
	    ;(print child-value)
	    (when (< child-value value)
	      (progn
		(setf value child-value)
		(setf tmp-child child)))))
	(return-from minimax-V2 (list :score value :full-board tmp-child)))
      
      ;;Enemy turn, maximizing player
      (let ((value -300)
	    (tmp-child))
	(dolist (child (get-childs full-board nil))
	  (let ((child-value (getf (minimax-V2 child (- n 1)) :score)))
	    (when (> child-value value)
	      (progn
		(setf value child-value)
		(setf tmp-child child)))))
	(return-from minimax-V2 (list :score value :full-board tmp-child)))))

(defun start-2 (full-board n)
  (let ((result (minimax-V2 full-board n)))
    (format-game (getf result :full-board))
    (print (getf result :score))
    (when (> n 1)
      (start-2 (getf result :full-board) (- n 1)))))



	
		
