;;;; Program to solve tsume in n-turn

(in-package :tsumeli)
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
    
    

;;--------------------- Auxiliary functions ---------------------------------------------


(defun get-childs (full-board ally)
  "Take a board and t if it's ally turn to play, nil otherwise. Return all boards configurations possible"
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
		     (list
		      :full-board (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))    
					:drops-enemy drops-enemy
					:drops-ally (push (subseq (aref board (first movement) (second movement)) 1) new-drops))
		      :message (format nil "~%Piece ~A in ~A move to ~A~%" (first piece-on) (getf piece-on :initial) movement))
		     result)
		    (if (and (<= (first movement) 2) ;promotion available
			     (not (equal (subseq (first piece-on) 0 1) "+")))
			(push
			 (list
			  :full-board (list :board (move-piece board (promote (first piece-on)) movement (getf piece-on :initial))    
					    :drops-enemy drops-enemy
					    :drops-ally (push (subseq (aref board (first movement) (second movement)) 1) new-drops))
			  :message (format nil "~%Piece ~A in ~A move to ~A and promote~%" (first piece-on) (getf piece-on :initial) movement))
			 result)))
	  
		  
					;Move on an empty square
		  (progn
		    (push 
		     (list
		      :full-board (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))    
					:drops-enemy drops-enemy
					:drops-ally drops-ally)
		      :message (format nil "~%Piece ~A in ~A move to ~A~%" (first piece-on) (getf piece-on :initial) movement))
		     result)
		    (if (and (<= (first movement) 2) ;promotion available
			     (not (equal (subseq (first piece-on) 0 1) "+")))
			(push
			 (list
			  :full-board (list :board (move-piece board (promote (first piece-on)) movement (getf piece-on :initial))    
					    :drops-enemy drops-enemy
					    :drops-ally drops-ally)
			  :message (format nil "~%Piece ~A in ~A move to ~A and promote~%" (first piece-on) (getf piece-on :initial) movement))
			 result))))))
	     
	  (dolist (piece-off (getf all :drops))
	    (dolist (drop (second piece-off))
	      

					;Drop
	      (push 
	       (list
		:full-board (list :board (drop-piece board (first piece-off) drop)
				  :drops-enemy drops-enemy
				  :drops-ally (remove (first piece-off) (copy-list drops-ally) :test #'equal))
		:message (format nil "~%Drop piece ~A to ~A~%" (first piece-off) drop))
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
			    (list
			     :full-board (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
					       :drops-enemy (push (concatenate 'string "-" (subseq (aref board (first movement) (second movement)) 1)) new-drops)
					       :drops-ally drops-ally)
			     :message (format nil "~%Piece ~A in ~A move to ~A~%" (first piece-on) (getf piece-on :initial) movement))
			    result)
			   (if (and (>= (first movement) 6) ;promotion available
				    (not (equal (subseq (first piece-on) 1 2) "+")))
			       (push
				(list
				 :full-board (list :board (move-piece board (promote (first piece-on)) movement (getf piece-on :initial))    
						   :drops-enemy (push (concatenate 'string "-" (subseq (aref board (first movement) (second movement)) 1)) new-drops)
						   :drops-ally drops-ally)
				 :message (format nil "~%Piece ~A in ~A move to ~A and promote~%" (first piece-on) (getf piece-on :initial) movement))
				result)))
		     
			 ;;Move on an empty square
			 (progn
			   (push 
			    (list
			     :full-board (list :board (move-piece board (first piece-on) movement (getf piece-on :initial))		    
					       :drops-enemy drops-enemy
					       :drops-ally drops-ally)
			     :message (format nil "~%Piece ~A in ~A move to ~A~%" (first piece-on) (getf piece-on :initial) movement))
			    result)
			   (if (and (>= (first movement) 6) ;promotion available
				    (not (equal (subseq (first piece-on) 1 2) "+")))
			       (push
				(list
				 :full-board (list :board (move-piece board (promote (first piece-on)) movement (getf piece-on :initial))    
						   :drops-enemy drops-enemy
						   :drops-ally drops-ally)
				 :message (format nil "~%Piece ~A in ~A move to ~A and promote~%" (first piece-on) (getf piece-on :initial) movement))
				result)))))))
	     
	       (dolist (piece-off (getf all :drops))
		 (dolist (drop (second piece-off))
		     
		   ;;Drop
		   (when (not (mate (drop-piece board (first piece-off) drop) drops-ally))
		     (push  
		      (list
		       :full-board (list :board (drop-piece board (first piece-off) drop)
					 :drops-enemy (remove (first piece-off) (copy-list drops-enemy) :test #'equal)
					 :drops-ally drops-ally)
		       :message (format nil "~%Drop piece ~A to ~A~%" (first piece-off) drop))
		      result))))
	  (return-from get-childs result)))))


;;;----------------------- Alpha beta pruning ----------------------------------------------

(defun alphabeta (full-board n evaluation-function &optional (alpha -300) (beta 300) message)
  (when (or (= n 0) (checkmate-full full-board))
    (return-from alphabeta (list :score (funcall evaluation-function full-board) :full-board full-board :message message)))
  (if (oddp n)
      ;;Ally turn, minimizing player
      (let ((value 300)
	    (tmp-child))
	(dolist (child (get-childs full-board t))
	  (let ((child-value (getf (alphabeta (getf child :full-board) (- n 1) evaluation-function alpha beta (getf child :message)) :score)))
	    ;(print child)
	    ;(print child-value)
	    (when (< child-value value)
	      (progn
		(setf value child-value)
		(setf tmp-child child)))
	    (setf beta (min beta value))
	    (when (< beta alpha)
	      ;; Beta cut
	      (return-from alphabeta (list :score value :full-board (getf tmp-child :full-board) :message (getf tmp-child :message))))))
	(return-from alphabeta (list :score value :full-board (getf tmp-child :full-board) :message (getf tmp-child :message))))
      
      ;;Enemy turn, maximizing player
      (let ((value -300)
	    (tmp-child))
	(dolist (child (get-childs full-board nil))
	  (let ((child-value (getf (alphabeta (getf child :full-board) (- n 1) evaluation-function alpha beta (getf child :message)) :score)))
	    (when (> child-value value)
	      (progn
		(setf value child-value)
		(setf tmp-child child)))
	    (setf alpha (max alpha value))
	    (when (< beta alpha)
	      ;; Alpha cut
	      (return-from alphabeta (list :score value :full-board (getf tmp-child :full-board) :message (getf tmp-child :message))))))
	(return-from alphabeta (list :score value :full-board (getf tmp-child :full-board) :message (getf tmp-child :message))))))



;;;--------------------- Minimax -----------------------------------

(defun minimax (full-board n evaluation-function &optional message)
  (when (or (= n 0) (checkmate-full full-board))
    (return-from minimax (list :score (funcall evaluation-function full-board) :full-board full-board :message message)))
  (if (oddp n)
      ;;Ally turn, minimizing player
      (let ((value 300)
	    (tmp-child))
	(dolist (child (get-childs full-board t))
	  (let ((child-value (getf (minimax (getf child :full-board) (- n 1) evaluation-function (getf child :message)) :score)))
	    ;(print child)
	    ;(print child-value)
	    (when (< child-value value)
	      (progn
		(setf value child-value)
		(setf tmp-child child)))))
	(return-from minimax (list :score value :full-board (getf tmp-child :full-board) :message (getf tmp-child :message))))
      
      ;;Enemy turn, maximizing player
      (let ((value -300)
	    (tmp-child))
	(dolist (child (get-childs full-board nil))
	  (let ((child-value (getf (minimax (getf child :full-board) (- n 1) evaluation-function (getf child :message)) :score)))
	    (when (> child-value value)
	      (progn
		(setf value child-value)
		(setf tmp-child child)))))
	(return-from minimax (list :score value :full-board (getf tmp-child :full-board) :message (getf tmp-child :message))))))


;;----------------------------------------------------------------------------

(defun start (full-board n evaluation-function algorithm)
  "Start algorithm with evaluation function, the board and the depth given"
  (let ((result (funcall algorithm full-board n evaluation-function)))
    (format-game (getf result :full-board))
    (format t (getf result :message))
    (format t "Score of the board: ~A~%" (funcall evaluation-function (getf result :full-board)))
    (when (> n 1)
      (start (getf result :full-board) (- n 1) evaluation-function algorithm))))




	
		
