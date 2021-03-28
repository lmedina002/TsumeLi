;;;; File containing all the functions for implementing the rules of shogi

;;;----------------------------------------------------------------------------------------------
;;; Board related functions


(defun init-empty-board ()
  "Create an empty board indexed by letter rows and columns"
  (list
   :9 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :8 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :7 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :6 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :5 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :4 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :3 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :2 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :1 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")))

(defun init-board ()
  "Create the initial board of the tsume to solve indexed from 0 to 8 by rows then columns. Ennemy pieces are prefixed with -"
  (list :board
	(make-array '(9 9)
		    :initial-contents 
		      ; 0   1   2   3   4   5   6   7   8
		    '(("_" "_" "_" "_" "_" "_" "_" "_" "_") ;0
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;1
		      ("_" "_" "_" "_" "_" "_" "L" "_" "R") ;2
		      ("_" "_" "_" "_" "_" "_" "_" "-J" "S") ;3
		      ("_" "_" "_" "_" "_" "_" "P" "G" "_") ;4
		      ("_" "_" "_" "-R" "_" "_" "_" "_" "_") ;5
		      ("_" "_" "_" "_" "_" "_" "K" "_" "_") ;6
		      ("_" "_" "_" "_" "_" "_" "P" "_" "_") ;7
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_")));8
	:drops-ally
	(list "P" "R")
	:drops-enemy
	(list "-P" "-G")))


;;;----------------------------------------------------------------------------------------------
;;; Win functions

(defun mate (board drops-ally)
  (let ((all (get-all-ally board drops-ally))
	(jewel (find-piece "-J" board)))
    (dolist (pieces (getf all :moves))
      (dolist (location (second pieces))
	(if (equal location jewel) (return-from mate t))))    
    (return-from mate nil)))

(defun checkmate (board drops-ally drops-enemy)
  "Return t if the jewel on the board is checkmate"
  ;; trouver jewel verif s'il est mat verif pour chaque move piece adverse qu'il est tjrs mat
  (when (mate board drops-ally)
    (let* ((all (get-all-enemy board drops-enemy))
	   (moves (getf all :moves))
	   (drops (getf all :drops)))
      (dolist (pieces moves)	
	(dolist (location (second pieces))
	  ;(print pieces)
	  ;(print location)
	  (when (not (mate (move-piece board (first pieces) location (getf pieces :initial)) drops-ally))
	    (return-from checkmate nil))))
      (dolist (item drops)	
	(dolist (place (second item))
	  ;(print pieces)
	  ;(print location)
	  (when (not (mate (drop-piece board (first item) place) (remove (first item) drops-enemy :test #'equal)))
	    (return-from checkmate nil))))
      (return-from checkmate t))))

(defun checkmate-full (full-board)
  (let ((drops-ally (getf full-board :drops-ally))
	(drops-enemy (getf full-board :drops-enemy))
	(board (getf full-board :board)))
    (checkmate board drops-ally drops-enemy)))

(defun move-piece (board piece location initial)
  "Return a copy from the board with the piece moved to the location"
  (let ((result (copy-array board)))
    (setf (aref result (first location) (second location)) piece)
    (setf (aref result (first initial) (second initial)) "_")
    (return-from move-piece result)))

(defun move-promote-piece (board piece location initial)
  "Return a copy from the board with the piece moved to the location and promoted"
  (let ((result (copy-array board)))
    (setf (aref result (first location) (second location)) (promote piece))
    (setf (aref result (first initial) (second initial)) "_")
    (return-from move-promote-piece result)))

(defun drop-piece (board piece location)
  "Return a copy from the board with the piece dropped to the location"
  (let ((result (copy-array board)))
    (setf (aref result (first location) (second location)) piece)
    (return-from drop-piece result)))
    
	
    
(defun select-square (board row column)
  "Return the piece on the square"
  (getf (getf board row) column))

(defun get-all-ally (board drops-ally)
  "Return every possible moves + drops for ally pieces with the notation (:moves (('Piece' '((row column) ...) :initial (row colummn)) :drops ('Piece' '((row column) ...))"
  (let* ((result (list :moves '() :drops '()))
	 (empty-pl)
	 (empty-n)
	 (empty))
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (aref board row column)))
	  (when (and (string-not-equal square  "_")
		     (string-not-equal (subseq square 0 1) "-")
		     (not (equal (get-available-move-ally square row column board) nil)))
	    (push (list square (get-available-move-ally square row column board) :initial (list row column)) (getf result :moves)))
	  (when (string-equal square "_")
	    (when (> row 1)
	      (progn
		(push (list row column) empty-pl)
		(push (list row column) empty-n)
		(push (list row column) empty)))
	    (when (= row 1)
	      (progn
		(push (list row column) empty-pl)
		(push (list row column) empty)))
	    (when (= row 0)
	      (push (list row column) empty))))))
    (dolist (piece drops-ally)
      (cond ((or (string-equal piece "L") (string-equal piece "P"))
	     (push (list piece empty-pl) (getf result :drops)))
	     ((string-equal piece "N")
	     (push (list piece empty-n) (getf result :drops)))
	     (t (push (list piece empty) (getf result :drops)))))
    (return-from get-all-ally result)))

(defun get-available-move-ally (piece row column board)
  (cond ((equal piece "P") (get-move-pawn-ally row column board))
	((equal piece "L") (get-move-lance-ally row column board))
	((equal piece "N") (get-move-knight-ally row column board))
	((equal piece "S") (get-move-silver-ally row column board))
	((equal piece "G") (get-move-gold-ally row column board))
	((equal piece "B") (get-move-bishop-ally row column board))
	((equal piece "R") (get-move-rook-ally row column board))
	((equal piece "K") (get-move-king-ally row column board))
	((equal piece "+R") (get-move-dragon-ally row column board))
	((equal piece "+B") (get-move-horse-ally row column board))
	((or (equal piece "+S") (equal piece "+N") (equal piece "+L") (equal piece "+P")) (get-move-gold-ally row column board))))

(defun get-all-enemy (board drops-enemy)
  "Return every possible moves + drops for ally pieces with the notation ('Piece' row column)"
  (let* ((result (list :moves '() :drops '()))
	 (empty-pl)
	 (empty-n)
	 (empty))
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (aref board row column)))
	  (when (and (string-not-equal square  "_")
		     (equal (subseq square 0 1) "-")
		     (not (equal (get-available-move-enemy square row column board) nil)))
	    (push (list square (get-available-move-enemy square row column board) :initial (list row column)) (getf result :moves)))
	  (when (string-equal square "_")
	    (when (< row 7)
	      (progn
		(push (list row column) empty-pl)
		(push (list row column) empty-n)
		(push (list row column) empty)))
	    (when (= row 7)
	      (progn
		(push (list row column) empty-pl)
		(push (list row column) empty)))
	    (when (= row 8)
	      (push (list row column) empty))))))
    (dolist (piece drops-enemy)
      (cond ((or (string-equal piece "-L") (string-equal piece "-P"))
	     (push (list piece empty-pl) (getf result :drops)))
	     ((string-equal piece "-N")
	     (push (list piece empty-n) (getf result :drops)))
	     (t (push (list piece empty) (getf result :drops)))))
    (return-from get-all-enemy result)))

(defun get-available-move-enemy (piece row column board)
  (cond ((equal piece "-P") (get-move-pawn-enemy row column board))
	((equal piece "-L") (get-move-lance-enemy row column board))
	((equal piece "-N") (get-move-knight-enemy row column board))
	((equal piece "-S") (get-move-silver-enemy row column board))
	((equal piece "-G") (get-move-gold-enemy row column board))
	((equal piece "-B") (get-move-bishop-enemy row column board))
	((equal piece "-R") (get-move-rook-enemy row column board))
	((equal piece "-J") (get-move-king-enemy row column board))
	((equal piece "-+R") (get-move-dragon-enemy row column board))
	((equal piece "-+B") (get-move-horse-enemy row column board))
	((or (equal piece "-+S") (equal piece "-+N") (equal piece "-+L") (equal piece "-+P")) (get-move-gold-enemy row column board))))

(defun promote (piece)
  "Return the string corresponding to the promotion of the given piece"
  (cond ((equal (subseq piece 0 1) "-")
	 (concatenate 'string "-+" (subseq piece 1)))
	(t (concatenate 'string "+" (subseq piece 0)))))

;;;----------------------------------------------------------------------------------------------
;;; Utility functions

(defun is-piece (row column board)
  (string-not-equal (aref board row column) "_"))

(defun is-ally (row column board)
  (and (char-not-equal (char (aref board row column) 0) #\_) (char-not-equal (char (aref board row column) 0) #\-)))

(defun is-enemy (row column board)
  (char-equal (char (aref board row column) 0) #\-))
  

(defun find-piece (piece board)
    (dotimes (row 9)
      (dotimes (column 9)
	(let ((square (aref board row column)))
	     (when (string-equal square piece)
	       (return-from find-piece (list row column)))))))

(defun tuple-coord (rows columns)
  "Transform a list of rows and a list of columns in a list of tuple (row column)"
  (if (= (length rows) (length columns)) 
  (let* ((result))
    (dotimes (i (length rows) result)
      (cond ((and (<= (nth i rows) 8) (>= (nth i rows) 0) (<= (nth i columns) 8) (>= (nth i columns) 0))
	     (push (list (nth i rows) (nth i columns)) result)))))))

(defun contains (item sequence)
  (if (member item sequence) T NIL))

(defun get-element (list predicate key)
              (when list
                (let* ((m0 (first list))
                       (m1 (getf m0 key)))
                  (mapc (lambda (e0 &aux (e1 (getf e0 key)))
                          (when (funcall predicate e1 m1)
                            (psetf m0 e0 m1 e1)))
                        list)
                  m0)))
	  

;Function taken from the library Alexandria to copy arrays
(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and adjustability (if any) as the original, unless overridden by the keyword
arguments. Performance depends on efficiency of general ADJUST-ARRAY in the host lisp -- for most cases a special purpose copying function is likely to
perform better."
  (let ((dims (array-dimensions array)))
    ;; Dictionary entry for ADJUST-ARRAY requires adjusting a
    ;; displaced array to a non-displaced one to make a copy.
    (adjust-array
     (make-array dims
                 :element-type element-type :fill-pointer fill-pointer
                 :adjustable adjustable :displaced-to array)
     dims)))




;;;----------------------------------------------------------------------------------------------
;;; All get-move-...-ally  functions that return the available moves for the "ally" pieces

(defun get-move-pawn-ally (row column board)
  (cond ((and (> row 0) (not (is-ally (- row 1) column board))) 
	 (list (list (- row 1) column))) ;double list for coherence with others get-move
	(t nil)))

(defun get-move-lance-ally (row column board)
  (cond ((> row 0)
	 (let* ((avail-row (remove (- row 1) '(0 1 2 3 4 5 6 7 8) :test #'<)))
	   (dolist (item avail-row)    
	     (cond
	       ((and (is-piece item column board) (is-enemy item column board))
		(setq avail-row (remove item avail-row :test #'>)))
	       ((and (is-piece item column board) (is-ally item column board))
		(setq avail-row (remove (+ item 1) avail-row :test #'>)))))
	   (tuple-coord avail-row (mapcar (lambda (x) (setq x column)) avail-row))))
	(t nil)))

(defun get-move-knight-ally (row column board)
  (cond ((and (> row 1) (< column 8) (> column 0))
	 (cond ((and (not (is-ally (- row 2) (+ column 1) board)) (not (is-ally (- row 2) (- column 1) board)))
		(list (list (- row 2) (+ column 1)) (list (- row 2) (- column 1))))
	       ((is-ally (- row 2) (+ column 1) board)
		(list (list (- row 2) (- column 1))))
	       ((is-ally (- row 2) (- column 1) board)
		(list (list (- row 2) (+ column 1))))))
	((and (> row 1) (= column 8))
	 (cond ((is-ally (- row 2) (- column 1) board)
		())
	       (t (list (list (- row 2) (- column 1))))))
	((and (> row 1) (= column 0))
	 (cond ((is-ally (- row 2) (+ column 1) board)
		())
	       (t (list (list (- row 2) (+ column 1))))))
	(t (promote "N"))))

(defun get-move-silver-ally (row column board)
  (let* ((all-row (list (- row 1) (- row 1) (- row 1) (+ row 1) (+ row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1)))
	 (result))
    (dolist (item (tuple-coord all-row all-column) result)
      (when (not (is-ally (first item) (second item) board))
	(push item result)))))

(defun get-move-gold-ally (row column board)
  (let* ((all-row (list (- row 1) (- row 1) (- row 1) row row (+ row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1) column))
	 (result))
    (dolist (item (tuple-coord all-row all-column) result)
      (when (not (is-ally (first item) (second item) board))
	(push item result)))))
    

(defun get-move-bishop-ally (row column board)
  (let* ((result)
	 (stop1)
	 (stop2)
	 (stop3)
	 (stop4))
    (dotimes (i 9 result)
      (when (and (/= i 0) (<= (+ row i) 8) (<= (+ column i) 8) (not stop1))
	(cond ((is-ally (+ row i) (+ column i) board)
		(setf stop1 t))
	       ((is-enemy (+ row i) (+ column i) board)
		(progn
		  (push (list (+ row i) (+ column i)) result)
		  (setf stop1 t)))
	       (t (push (list (+ row i) (+ column i)) result))))
      (when (and (/= i 0) (<= (+ row i) 8) (>= (- column i) 0) (not stop2))
	(cond ((is-ally (+ row i) (- column i) board)
		(setf stop2 t))
	       ((is-enemy (+ row i) (- column i) board)
		(progn
		  (push (list (+ row i) (- column i)) result)
		  (setf stop2 t)))
	       (t (push (list (+ row i) (- column i)) result))))
      (when (and (/= i 0) (>= (- row i) 0) (<= (+ column i) 8) (not stop3))
	(cond ((is-ally (- row i) (+ column i) board)
		(setf stop3 t))
	       ((is-enemy (- row i) (+ column i) board)
		(progn
		  (push (list (- row i) (+ column i)) result)
		  (setf stop3 t)))
	       (t (push (list (- row i) (+ column i)) result))))
      (when (and (/= i 0) (>= (- row i) 0) (>= (- column i) 0) (not stop4))
	(cond ((is-ally (- row i) (- column i) board)
		(setf stop4 t))
	       ((is-enemy (- row i) (- column i) board)
		(progn
		 (push (list (- row i) (- column i)) result)
		 (setf stop4 t)))
	       (t (push (list (- row i) (- column i)) result)))))))

(defun get-move-rook-ally (row column board)
  (let* ((result)
	 (stop1)
	 (stop2)
	 (stop3)
	 (stop4))
    (dotimes (i 9 result)
      (when (and (/= i 0) (<= (+ row i) 8) (not stop1))
	(cond ((is-ally (+ row i) column board)
		(setq stop1 t))
	       ((is-enemy (+ row i) column board)
		(progn
		  (push (list (+ row i) column) result)
		  (setq stop1 t)))
	       (t (push (list (+ row i) column) result))))
      (when (and (/= i 0) (>= (- row i) 0) (not stop2))
	(cond ((is-ally (- row i) column board)
		(setq stop2 t))
	       ((is-enemy (- row i) column board)
		(progn
		  (push (list (- row i) column) result)
		  (setq stop2 t)))
	       (t (push (list (- row i) column) result))))
      (when (and (/= i 0) (<= (+ column i) 8) (not stop3))
	(cond ((is-ally row (+ column i) board)
		(setq stop3 t))
	       ((is-enemy row (+ column i) board)
		(progn
		  (push (list row (+ column i)) result)
		  (setq stop3 t)))
	       (t (push (list row (+ column i)) result))))
      (when (and (/= i 0) (>= (- column i) 0) (not stop4))
	(cond ((is-ally row (- column i) board)
		(setq stop4 t))
	       ((is-enemy row (- column i) board)
		(progn
		  (push (list row (- column i)) result)
		  (setq stop4 t)))
	       (t (push (list row (- column i)) result)))))))

(defun get-move-king-ally (row column board)
  (let* ((result)) ;Moves from a gold general with 2 additionnal moves
    (when (and (<= (+ row 1) 8) (>= (- column 1) 0) (not (is-ally (+ row 1) (- column 1) board)))
      (push (list (+ row 1) (- column 1)) result))      
    (when (and (<= (+ row 1) 8) (<= (+ column 1) 8) (not (is-ally (+ row 1) (+ column 1) board)))
      (push (list (+ row 1) (+ column 1)) result))
    (append result (get-move-gold-ally row column board))))

(defun get-move-dragon-ally (row column board)
  (let* ((result))
    (when (and (<= (+ row 1) 8) (<= (+ column 1) 8) (not (is-ally (+ row 1) (+ column 1) board)))
      (push (list (+ row 1) (+ column 1)) result))
    (when (and (<= (+ row 1) 8) (>= (- column 1) 0) (not (is-ally (+ row 1) (- column 1) board)))
      (push (list (+ row 1) (- column 1)) result))
    (when (and (>= (- row 1) 0) (<= (+ column 1) 8) (not (is-ally (- row 1) (+ column 1) board)))
      (push (list (- row 1) (+ column 1)) result))
    (when (and (>= (- row 1) 0) (>= (- column 1) 0) (not (is-ally (- row 1) (- column 1) board)))
      (push (list (- row 1) (- column 1)) result))
    (append result (get-move-rook-ally row column board))))

(defun get-move-horse-ally (row column board)
  (let* ((result))
    (when (and (<= (+ row 1) 8) (not (is-ally (+ row 1) column board)))
      (push (list (+ row 1) column) result))
    (when (and (>= (- row 1) 0) (not (is-ally (- row 1) column board)))
      (push (list (- row 1) column) result))
    (when (and (<= (+ column 1) 8) (not (is-ally row (+ column 1) board)))
      (push (list row (+ column 1)) result))
    (when (and (>= (- column 1) 0) (not (is-ally row (- column 1) board)))
      (push (list row (- column 1)) result))
    (append result (get-move-bishop-ally row column board))))


;;;----------------------------------------------------------------------------------------------
;;; All get-move-...-enemy  functions that return the available moves for the "enemy" pieces

(defun get-move-pawn-enemy (row column board)
  (cond ((and (< row 8) (not (is-enemy (+ row 1) column board)))
	 (list (list (+ row 1) column))) ;double list for coherence with others get-move
	(t nil)))

(defun get-move-lance-enemy (row column board)
  (cond ((< row 8)
	 (let* ((avail-row (remove (+ row 1) '(0 1 2 3 4 5 6 7 8) :test #'>)))
	   (dolist (item avail-row)	     
	     (cond
	       ((and (is-piece item column board) (is-ally item column board))
		(setq avail-row (remove item avail-row :test #'<)))
	       ((and (is-piece item column board) (is-enemy item column board))
		(setq avail-row (remove (- item 1) avail-row :test #'<)))))		
	   (tuple-coord avail-row (mapcar (lambda (x) (setq x column)) avail-row))))
	(t nil)))

(defun get-move-knight-enemy (row column board)
  (cond ((and (< row 7) (< column 8) (> column 0))
	 (cond ((and (not (is-enemy (+ row 2) (+ column 1) board)) (not (is-enemy (+ row 2) (- column 1) board)))
		(list (list (+ row 2) (+ column 1)) (list (+ row 2) (- column 1))))
	       ((is-enemy (+ row 2) (+ column 1) board)
		(list (list (+ row 2) (- column 1))))
	       ((is-enemy (+ row 2) (- column 1) board)
		(list (list (+ row 2) (+ column 1))))))
	((and (< row 7) (= column 8))
	 (cond ((is-enemy (+ row 2) (- column 1) board)
		())
	       (t (list (list (+ row 2) (- column 1))))))
	((and (< row 7) (= column 0))
	 (cond ((is-ally (+ row 2) (+ column 1) board)
		())
	       (t (list (list (+ row 2) (+ column 1))))))
	(t (promote "-N"))))

(defun get-move-silver-enemy (row column board)
  (let* ((all-row (list (+ row 1) (+ row 1) (+ row 1) (- row 1) (- row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1)))
	 (result))
    (dolist (item (tuple-coord all-row all-column) result)
      (when (not (is-enemy (first item) (second item) board))
	(push item result)))))

(defun get-move-gold-enemy (row column board)
  (let* ((all-row (list (+ row 1) (+ row 1) (+ row 1) row row (- row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1) column))
	 (result))
    (dolist (item (tuple-coord all-row all-column) result)
      (when (not (is-enemy (first item) (second item) board))
	(push item result)))))

(defun get-move-bishop-enemy (row column board)
  (let* ((result)
	 (stop1)
	 (stop2)
	 (stop3)
	 (stop4))
    (dotimes (i 9 result)
      (when (and (/= i 0) (<= (+ row i) 8) (<= (+ column i) 8) (not stop1))
	(cond ((is-enemy (+ row i) (+ column i) board)
		(setq stop1 t))
	       ((is-ally (+ row i) (+ column i) board)
		(progn
		  (push (list (+ row i) (+ column i)) result)
		  (setq stop1 t)))
	       (t (push (list (+ row i) (+ column i)) result))))
      (when (and (/= i 0) (<= (+ row i) 8) (>= (- column i) 0) (not stop2))
	(cond ((is-enemy (+ row i) (- column i) board)
		(setq stop2 t))
	       ((is-ally (+ row i) (- column i) board)
		(progn
		  (push (list (+ row i) (- column i)) result)
		  (setq stop2 t)))
	       (t (push (list (+ row i) (- column i)) result))))
      (when (and (/= i 0) (>= (- row i) 0) (<= (+ column i) 8) (not stop3))
	(cond ((is-enemy (- row i) (+ column i) board)
		(setq stop3 t))
	       ((is-ally (- row i) (+ column i) board)
		(progn
		  (push (list (- row i) (+ column i)) result)
		  (setq stop3 t)))
	       (t (push (list (- row i) (+ column i)) result))))
      (when (and (/= i 0) (>= (- row i) 0) (>= (- column i) 0) (not stop4))
	(cond ((is-enemy (- row i) (- column i) board)
		(setq stop4 t))
	       ((is-ally (- row i) (- column i) board)
		(progn
		 (push (list (- row i) (- column i)) result)
		 (setq stop4 t)))
	       (t (push (list (- row i) (- column i)) result)))))))

(defun get-move-rook-enemy (row column board)
  (let* ((result)
	 (stop1)
	 (stop2)
	 (stop3)
	 (stop4))
    (dotimes (i 9 result)
      (when (and (/= i 0) (<= (+ row i) 8) (not stop1))
	(cond ((is-enemy (+ row i) column board)
		(setq stop1 t))
	       ((is-ally (+ row i) column board)
		(progn
		  (push (list (+ row i) column) result)
		  (setq stop1 t)))
	       (t (push (list (+ row i) column) result))))
      (when (and (/= i 0) (>= (- row i) 0) (not stop2))
	(cond ((is-enemy (- row i) column board)
		(setq stop2 t))
	       ((is-ally (- row i) column board)
		(progn
		  (push (list (- row i) column) result)
		  (setq stop2 t)))
	       (t (push (list (- row i) column) result))))
      (when (and (/= i 0) (<= (+ column i) 8) (not stop3))
	(cond ((is-enemy row (+ column i) board)
		(setq stop3 t))
	       ((is-ally row (+ column i) board)
		(progn
		  (push (list row (+ column i)) result)
		  (setq stop3 t)))
	       (t (push (list row (+ column i)) result))))
      (when (and (/= i 0) (>= (- column i) 0) (not stop4))
	(cond ((is-enemy row (- column i) board)
		(setq stop4 t))
	       ((is-ally row (- column i) board)
		(progn
		  (push (list row (- column i)) result)
		  (setq stop4 t)))
	       (t (push (list row (- column i)) result)))))))

(defun get-move-king-enemy (row column board)
  (let* ((result)) ;Moves from a gold general with 2 additionnal moves
    (when (and (>= (- row 1) 0) (>= (- column 1) 0) (not (is-enemy (- row 1) (- column 1) board)))
      (push (list (- row 1) (- column 1)) result))      
    (when (and (>= (- row 1) 0) (<= (+ column 1) 8) (not (is-enemy (- row 1) (+ column 1) board)))
      (push (list (- row 1) (+ column 1)) result))
    (append result (get-move-gold-enemy row column board))))

(defun get-move-dragon-enemy (row column board)
  (let* ((result))
    (when (and (<= (+ row 1) 8) (<= (+ column 1) 8) (not (is-enemy (+ row 1) (+ column 1) board)))
      (push (list (+ row 1) (+ column 1)) result))
    (when (and (<= (+ row 1) 8) (>= (- column 1) 0) (not (is-enemy (+ row 1) (- column 1) board)))
      (push (list (+ row 1) (- column 1)) result))
    (when (and (>= (- row 1) 0) (<= (+ column 1) 8) (not (is-enemy (- row 1) (+ column 1) board)))
      (push (list (- row 1) (+ column 1)) result))
    (when (and (>= (- row 1) 0) (>= (- column 1) 0) (not (is-enemy (- row 1) (- column 1) board)))
      (push (list (- row 1) (- column 1)) result))
    (append result (get-move-rook-enemy row column board))))

(defun get-move-horse-enemy (row column board)
  (let* ((result))
    (when (and (<= (+ row 1) 8) (not (is-enemy (+ row 1) column board)))
      (push (list (+ row 1) column) result))
    (when (and (>= (- row 1) 0) (not (is-enemy (- row 1) column board)))
      (push (list (- row 1) column) result))
    (when (and (<= (+ column 1) 8) (not (is-enemy row (+ column 1) board)))
      (push (list row (+ column 1)) result))
    (when (and (>= (- column 1) 0) (not (is-enemy row (- column 1) board)))
      (push (list row (- column 1)) result))
    (append result (get-move-bishop-enemy row column board))))


    
      
   
 

  
	
	
	
  
   
