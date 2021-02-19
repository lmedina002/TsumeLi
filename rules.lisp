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
  (make-array '(9 9)
	:initial-contents ;Tsume 68, soluce (S 6 7) -> (+S 4 7)
	 ;  0   1   2   3   4   5   6   7   8
	 '(("_" "_" "_" "_" "_" "_" "_" "_" "_") ;0
	 ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;1
	 ("_" "_" "_" "_" "_" "_" "L" "_" "R") ;2
	 ("_" "_" "_" "_" "_" "_" "_" "-J" "S") ;3
	 ("_" "_" "_" "_" "_" "_" "P" "P" "_") ;4
	 ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;5
	 ("_" "_" "_" "_" "_" "_" "K" "_" "_") ;6
	 ("_" "_" "_" "_" "_" "_" "P" "_" "_") ;7
	 ("_" "_" "_" "_" "_" "_" "_" "_" "_"))));8


;;;----------------------------------------------------------------------------------------------
;;; Win functions

(defun mate (board)
  (print board)
  (let ((moves (get-all-ally board))
	(jewel (find-piece "-J" board)))
    (dolist (pieces moves)
      (dolist (location (second pieces))
	(if (equal location jewel) (return-from mate t))))))

(defun checkmate (board)
  "Return t if the jewel on the board is checkmate"
  ;; trouver jewel verif s'il est mat verif pour chaque move piece adverse qu'il est tjrs mat
  (when (mate board)
    (let ((moves (get-all-enemy board)))
      ;(print moves)
      (dolist (pieces moves)	
	(dolist (location (second pieces))
	  ;(print pieces)
	  ;(print location)
	  (when (not (mate (move-piece board (first pieces) location)))
	    (return-from checkmate nil))))
      (return-from checkmate t))))

(defun move-piece (board piece location)
  "Return a copy from the board with the piece moved to the location"
  (let ((result (copy-array board))
	(initial (find-piece piece board)))
    (setf (aref result (first location) (second location)) piece)
    (setf (aref result (first initial) (second initial)) "_")
    (return-from move-piece result)))
    
	
    
(defun select-square (board row column)
  "Return the piece on the square"
  (getf (getf board row) column))

(defun get-all-ally (board)
  "Return every possible moves + drops for ally pieces with the notation ('Piece' row column)"
  (let* ((result))
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (aref board row column)))
	     (when (and (string-not-equal square  "_") (string-not-equal (subseq square 0 1) "-"))
	       (push (list square (get-available-move-ally square row column board)) result)))))))

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

(defun get-all-enemy (board)
  "Return every possible moves + drops for enemy pieces with the notation ('Piece' row column)"
  (let* ((result))
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (aref board row column)))
	     (when (and (string-not-equal square  "_") (equal (subseq square 0 1) "-"))
	       (push (list square (get-available-move-enemy square row column board)) result)))))))

(defun get-available-move-enemy (piece row column)
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
	(t (promote "P"))))

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
	(t (promote "L"))))

(defun get-move-knight-ally (row column board)
  (cond ((and (> row 1) (< column 8) (> column 0))
	 (list (list (- row 2) (+ column 1)) (list (- row 2) (- column 1))))
	((and (> row 1) (= column 8))
	 (list (list (- row 2) (- column 1))))
	((and (> row 1) (= column 0))
	 (list (list (- row 2) (+ column 1))))
	(t (promote "N"))))

(defun get-move-silver-ally (row column board)
  (let* ((all-row (list (- row 1) (- row 1) (- row 1) (+ row 1) (+ row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1))))
    (tuple-coord all-row all-column)))

(defun get-move-gold-ally (row column board)
  (let* ((all-row (list (- row 1) (- row 1) (- row 1) row row (+ row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1) column)))
    (tuple-coord all-row all-column)))

(defun get-move-bishop-ally (row column board)
  (let* ((result))
    (dotimes (i 9 result)
      (when (and (/= i 0) (<= (+ row i) 8) (<= (+ column i) 8))
	(push (list (+ row i) (+ column i)) result))
      (when (and (/= i 0) (<= (+ row i) 8) (>= (- column i) 0))
	(push (list (+ row i) (- column i)) result))
      (when (and (/= i 0) (>= (- row i) 0) (<= (+ column i) 8))
	(push (list (- row i) (+ column i)) result))
      (when (and (/= i 0) (>= (- row i) 0) (>= (- column i) 0))
	(push (list (- row i) (- column i)) result)))))

(defun get-move-rook-ally (row column board)
  (let* ((result))
    (dotimes (i 9 result)
      (when (and (/= i 0) (<= (+ row i) 8))
	(push (list (+ row i) column) result))
      (when (and (/= i 0) (>= (- row i) 0))
	(push (list (- row i) column) result))
      (when (and (/= i 0) (<= (+ column i) 8))
	(push (list row (+ column i)) result))
      (when (and (/= i 0) (>= (- column i) 0))
	(push (list row (- column i)) result)))))

(defun get-move-king-ally (row column board)
  (let* ((result)) ;Moves from a gold general with 2 additionnal moves
    (when (and (<= (+ row 1) 8) (>= (- column 1) 0))
      (push (list (+ row 1) (- column 1)) result))      
    (when (and (<= (+ row 1) 8) (<= (+ column 1) 8))
      (push (list (+ row 1) (+ column 1)) result))
    (append result (get-move-gold-ally row column board))))

(defun get-move-dragon-ally (row column board)
  (let* ((result))
    (when (and (<= (+ row 1) 8) (<= (+ column 1) 8))
      (push (list (+ row 1) (+ column 1)) result))
    (when (and (<= (+ row 1) 8) (>= (- column 1) 0))
      (push (list (+ row 1) (- column 1)) result))
    (when (and (>= (- row 1) 0) (<= (+ column 1) 8))
      (push (list (- row 1) (+ column 1)) result))
    (when (and (>= (- row 1) 0) (>= (- column 1) 0))
      (push (list (- row 1) (- column 1)) result))
    (append result (get-move-rook-ally row column board))))

(defun get-move-horse-ally (row column board)
  (let* ((result))
    (when (<= (+ row 1) 8)
      (push (list (+ row 1) column) result))
    (when (>= (- row 1) 0)
      (push (list (- row 1) column) result))
    (when (<= (+ column 1) 8)
      (push (list row (+ column 1)) result))
    (when (>= (- column 1) 0)
      (push (list row (- column 1)) result))
    (append result (get-move-bishop-ally row column board))))


;;;----------------------------------------------------------------------------------------------
;;; All get-move-...-enemy  functions that return the available moves for the "enemy" pieces

(defun get-move-pawn-enemy (row column board)
  (cond ((< row 8)
	 (list (list (+ row 1) column))) ;double list for coherence with others get-move
	(t (promote "P"))))

(defun get-move-lance-enemy (row column board)
  (cond ((< row 8)
	 (let* ((avail-row (remove (+ row 1) '(0 1 2 3 4 5 6 7 8) :test #'>)))
	   (dolist (item avail-row)	     
	     (when (is-piece item column board)
	       (setq avail-row (remove item avail-row :test #'<))))		
	   (tuple-coord avail-row (mapcar (lambda (x) (setq x column)) avail-row))))
	(t (promote "L"))))

(defun get-move-knight-enemy (row column board)
  (cond ((and (< row 7) (< column 8) (> column 0))
	 (list (list (+ row 2) (+ column 1)) (list (+ row 2) (- column 1))))
	((and (< row 7) (= column 8))
	 (list (list (+ row 2) (- column 1))))
	((and (< row 7) (= column 0))
	 (list (list (+ row 2) (+ column 1))))
	(t (promote "N"))))

(defun get-move-silver-enemy (row column board)
  (let* ((all-row (list (+ row 1) (+ row 1) (+ row 1) (- row 1) (- row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1))))
    (tuple-coord all-row all-column)))

(defun get-move-gold-enemy (row column board)
  (let* ((all-row (list (+ row 1) (+ row 1) (+ row 1) row row (- row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1) column)))
    (tuple-coord all-row all-column)))

(defun get-move-bishop-enemy (row column board)
  (get-move-bishop-ally row column board))

(defun get-move-rook-enemy (row column board)
  (get-move-rook-ally row column board))

(defun get-move-king-enemy (row column board)
  (get-move-king-ally row column board))

(defun get-move-dragon-enemy (row column board)
  (get-move-dragon-ally row column board))

(defun get-move-horse-enemy (row column board)
  (get-move-horse-ally row column board)) 


    
      
   
 

  
	
	
	
  
   
