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
	(list ;Tsume 68, soluce (S 6 7) -> (+S 5 8)
	 ;  0   1   2   3   4   5   6   7   8
	 '("_" "_" "_" "_" "_" "_" "_" "_" "_") ;0
	 '("_" "_" "_" "_" "_" "_" "_" "_" "_") ;1
	 '("_" "_" "_" "_" "_" "_" "L" "_" "R") ;2
	 '("_" "_" "_" "_" "_" "_" "S" "J" "S") ;3
	 '("_" "_" "_" "_" "_" "_" "_" "_" "_") ;4
	 '("_" "_" "_" "_" "_" "_" "_" "_" "_") ;5
	 '("_" "_" "_" "_" "_" "_" "_" "_" "_") ;6
	 '("_" "_" "_" "_" "_" "_" "_" "_" "_") ;7
	 '("_" "_" "_" "_" "_" "_" "_" "_" "_"));8
	:drops-ally '()
	:drops-ennemy '()))

;;;----------------------------------------------------------------------------------------------
;;; Win functions

(defun mate (board)
  (let ((moves (get-all-ally board))
	(jewel (find-jewel board)))
    (dolist (pieces moves)
      (dolist (location (second pieces))
	(if (equal location jewel) (return-from mate t))))))

(defun checkmate (board)
  "Return t if the jewel on the board is checkmate"
  ;; trouver jewel verif s'il est mat verif pour chaque move piece adverse qu'il est tjrs mat
  )

(defun select-square (board row column)
  "Return the piece on the square"
  (getf (getf board row) column))

(defun get-all-ally (board)
  "Return every possible moves + drops for ally pieces with the notation ('Piece' row column)"
  (let* ((result))
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (list (nth column (nth row (getf board :board))))))
	     (when (string-not-equal (format nil (car square)) "_")
	       (push (list square (get-available-move-ally (format nil (car square)) row column)) result)))))))

(defun get-available-move-ally (piece row column)
  (cond ((equal piece "P") (get-move-pawn-ally row column))
	((equal piece "L") (get-move-lance-ally row column))
	((equal piece "N") (get-move-knight-ally row column))
	((equal piece "S") (get-move-silver-ally row column))
	((equal piece "G") (get-move-gold-ally row column))
	((equal piece "B") (get-move-bishop-ally row column))
	((equal piece "R") (get-move-rook-ally row column))
	((equal piece "K") (get-move-king-ally row column))
	((equal piece "+R") (get-move-dragon-ally row column))
	((equal piece "+B") (get-move-horse-ally row column))
	((or (equal piece "+S") (equal piece "+N") (equal piece "+L") (equal piece "+P")) (get-move-gold-ally row column))))

(defun get-all-enemy (board)
  "Return every possible moves + drops for enemy pieces with the notation ('Piece' row column)"
  (let* ((result))
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (list (nth column (nth row (getf board :board))))))
	     (when (string-not-equal (format nil (car square)) "_")
	       (push (list square (get-available-move-enemy (format nil (car square)) row column)) result)))))))

(defun get-available-move-enemy (piece row column)
  (cond ((equal piece "-P") (get-move-pawn-enemy row column))
	((equal piece "-L") (get-move-lance-enemy row column))
	((equal piece "-N") (get-move-knight-enemy row column))
	((equal piece "-S") (get-move-silver-enemy row column))
	((equal piece "-G") (get-move-gold-enemy row column))
	((equal piece "-B") (get-move-bishop-enemy row column))
	((equal piece "-R") (get-move-rook-enemy row column))
	((equal piece "-K") (get-move-king-enemy row column))
	((equal piece "-+R") (get-move-dragon-enemy row column))
	((equal piece "-+B") (get-move-horse-enemy row column))
	((or (equal piece "-+S") (equal piece "-+N") (equal piece "-+L") (equal piece "-+P")) (get-move-gold-enemy row column))))

(defun promote (piece)
  "Return the string corresponding to the promotion of the given piece"
  (cond ((equal (subseq piece 0 1) "-")
	 (concatenate 'string "-+" (subseq piece 1)))
	(t (concatenate 'string "+" (subseq piece 0)))))

;;;----------------------------------------------------------------------------------------------
;;; Utility functions

(defun find-jewel (board)
    (dotimes (row 9)
      (dotimes (column 9)
	(let ((square (list (nth column (nth row (getf board :board))))))
	     (when (string-equal (format nil (car square)) "J")
	       (return-from find-jewel (list row column)))))))

(defun tuple-coord (rows columns)
  "Transform a list of rows and a list of columns in a list of tuple (row column)"
  (if (= (length rows) (length columns)) 
  (let* ((result))
    (dotimes (i (length rows) result)
      (cond ((and (<= (nth i rows) 8) (>= (nth i rows) 0) (<= (nth i columns) 8) (>= (nth i columns) 0))
	     (push (list (nth i rows) (nth i columns)) result)))))))


;;;----------------------------------------------------------------------------------------------
;;; All get-move-...-ally  functions that return the available moves for the "ally" pieces

(defun get-move-pawn-ally (row column)
  (cond ((> row 0)
	 (list (list (- row 1) column))) ;double list for coherence with others get-move
	(t (promote "P"))))

(defun get-move-lance-ally (row column)
  (cond ((> row 0)
	 (let* ((avail-row (remove (- row 1) '(0 1 2 3 4 5 6 7 8) :test #'<))
		(avail-column (mapcar (lambda (x) (setq x column)) avail-row)))
	   (tuple-coord avail-row avail-column)))
	(t (promote "L"))))

(defun get-move-knight-ally (row column)
  (cond ((and (> row 1) (< column 8) (> column 0))
	 (list (list (- row 2) (+ column 1)) (list (- row 2) (- column 1))))
	((and (> row 1) (= column 8))
	 (list (list (- row 2) (- column 1))))
	((and (> row 1) (= column 0))
	 (list (list (- row 2) (+ column 1))))
	(t (promote "N"))))

(defun get-move-silver-ally (row column)
  (let* ((all-row (list (- row 1) (- row 1) (- row 1) (+ row 1) (+ row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1))))
    (tuple-coord all-row all-column)))

(defun get-move-gold-ally (row column)
  (let* ((all-row (list (- row 1) (- row 1) (- row 1) row row (+ row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1) column)))
    (tuple-coord all-row all-column)))

(defun get-move-bishop-ally (row column)
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

(defun get-move-rook-ally (row column)
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

(defun get-move-king-ally (row column)
  (let* ((result)) ;Moves from a gold general with 2 additionnal moves
    (when (and (<= (+ row 1) 8) (>= (- column 1) 0))
      (push (list (+ row 1) (- column 1)) result))      
    (when (and (<= (+ row 1) 8) (<= (+ column 1) 8))
      (push (list (+ row 1) (+ column 1)) result))
    (append result (get-move-gold-ally row column))))

(defun get-move-dragon-ally (row column)
  (let* ((result))
    (when (and (<= (+ row 1) 8) (<= (+ column 1) 8))
      (push (list (+ row 1) (+ column 1)) result))
    (when (and (<= (+ row 1) 8) (>= (- column 1) 0))
      (push (list (+ row 1) (- column 1)) result))
    (when (and (>= (- row 1) 0) (<= (+ column 1) 8))
      (push (list (- row 1) (+ column 1)) result))
    (when (and (>= (- row 1) 0) (>= (- column 1) 0))
      (push (list (- row 1) (- column 1)) result))
    (append result (get-move-rook-ally row column))))

(defun get-move-horse-ally (row column)
  (let* ((result))
    (when (<= (+ row 1) 8)
      (push (list (+ row 1) column) result))
    (when (>= (- row 1) 0)
      (push (list (- row 1) column) result))
    (when (<= (+ column 1) 8)
      (push (list row (+ column 1)) result))
    (when (>= (- column 1) 0)
      (push (list row (- column 1)) result))
    (append result (get-move-bishop-ally row column))))


;;;----------------------------------------------------------------------------------------------
;;; All get-move-...-enemy  functions that return the available moves for the "enemy" pieces

(defun get-move-pawn-enemy (row column)
  (cond ((< row 8)
	 (list (list (+ row 1) column))) ;double list for coherence with others get-move
	(t (promote "P"))))

(defun get-move-lance-enemy (row column)
  (cond ((< row 8)
	 (let* ((avail-row (remove (+ row 1) '(0 1 2 3 4 5 6 7 8) :test #'>))
		(avail-column (mapcar (lambda (x) (setq x column)) avail-row)))
	   (tuple-coord avail-row avail-column)))
	(t (promote "L"))))

(defun get-move-knight-enemy (row column)
  (cond ((and (< row 7) (< column 8) (> column 0))
	 (list (list (+ row 2) (+ column 1)) (list (+ row 2) (- column 1))))
	((and (< row 7) (= column 8))
	 (list (list (+ row 2) (- column 1))))
	((and (< row 7) (= column 0))
	 (list (list (+ row 2) (+ column 1))))
	(t (promote "N"))))

(defun get-move-silver-enemy (row column)
  (let* ((all-row (list (+ row 1) (+ row 1) (+ row 1) (- row 1) (- row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1))))
    (tuple-coord all-row all-column)))

(defun get-move-gold-enemy (row column)
  (let* ((all-row (list (+ row 1) (+ row 1) (+ row 1) row row (- row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1) column)))
    (tuple-coord all-row all-column)))

(defun get-move-bishop-enemy (row column)
  (get-move-bishop-ally row column))

(defun get-move-rook-ennemy (row column)
  (get-move-rook-ally row column))

(defun get-move-king-enemy (row column)
  (get-move-king-ally row column))

(defun get-move-dragon-enemy (row column)
  (get-move-dragon-ally row column))

(defun get-move-horse-enemy (row column)
  (get-move-horse-ally row column)) 


    
      
   
 

  
	
	
	
  
   
