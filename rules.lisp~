;;;; File containing all the functions for implementing the rules of shogi

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
  "Create the initial board of the tsume to solve indexed by letter rows and columns. Ennemy pieces are prefixed with -"
  (list :board
    (list ;Tsume 68, soluce (S 6 7) -> (+S 5 8)
     '("" "" "" "" "" "" "" "" "")
     '("" "" "" "" "" "" "" "" "")
     '("" "" "" "" "" "" "L" "" "R")
     '("" "" "" "" "" "" "S" "J" "S")
     '("" "" "" "" "" "" "" "" "")
     '("" "" "" "" "" "" "" "" "")
     '("" "" "" "" "" "" "" "" "")
     '("" "" "" "" "" "" "" "" "")
     '("" "" "" "" "" "" "" "" ""))
    :drops '()))


(defun select-square (board row column)
  "Return the piece on the square"
  (getf (getf board row) column))

(defun get-all (board)
  "Return every possible moves + drops with the notation ('Piece' row column)"
  (let* ((result))
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (list (nth column (nth row (getf board :board))))))
	  (typecase (car square)
	    (character
	     (when (char-not-equal (car square) #\Null)
	       (push (list square (get-available-move square row column)) result))))
	  (print result))))))

(defun get-test (board)
  "Return every possible moves + drops with the notation ('Piece' row column)"
  (let* ((result))
    (dotimes (row 9 result)
      (dotimes (column 9 result)
	(let ((square (list (nth column (nth row (getf board :board))))))
	  (print square))))))
    


(defun get-available-move (piece row column)
  (cond ((equal piece "P") (get-move-pawn row column))
	((equal piece "L") (get-move-lance row column))
	((equal piece "N") (get-move-knight row column))
	((equal piece "S") (get-move-silver row column))
	((equal piece "G") (get-move-gold row column))
	((equal piece "B") (get-move-bishop row column))
	((equal piece "R") (get-move-rook row column))
	((equal piece "K") (get-move-king row column))
	((equal piece "J") (get-move-king row column)) ;roi adverse a voir si utile
	((equal piece "+R") (get-move-dragon row column))
	((equal piece "+B") (get-move-horse row column))
	((or (equal piece "+S") (equal piece "+N") (equal piece "+L") (equal piece "+P")) (get-move-gold row column))))

;;; Utility functions
(defun tuple-coord (rows columns)
  "Transform a list of rows and a list of columns in a list of tuple (row column)"
  (if (= (length rows) (length columns)) 
  (let* ((result))
    (dotimes (i (length rows) result)
      (cond ((and (<= (nth i rows) 9) (>= (nth i rows) 1) (<= (nth i columns) 9) (>= (nth i columns) 1))
	    (push (list (nth i rows) (nth i columns)) result)))))))

;;; All get-move-... functions that return the available moves
(defun get-move-pawn (row column)
  (cond ((> row 1)
	 (list (+ row 1 column)))
	(promote "P")))

(defun get-move-lance (row column)
  (cond ((> row 1)
	 (let* ((avail-row (remove (+ row 1) '(1 2 3 4 5 6 7 8 9) :test #'>))
		(avail-column (mapcar (lambda (x) (setq x column)) avail-row)))
	   (tuple-coord avail-row avail-column)))
	(promote "L")))

(defun get-move-knight (row column)
  (cond ((and (> row 2) (< column 9) (> column 1))
	 (list (list (+ row 2) (+ column 1)) (list (+ row 2) (- column 1))))
	((and (> row 2) (= column 9))
	 (list (list (+ row 2) (- column 1))))
	((and (> row 2) (= column 1))
	 (list (list (+ row 2) (+ column 1))))
	(promote "N")))

(defun get-move-silver (row column)
  (let* ((all-row (list (+ row 1) (+ row 1) (+ row 1) (- row 1) (- row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1))))
    (tuple-coord all-row all-column)))

(defun get-move-gold (row column)
  (let* ((all-row (list (+ row 1) (+ row 1) (+ row 1) row row (- row 1)))
	 (all-column (list (+ column 1) column (- column 1) (+ column 1) (- column 1) column)))
    (tuple-coord all-row all-column)))

(defun get-move-bishop (row column)
  (let* ((result))
    (dotimes (i 9 result)
      (when (and (/= i 0) (<= (+ row i) 9) (<= (+ column i) 9))
	(push (list (+ row i) (+ column i)) result))
      (when (and (/= i 0) (<= (+ row i) 9) (>= (- column i) 1))
	(push (list (+ row i) (- column i)) result))
      (when (and (/= i 0) (>= (- row i) 1) (<= (+ column i) 9))
	(push (list (- row i) (+ column i)) result))
      (when (and (/= i 0) (>= (- row i) 1) (>= (- column i) 1))
	(push (list (- row i) (- column i)) result)))))

(defun get-move-rook (row column)
  (let* ((result))
    (dotimes (i 9 result)
      (when (and (/= i 0) (<= (+ row i) 9))
	(push (list (+ row i) column) result))
      (when (and (/= i 0) (>= (- row i) 1))
	(push (list (- row i) column) result))
      (when (and (/= i 0) (<= (+ column i) 9))
	(push (list row (+ column i)) result))
      (when (and (/= i 0) (>= (- column i) 1))
	(push (list row (- column i)) result)))))

(defun get-move-king (row column)
  (let* ((result)) ;Moves from a gold general with 2 additionnal moves
    (when (and (>= (- row 1) 1) (>= (- column 1) 1))
      (push (list (- row 1) (- column 1)) result))      
    (when (and (>= (- row 1) 1) (<= (+ column 1) 9))
      (push (list (- row 1) (+ column 1)) result))
    (append result (get-move-gold row column))))

(defun get-move-dragon (row column)
  (let* ((result))
    (when (and (<= (+ row 1) 9) (<= (+ column 1) 9))
      (push (list (+ row 1) (+ column 1)) result))
    (when (and (<= (+ row 1) 9) (>= (- column 1) 1))
      (push (list (+ row 1) (- column 1)) result))
    (when (and (>= (- row 1) 1) (<= (+ column 1) 9))
      (push (list (- row 1) (+ column 1)) result))
    (when (and (>= (- row 1) 1) (>= (- column 1) 1))
      (push (list (- row 1) (- column 1)) result))
    (append result (get-move-rook row column))))

(defun get-move-horse (row column)
  (let* ((result))
    (when (<= (+ row 1) 9)
      (push (list (+ row 1) column) result))
    (when (>= (- row 1) 1)
      (push (list (- row 1) column) result))
    (when (<= (+ column 1) 9)
      (push (list row (+ column 1)) result))
    (when (>= (- column 1) 1)
      (push (list row (- column 1)) result))
    (append result (get-move-bishop row column))))  

    
      
   
 

  
	
	
	
  
   
