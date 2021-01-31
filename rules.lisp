;;;; File containing all the functions for implementing the rules of shogi

(defun init-empty-board ()
  "Create an empty board indexed by letter rows and columns"
  (list
   :1 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :2 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :3 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :4 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :5 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :6 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :7 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :8 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :9 '(:1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")))

(defun select-square (board row column)
  "Return the piece on the square"
  (getf (getf board row) column))

(defun move (board row column)
  (move-piece (board (select-square board row column))))

;(defun move-piece (board piece direction)
;  (cond ((equal piece "P") (move-pawn))
	
(defun get-available-move (piece row column)
  (cond ((equal piece "P") (get-move-pawn row column))
	((equal piece "L") (get-move-lance row column))
	((equal piece "N") (get-move-knight row column))
	((equal piece "S") (get-move-silver row column))
	((equal piece "G") (get-move-gold row column))
	((equal piece "B") (get-move-bishop row column))
	((equal piece "R") (get-move-rook row column))
	((equal piece "K") (get-move-knight row column))
	((equal piece "J") (get-move-jewel row column)) ;roi adverse a voir si utile
	((equal piece "+R") (get-move-dragon row column))
	((equal piece "+B") (get-move-horse row column))
	((or (equal piece "+S") (equal piece "+N") (equal piece "+L") (equal piece "+P")) (get-move-gold row column))))

;;; All get-move-... function that return the available moves
(defun get-move-pawn (row column)
  (if (> row 1)
      (list (+ row 1) column) (promote "P")))

(defun get-move-lance (row column)
  (cond ((> row 1)
      (let* ((avail-row (remove (+ row 1) '(1 2 3 4 5 6 7 8 9) :test #'>))
	      (avail-column (mapcar (lambda (x) (setq x column)) avail-row)))
       (list avail-row avail-column)))
      (promote "L")))  
	
	
	
	
  
   
