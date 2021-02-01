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

;;; All get-move-... functions that return the available moves
(defun get-move-pawn (row column)
  (cond (> row 1)
      (list (+ row 1) column) (promote "P")))

(defun get-move-lance (row column)
  (cond ((> row 1)
      (let* ((avail-row (remove (+ row 1) '(1 2 3 4 5 6 7 8 9) :test #'>))
	      (avail-column (mapcar (lambda (x) (setq x column)) avail-row)))
       (list avail-row avail-column)))
	(promote "L")))

(defun get-move-knight (row column)
  (cond ((and (> row 2) (< column 9) (> column 1))
	 (list (list (+ row 2) (+ row 2)) (list (+ column 1) (- column 1))))
	((and (> row 2) (= column 9))
	 (list (list (+ row 2)) (list (- column 1))))
	((and (> row 2) (= column 1))
	 (list (list (+ row 2)) (list (+ column 1))))
	(promote "N")))

(defun get-move-silver (row column) ;marche pas car enleve separement -> changer mode de renvoi list doublon row column&
  (let* ((avail-move (list (list (+ row 1) (+ row 1) (+ row 1) (- row 1) (- row 1))
			 (list (+ column 1) column (- column 1) (+ column 1) (- column 1)))))
    (list (remove 1 (remove 9 (first avail-move) :test #'<) :test #'>)
	  (remove 1 (remove 9 (second avail-move) :test #'<) :test #'>))))
    
	 
	
	
	
	
  
   
