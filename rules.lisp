;;;; File containing all the functions for implementing the rules of shogi

(defun init-empty-board ()
  "Create an empty board indexed by letter and number"
  (list
   :a (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :b (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :c (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :d (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :e (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :f (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :g (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :h (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :i (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")))

(defun select-square (board letter number)
  "Return the piece on the square"
  (getf (getf board letter) number))

(defun move (board letter number)
  (move-piece (board (select-square board letter number))))

(defun move-piece (board piece)
  (cond ((equal piece "P") (move-pawn))
	
  
  
   
