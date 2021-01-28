;;;; File containing all the functions for implementing the rules of shogi

(defun initEmptyBoard ()
  (list
   :a (list :1 "" :2 "" :3 "" :4 "a" :5 "" :6 "" :7 "" :8 "" :9 "")
   :b (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :c (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :d (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :e (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :f (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :g (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :h (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")
   :i (list :1 "" :2 "" :3 "" :4 "" :5 "" :6 "" :7 "" :8 "" :9 "")))

(defun selectCase (board letter number)
  (getf (getf board letter) number))

(defun move (board letter number)
  (movePiece (board (selectCase board letter number))))

(defun movePiece (board piece)
  (cond ((equal piece "P") (movePawn))
	
  
  
   
