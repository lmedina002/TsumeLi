;;;; File containing the functions for handling interactions with users

(defun format-game (board)
  "Print an user-friendly version of the game situation: board + drops"
  (format t "~%Enemy hand:~%")
  (dolist (item (getf board :drops-enemy))
    (format t " ~A " item))
  (format t "~%")
  (format-board (getf board :board))
  (format t "~%Ally hand:~%")
  (dolist (item (getf board :drops-ally))
    (format t " ~A " item))
  (format t "~%"))

(defun format-board (board)
  "Print an user-friendly version of the board"
  (format t "~%  0    1    2    3    4    5    6    7    8")
  (format t "~%---------------------------------------------~%")
  (dotimes (row 9)    
    (dotimes (column 9)
      (cond ((equal (aref board row column) "_")
	     (format t "|   |"))
	    ((equal (length (aref board row column)) 1)
	     (format t "| ~A |" (aref board row column)))
	    ((equal (length (aref board row column)) 2)
	     (format t "|~A |" (aref board row column)))
	    (t (format t "|~A|" (aref board row column)))))
    (format t "~A~%---------------------------------------------~%" row)))
  
  
      
