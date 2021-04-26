(in-package :tsumeli)

(defparameter *excel-file-name* "tsume-configuration.xlsx")

(defun read-config ()
  (let* ((raw (xlsx:as-plist (xlsx:read-sheet *excel-file-name*)))
	 (board (read-board raw))
	 (enemy-hand (deduce-hand board))
	 (ally-hand (read-hand raw "15" "A"))
	 (evaluation-function (read-evaluation-function raw))
	 (algorithm (read-algorithm raw))
	 (turns (read-turns raw))
	 (depth (read-depth raw)))
    (return-from read-config (list :full-board (list :board board :drops-ally ally-hand :drops-enemy enemy-hand)
				   :evaluation-function evaluation-function
				   :algorithm algorithm
				   :turns turns
				   :depth depth))))
    

(defun read-hand (raw-plist row-string first-col)
  (let ((hand '()))
    (loop
      for (cell value) on raw-plist by #'cddr ;same as (cdr (cdr list)), allow to take two element each time
      do
	 (when (and (string-equal (subseq (string cell) 1) row-string) (string-not-equal (subseq (string cell) 0 1) first-col))
	   (push value hand)))
    (return-from read-hand hand)))

(defun deduce-hand (board)
  (let ((hand (list "-P" "-P" "-P" "-P" "-P" "-P" "-P" "-P" "-P" "-L" "-L" "-N" "-N" "-S" "-S" "-G" "-G" "-R" "-B"))) ;Full-hand
    (dotimes (row 9 hand)
      (dotimes (column 9 hand)
	(let ((square (aref board row column)))
	  (when (and (string-not-equal (subseq square 0 1) "-")
		     (string-not-equal square "_"))
	      (setf hand (remove (concatenate 'string "-" (subseq square (- (length square) 1))) hand :test #'equal :count 1))))))))

(defun read-board (raw-plist)
  (let ((board (make-array '(9 9)
			   :initial-contents
			   ;;  0   1   2   3   4   5   6   7   8
			   '(("_" "_" "_" "_" "_" "_" "_" "_" "_") ;0
			     ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;1
			     ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;2
			     ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;3
			     ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;4
			     ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;5
			     ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;6
			     ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;7
			     ("_" "_" "_" "_" "_" "_" "_" "_" "_"))))) ;8
    (loop
      for (cell value) on raw-plist by #'cddr ;same as (cdr (cdr list)), allow to take two element each time
      do
	 (progn
	   (let* ((cell-s (string cell))
		  (col (- (parse-integer cell-s :start 1) 5)))
	     (when (and (>= col 0) (<= col 8)) 
	       (cond ((string-equal (subseq cell-s 0 1) "B")
		      (setf (aref board col 0) value))
		     ((string-equal (subseq cell-s 0 1) "C")
		      (setf (aref board col 1) value))
		     ((string-equal (subseq cell-s 0 1) "D")
		      (setf (aref board col 2) value))
		     ((string-equal (subseq cell-s 0 1) "E")
		      (setf (aref board col 3) value))
		     ((string-equal (subseq cell-s 0 1) "F")
		      (setf (aref board col 4) value))
		     ((string-equal (subseq cell-s 0 1) "G")
		      (setf (aref board col 5) value))
		     ((string-equal (subseq cell-s 0 1) "H")
		      (setf (aref board col 6) value))
		     ((string-equal (subseq cell-s 0 1) "I")
		      (setf (aref board col 7) value))
		     ((string-equal (subseq cell-s 0 1) "J")
		      (setf (aref board col 8) value)))))))
    (return-from read-board board)))

(defun read-turns (raw-plist)
  (getf raw-plist :B17))

(defun read-depth (raw-plist)
  (getf raw-plist :B24))

(defun read-evaluation-function (raw-plist)
  (let ((choice (getf raw-plist :A26)))
    (cond ((= choice 1)
	   (return-from read-evaluation-function #'evaluation-enemy-full))
	  ((= choice 2)
	   (return-from read-evaluation-function #'raw-material-balance))
	  ((= choice 3)
	   (return-from read-evaluation-function #'ponderate-material-balance))
	  (t (return-from read-evaluation-function #'evaluation-enemy-full)))))

(defun read-algorithm (raw-plist)
  (let ((choice (getf raw-plist :B26)))
    (cond ((= choice 1)
	   (return-from read-algorithm #'minimax))
	  ((= choice 2)
	   (return-from read-algorithm #'alphabeta))
	  (t (return-from read-algorithm #'alphabeta)))))
	   
	   
