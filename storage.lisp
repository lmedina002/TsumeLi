;;;;Fichier non fonctionnel, sert uniquement à stocker des configurations de tsume

(list
	  :board (make-array '(9 9)
		    :initial-contents ;Tsume 232, soluce drop (S 1 3)
		      ; 0   1   2   3   4   5   6   7   8
		    '(("_" "_" "_" "_" "-J" "_" "_" "_" "_") ;0
		      ("_" "_" "_" "_" "_" "-G" "S" "_" "_") ;1
		      ("_" "_" "_" "G" "_" "_" "_" "_" "_") ;2
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;3
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;4
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;5
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;6
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_") ;7
		      ("_" "_" "_" "_" "_" "_" "_" "_" "_")));8
	  :drops-ally
	  (list "S")
	  :drops-enemy
	  (list "-P" "-L" "-N" "-S" "-G" "-B" "-R"))
