;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defsystem "tsumeli"
  :name "TsumeLi"
  :version "0.1"
  :author "Loic Medina"
  :description "Solving shogi tsume"
  :long-description "Lisp implementation of shogi tsume solving"
  :depends-on (:xlsx)
  :components ((:file "rules")
	       (:file "interactions")
               (:file "tsume-1"
                :depends-on ("rules" "interactions" "read-excel"))
               (:file "rules-test"
		:depends-on ("rules"))
	       (:file "tsume-n"
                :depends-on ("rules" "interactions" "read-excel"))
	       (:file "storage")
	       (:file "read-excel")))
