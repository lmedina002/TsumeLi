;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defsystem "tsumeli"
  :name "TsumeLi"
  :version "0.1"
  :author "Loic Medina"
  :description "Solving shogi tsume"
  :long-description "Lisp implementation of shogi tsume solving"
  :depends-on (:xlsx)
  :components ((:file "package")
	       (:file "rules")
	       (:file "interactions")
               (:file "tsume-1"
                :depends-on ("rules" "interactions" "read-excel"))
               (:file "rules-test"
		:depends-on ("rules"))
	       (:file "tsume-n"
                :depends-on ("rules" "interactions"))
	       (:file "storage")
	       (:file "read-excel")
	       (:file "main"
		:depends-on ("read-excel")))
  :build-operation "program-op"
  :build-pathname "tsumeli-binary"
  :entry-point "tsumeli::main")
