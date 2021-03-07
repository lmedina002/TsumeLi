;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defsystem "tsumeli"
  :name "TsumeLi"
  :version "0.1"
  :author "Loïc Medina"
  :description "Solving shogi tsume"
  :long-description "Lisp implementation of shogi tsume solving"
  :components ((:file "rules")
	       (:file "interactions")
               (:file "tsume-1"
                      :depends-on ("rules"))
               (:file "rules-test"
                      :depends-on ("rules"))))
