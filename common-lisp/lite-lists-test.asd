

(asdf:defsystem #:lite-lists-test
  :name "lite lists test"
  :version "0.1"
  :author "sqfmklt"
  :license "Modified BSD"
  :description "Test for lite lists"
  :long-description "A short test and demo of the lite-lists syntax extension"
  :serial t
  :components ((:file "lite-lists-test"))
  :depends-on (#:lite-lists)
)
