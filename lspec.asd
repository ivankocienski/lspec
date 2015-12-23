
(asdf:defsystem #:lspec
  :description "spec type thing for lisp"
  :author "Me"
  :license "MIT"
  :serial t
  :components ((:module "src"
			:components ((:file "package")
				     (:file "expectations")
				     (:file "main")))))
