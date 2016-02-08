
(asdf:defsystem #:lspec
  :description "spec type thing for lisp"
  :author "Me"
  :license "MIT"
  :serial t
  :components ((:module "src"
			:components ((:file "package")
				     (:module "formatters"
					      :components ((:file "base")
							   (:file "tree")))
				     (:file "expectations")
				     (:file "lisp")
				     (:file "main")))))
