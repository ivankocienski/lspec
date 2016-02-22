(in-package :cl-user)

(defpackage :lspec-example
  (:use :cl :lspec))

(in-package :lspec-example)

(specify "A group of tests"

  (around-each
   ;; do some set up
   (yield)
   ;; cleanup
   )
  
  (context "a way of nesting and segmenting tests"
	   
	   (it "is okay"
	       (let ((zero 0))
		 (expect zero :to-be-zero)))

	   (it "will fail"
	       (let ((not-zero 1))
		 (expect not-zero :to-be-zero)))


	   (it "should find things"
	       (let ((true t))
		 (expect true :to-be-true)))))




(specify "tests with callback blocks"

  (around-each

   ;; set up
   (format t "around 0 set up~%")

   (yield)

   ;; tear down
   (format t "around 0 tear down~%"))
  
  (around-each

   ;; set up
   (format t "around 1 set up~%")

   (yield)

   ;; tear down
   (format t "around 1 tear down~%"))

  (it "should only run above blocks~%"
      (format t "  in C1 spec~%"))

  (it "should be pending")

  (it "is also pending"
      (pending))

  (it "is pending with a nice message"
      (pending "FIXME"))
  
  (context "this is a sub context"
	   (around-each

	    ;; set up
	    (format t "around 2 set up~%")

	    (yield)

	    ;; tear down
	    (format t "around 2 tear down~%")
	    )

	   (it "should run before blocks!"
	       (format t "  in spec~%")))

  (it "should only be here"
      (format t "  in C1 spec~%")))

