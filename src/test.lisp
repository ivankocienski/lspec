(in-package :lspec)

(specify "A group of tests"

  (before
   ;; do some set up here ...
   )

  (after-each
   ;; do some cleanup
   )

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
   (format t "around 0 tear down~%")
   )
  
  (around-each

   ;; set up
   (format t "around 1 set up~%")

   (yield)

   ;; tear down
   (format t "around 1 tear down~%")
   )

  (context "this is a sub context"
	   
	   (it "should run before blocks!"
	       (format t "  in spec~%"))))

