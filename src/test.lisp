(in-package :lspec)

(specify "A group of tests"

  (it "is okay"
      (let ((zero 0))
	(expect zero :to-be-zero)))

  (it "will fail"
      (let ((not-zero 1))
	(expect not-zero :to-be-zero)))


  (it "should find things"
      (let ((true t))
	(expect true :to-be-true))))
