(in-package :lspec)

(defstruct ordered-hash-container
  name-hash
  entries)

(defun make-ordered-hash ()
  (make-ordered-hash-container :name-hash (make-hash-table :test 'equal)))

(defun do-ordered-hash ()
  )

(defun hmmm... how does this work then?
       )
