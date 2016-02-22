(in-package :lspec)

(defun al-insert (db key value)
  "inserts or updates value in a-list db"
  (labels ((scan-insert (lst)
	   (if lst
	       (let* ((scan-cons (car lst))
		      (scan-key  (car scan-cons)))

		 (if (equal key scan-key)
		     (acons key value (cdr lst))
		     (cons scan-cons (scan-insert (cdr lst)))))

	       (list (cons key value)))))
    
    (scan-insert db)))

(defun al-delete (db key)
  "removes  (or NOP) a-list key"
  (delete-if (lambda (c) (equal (car c) key)) db))

(defun al-lookup (db key)
  "Looks up key in a-list"
  (cdr (find-if (lambda (c) (equal (car c) key)) db)))

(defmacro al-each ((db key-var val-var) &body body)
  "iterates over a-list yielding key and value"
  `(dolist (c ,db)
     (let ((,key-var (car c))
	   (,val-var (cdr c)))
       ,@body)))

(defmacro al-each-value ((db val-var) &body body)
  "iterates over a-list with only value"
  (let ((key-var (gensym)))
    `(al-each (,db ,key-var ,val-var)
       (declare (ignore ,key-var))
       ,@body)))
			 

(defun repeat-string (count str)
  "repeat a string N times"
  (format nil "~V@{~a~:*~}" count str))

(defun empty? (str)
  "tests if something is empty or nil"
  (or (null str) (= (length str) 0)))

(defun joins (input-list &optional join-string)
  "join a list of values as a string with optional
   join string interspersed inbetween"
  (with-output-to-string (stream)
    (labels ((print-bits (bits)
	       (let ((bit (car bits)) (rest (cdr bits)))
		 (format stream bit)
		 (if rest
		     (progn
		       (if join-string
			   (format stream join-string))
		       (print-bits rest))))))
      
      (print-bits input-list))))


(defun chomp-by-char (string char)
  (let ((found-at (position char string)))

    (values (subseq string 0 found-at)
	    found-at)))

(defun split-by-char (string char)
	 (multiple-value-bind (sub-string len) (chomp-by-char string char)
	   (if len
	       (cons sub-string
		     (split-by-char (subseq string (1+ len)) char))
	       (list sub-string))))


(defun get-real-time ()
  "returns current time as a fractional number of seconds"
  (/ (float (get-internal-real-time))
     internal-time-units-per-second))
	    


(defmacro package-macrolet (forms &body body)
  "like macrolet but sub-macro symbols are internedd in the
   current *package*."
  (let ((transformed (mapcar (lambda (macro-dec) 
				 (list (intern (string (first macro-dec)) *package*)
				       (second macro-dec)
				       (third  macro-dec)));)
			     forms)))
    `(macrolet (,@transformed) ,@body)))


