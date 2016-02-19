(in-package :lspec)

(defun al-insert (db key value)
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
  (delete-if (lambda (c) (equal (car c) key)) db))

(defun al-lookup (db key)
  (cdr (find-if (lambda (c) (equal (car c) key)) db)))

(defmacro al-each ((db key-var val-var) &body body)
  `(dolist (c ,db)
     (let ((,key-var (car c))
	   (,val-var (cdr c)))
       ,@body)))

(defmacro al-each-value ((db val-var) &body body)
  (let ((key-var (gensym)))
    `(al-each (,db ,key-var ,val-var)
       (declare (ignore ,key-var))
       ,@body)))
			 

(defun repeat-string (count str)
  (format nil "~V@{~a~:*~}" count str))

(defun joins (input-list &optional join-string)
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
  (/ (float (get-internal-real-time))
     internal-time-units-per-second))
	    
