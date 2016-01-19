(in-package :lspec)

(defun al-insert (db key value)
  (labels ((scan-insert (lst)
	   (if lst
	       (let* ((scan-cons (car lst))
		      (scan-key  (car scan-cons)))

		 (if (eq key scan-key)
		     (acons key value (cdr lst))
		     (cons scan-cons (scan-insert (cdr lst)))))

	       (list (cons key value)))))
    
    (scan-insert db)))

(defun al-delete (db key)
  (delete-if (lambda (c) (eq (car c) key)) db))

(defun al-lookup (db key)
  (cdr (find-if (lambda (c) (eq (car c) key)) db)))

(defmacro al-each ((db key-var val-var) &body body)
  `(dolist (c ,db)
     (let ((,key-var (car c))
	   (,val-var (cdr c)))
       ,@body)))
