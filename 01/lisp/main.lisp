(defvar *passwords* nil)

(defun load-pass (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    ( when in
	   (loop for line = (read-line in nil)
		 while line do 
		 	(setq *passwords*
			      (push line *passwords*)))
	   (close in))))

(defun parse-pass()
  (setq *passwords*
	(mapcar #'(lambda (x) (parse-integer x)) *passwords*)))

(defun sort-pass ()
  (setq *passwords* (sort *passwords* #'(lambda (x y) ( < x y)))))


;; filter elements that when added with the two
;; smallest elements is greater than 2020

(defun remove-too-large ()
  (let ((minval (+ (car *passwords*) (car (cdr *passwords*)))))
  (setq *passwords*
	(remove-if-not 
	  #'(lambda (x) ( <= (+ x minval) 2020 )) *passwords*))))


(defun validate (x y z)
  (and (/= x y) (/= x z) (/= z y) (= (+ x (+ y z)) 2020)))

(defun find-solution ()
  (loop for x in *passwords*
	do (loop for y in *passwords*
		 do (loop for z in *passwords*
			  do (if (validate x y z)
			       (return-from find-solution (list x y z))
			       nil)))))


(defun run()
  (load-pass "../input")
  (parse-pass)
  (sort-pass)
  (remove-too-large)
  (format t "~a~%" (reduce #'* (find-solution))))
