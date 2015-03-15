(defun matrix-multiply (a b)
  (flet ((col (mat i) (mapcar #'(lambda (row) (elt row i)) mat))
         (row (mat i) (elt mat i)))
    (loop for row from 0 below (length a)
          collect (loop for col from 0 below (length (row b 0))
                        collect (apply #'+ (mapcar #'* (row a row) (col b col)))))))

(defun get-table (n)
  (let ((cur '((2 0)))
        (mat '((4 1) (1 0)))
        (res nil))
    (setf res (loop repeat n
                    collect (caar cur)
                    do      (setf cur (matrix-multiply mat cur))))
    (make-array (length res) :initial-contents res)))

(defvar *nums* (get-table 27))

(defun solve (n)
  (loop for x across *nums* sum
        (if (<= x n) x 0)))

(loop repeat (read) do
      (format t "~a~%" (solve (read))))
