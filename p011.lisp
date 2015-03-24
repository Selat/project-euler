(defun find-max (a b n m func)
  (let ((res 0))
    (loop for i from b to n
          do (loop for j from 0 to m
                   do (setf res (max res (funcall func a i j)))))
    res))

(defmacro gen-sum-func (k i j)
  `(lambda (a i j)
     ,(append '(*) (loop for l from 0 below k
                         collect `(aref a ,(list '+ 'i (* l i))
                                        ,(list '+ 'j (* l j)))))))

(defun read-arr (n)
  (let ((a (make-array (list n n) :element-type 'fixnum)))
    (loop for i from 0 below n
          do (loop for j from 0 below n
                   do (setf (aref a i j) (read))))
    a))

(defun solve (n k)
  (let ((a (read-arr n)))
    (format t "~A~%"
            (max (find-max a 0 (- n k) (1- n)
                           (gen-sum-func 4 1 0))
                 (find-max a 0 (1- n) (- n k)
                           (gen-sum-func 4 0 1))
                 (find-max a 0 (- n k) (- n k)
                           (gen-sum-func 4 1 1))
                 (find-max a 3 (1- n) (- n k)
                           (gen-sum-func 4 -1 1))))))

(solve 20 4)
