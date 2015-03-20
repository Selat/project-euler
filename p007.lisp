(defconstant &maxn& 10500)
(defvar *primes* (make-array &maxn& :element-type 'fixnum))

(defun fill-primes (n)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum n)
           (type simple-array *primes*))
  (let ((used (make-array (1+ (ceiling n 2)) :element-type 'bit :initial-element 0))
        (cur-prime-id 0))
    (setf (aref *primes* 0) 2)
    (loop for i from 1 below (ceiling n 2)
          for num = (1+ (* 2 i))
          do (when (= (aref used i) 0)
               (setf (aref *primes* (incf cur-prime-id)) num)
               (loop for j from num to n by num
                     for id = (floor j 2)
                     do (when (= (mod j 2) 1) (setf (aref used id) 1)))))))

(fill-primes (* &maxn& 10))

(defun solve (n)
  (aref *primes* (1- n)))

(loop repeat (read) do
  (format t "~A~%" (solve (read))))
