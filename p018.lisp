(defun read-arr (n)
  (let ((res (make-array (list n n) :element-type 'fixnum)))
    (loop for i from 0 below n do
      (loop for j from 0 to i do
        (setf (aref res i j) (read))))
    res))

(defun solve (n a)
  (let ((dp (make-array (list n n) :element-type 'fixnum)))
    (setf (aref dp 0 0) (aref a 0 0))
    (loop for i from 1 below n do
      (setf (aref dp i 0)
            (+ (aref a i 0)
               (max (aref dp (1- i) 0))))
      (loop for j from 1 below i do
        (setf (aref dp i j)
              (+ (aref a i j)
                 (max (aref dp (1- i) j)
                      (aref dp (1- i) (1- j))))))
      (setf (aref dp i i) (+ (aref a i i) (aref dp (1- i) (1- i))))
      (setf (aref dp i (1- i))
            (+ (aref a i (1- i))
               (max (aref dp (1- i) (1- i))
                    (if (> i 1) (aref dp (1- i) (- i 2)) 0)))))
    (loop for i from 0 below n maximize (aref dp (1- n) i))))

(loop repeat (read) do
  (format t "~A~%" (let ((n (read)))
                     (solve n (read-arr n)))))
