(defconstant &maxn& 5000)
(defvar *primes* (make-array &maxn& :element-type 'fixnum))

(defun fill-primes (n)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum n)
           (type (simple-array fixnum) *primes*))
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

(defun factorize (n)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) *primes*)
           (type fixnum n))
  (if (= n 1) 1
      (let ((res 1)
            (curn n))
        (declare (type integer res))
        (loop named outer for p across *primes*
              when (> (* p p) n) return (* res 2)
                do (when (zerop (mod n p))
                     (let ((num 0)
                           (tmp p))
                       (loop while (zerop (mod curn tmp))
                             do (incf num)
                                (setf tmp (* tmp p)))
                       (setf curn (/ curn (/ tmp p)))
                       (setf res (* res (1+ num)))
                       (if (= curn 1) (return-from outer res))))))))

(defun solve (n)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum n))
  (let ((cnt 1)
        (i 2)
        (a 1)
        (b 2))
    (loop while (<= cnt n) do
      (if (evenp i)
          (setf cnt (* a (setf b (factorize (1+ i)))))
          (setf cnt (* (setf a (factorize (/ (1+ i) 2))) b)))
      (incf i))
    (/ (* i (1- i)) 2)))

(fill-primes (* &maxn& 7))

(loop repeat (read) do
      (format t "~A~%" (solve (read))))
