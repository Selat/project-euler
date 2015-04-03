(defconstant &maxn& 100000)
(defconstant &maxprime& 1000)
(defvar *primes* (make-array &maxprime& :element-type 'fixnum))
(defvar *amicable-numbers* nil)

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


(defun sum-divisors (n)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) *primes*)
           (type fixnum n))
  (let ((res 1)
        (curn n))
    (declare (type integer res))
    (- (* (loop named outer for p across *primes*
                when (> (* p p) n) return res
                  do (when (zerop (mod n p))
                       (let ((num 0)
                             (tmp p))
                         (loop while (zerop (mod curn tmp))
                               do (incf num)
                                  (setf tmp (* tmp p)))
                         (setf curn (/ curn (/ tmp p)))
                         (setf res (* res (/ (1- tmp) (1- p))))
                         (if (= curn 1) (return-from outer res)))))
          (if (> curn 1) (1+ curn) 1))
       n)))

(defun prefill-answers (n)
  (let ((ans 0))
    (loop for i from 2 to n
          for sum = (sum-divisors i) do
            (when (and (= (sum-divisors sum) i) (not (= i sum)))
              (incf ans i)
              (push (cons i ans) *amicable-numbers*)))))

(defun solve (n)
  (loop for num in *amicable-numbers*
        when (<= (first num) n) return (rest num)
          finally (return 0)))

(fill-primes &maxprime&)
(prefill-answers &maxn&)

(loop repeat (read) do
  (format t "~A~%" (solve (read))))
